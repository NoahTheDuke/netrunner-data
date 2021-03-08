(ns nr-edn.csv
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [semantic-csv.core :as sc]
            [zprint.core :as zp]
            [nr-edn.combine :refer [load-data load-edn-from-dir]]
            [nr-edn.download :refer [convert-subtypes]]
            [nr-edn.utils :refer [cards->map slugify prune-null-fields]]))

(defn key-slug
  [v]
  (keyword (slugify v)))

(defn card-title
  [card]
  (let [title (:title card)
        subtitle (:subtitle card)
        full-title (if (not-empty subtitle)
                     (str title ": " subtitle)
                     title)]
    (assoc card
           :title full-title
           :id (slugify full-title))))

(defn add-fields
  [card]
  (-> card
      (card-title)
      (assoc :advancement-requirement (when (= :agenda (:type card))
                                        (or (:advancement-requirement card) (:cost card)))
             :deck-limit (when-not (= :identity (:type card))
                           (or (:deck-limit card)
                               3))
             :faction (if (= :neutral (:faction card))
                        (if (= :corp (:side card))
                          :neutral-corp
                          :neutral-runner)
                        (:faction card))
             :influence-cost (when-not (= :identity (:type card))
                               (or (:influence-value card) 0))
             :flavor (->> [(:flavor card)
                           (when (:attribution card)
                             (str "<champion>" (:attribution card) "</champion>"))]
                          (filter identity)
                          (string/join "\n")
                          (not-empty)))
      (dissoc :attribution
              (when (= :agenda (:type card)) :cost)
              (when (and (= :agenda (:type card)) (not= :neutral (:faction card))) :influence-cost)
              :influence-value
              :subtitle)))

(defn string-trim
  [s]
  ((fnil string/trim "") s))

(defn string-replace
  [s]
  (-> (or (not-empty s) "")
      (string/replace "\\n" "\n")
      (string/replace "{b}" "<strong>")
      (string/replace "{/b}" "</strong>")
      (string/replace "{i}" "<em>")
      (string/replace "{/i}" "</em>")
      (string/replace "{ra}" "")
      (string/replace "{/ra}" "")
      (string/replace "{c}" "[credit]")
      (string/replace "{click}" "[click]")
      (string/replace "{MU}" "[mu]")
      (string/replace "{trash}" "[trash]")
      (string/replace "{sub}" "[subroutine]")))

(defn clean-text
  [s]
  (->> s
       (string-replace)
       (string-trim)
       (string/split-lines)
       (map string-trim)
       (remove empty?)
       (string/join "\n")
       (#(string/split % #" "))
       (remove empty?)
       (string/join " ")
       (not-empty)))

(defn process-adv-req
  [s]
  (try (sc/->int s)
       (catch Exception e
         0)))

(defn process-inf-limit
  [s]
  (try (sc/->int s)
       (catch Exception e
         0)))

(defn make-types-fn
  [types]
  (fn [row-val]
    (when (not-empty row-val)
      (let [row-val (key-slug row-val)
            card-type (:code (get types row-val))]
        (or card-type
            (println (str row-val " contains a malformed subtype")))))))

(defn make-subtypes-fn
  [subtypes]
  (fn [row-val]
    (when (not-empty row-val)
      (->> (convert-subtypes row-val)
           (filter #(or (get subtypes %)
                        (println (str row-val " contains a malformed subtype"))))
           (into [])
           not-empty))))

(defn make-cast-fns
  [types-fn subtypes-fn]
  {:advancement-requirement process-adv-req
   :agenda-points sc/->int
   :attribution clean-text
   :base-link sc/->int
   :cost edn/read-string
   :deck-limit sc/->int
   :faction key-slug
   :influence-limit process-inf-limit
   :influence-value sc/->int
   :memory-cost sc/->int
   :minimum-deck-size sc/->int
   :position sc/->int
   :quantity sc/->int
   :side key-slug
   :strength edn/read-string
   :subtype subtypes-fn
   :subtitle string-trim
   :text clean-text
   :title string-trim
   :trash-cost sc/->int
   :type types-fn
   :uniqueness (fn [row-val] (= "TRUE" row-val))})

(defn manually-cast-columns
  [cast-fns]
  (fn [row]
    (reduce-kv
      (fn [m row-key row-value]
        (if-let [cast-fn (get cast-fns row-key)]
          (assoc m row-key (cast-fn row-value))
          (assoc m row-key row-value)))
      {}
      row)))

(defn build-cards
  [path cast-columns]
  (->> (sc/slurp-csv path)
       (map cast-columns)
       (map add-fields)
       (map prune-null-fields)))

(defn build-set-cards
  [cards code-num set-name]
  (->> (for [card cards]
         {:card-id (:id card)
          :position (:position card)
          :quantity (if (= :identity (:type card)) 1 3)
          :set-id (slugify set-name)
          :flavor (clean-text (:flavor card))
          :illustrator (clean-text (:artist card))})
       (map prune-null-fields)
       (sort-by :position)
       (map-indexed (fn [idx itm] (assoc itm :position (inc idx) :code (str (+ code-num (inc idx))))))
       (sort-by :code)
       (group-by :set-id)))

; (comment
;   (let [cards (->> (sc/slurp-csv "borealis.csv")
;                    (map card-title)
;                    (map #(assoc % :position (sc/->int (:card-number %)))))
;         set-cards (build-set-cards cards 32000 "Borealis")
;         existing-cards (cards->map :id (load-edn-from-dir "edn/cards"))]

;     (when-let [card (first (remove #(get existing-cards (:card-id %)) (first (vals set-cards))))]
;       (throw (Exception. (:card-id card))))

;     (zp/set-options!
;       {:style :community
;        :map {:comma? false
;              :force-nl? true}
;        :width 1000})

;     (println "Writing set-cards")
;     (doseq [[title s] set-cards]
;       (spit (str "edn/set-cards/" title ".edn")
;             (str (zp/zprint-str (into [] s)) "\n")))
;     ))

(defn strip-set-fields
  [card]
  (dissoc card :artist :attribution :flavor :illustrator :quantity))

(defn build-from-csv
  []
  (let [
        _ (println "Loading existing data")
        code-num 30000
        set-name "System Gateway"

        types (load-data "types")
        types-fn (make-types-fn types)

        subtypes (load-data "subtypes")
        subtypes-fn (make-subtypes-fn subtypes)

        cast-fns (make-cast-fns types-fn subtypes-fn)
        cast-columns (manually-cast-columns cast-fns)

        _ (println "Building cards")
        cards (build-cards "gateway.csv" cast-columns)

        _ (println "Building set-cards")
        set-cards (build-set-cards cards code-num set-name)
        ]

    (zp/set-options!
      {:style :community
       :map {:comma? false
             :force-nl? true}
       :width 1000})

    (println "Writing set-cards")
    (doseq [[title s] set-cards]
      (spit (str "edn/set-cards/" title ".edn")
            (str (zp/zprint-str (into [] s)) "\n")))

    (println "Writing cards")
    (doseq [card (map #(dissoc % :position) cards)
            :let [card (strip-set-fields card)]]
      (spit (str "edn/cards/system-gateway-" (:id card) ".edn")
            (str (zp/zprint-str card) "\n")))
    (println "Done!")
    (System/exit 0)))
