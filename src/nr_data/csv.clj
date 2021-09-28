(ns nr-data.csv
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [cond-plus.core :refer [cond+]]
   [medley.core :refer [find-first]]
   [nr-data.data :refer [load-data]]
   [nr-data.download :refer [convert-subtypes]]
   [nr-data.text :refer [add-stripped-card-text]]
   [nr-data.utils :refer [prune-null-fields slugify]]
   [semantic-csv.core :as sc]
   [zprint.core :as zp]))

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

(defn convert-faction
  [card]
  (cond+
    [(= :neutral (:faction card))
     (if-let [side (:side card)]
       (if (= :corp side) :neutral-corp :neutral-runner)
       (case (:type card)
         (:agenda :asset :ice :operation :upgrade) :neutral-corp
         (:event :hardware :program :resource) :neutral-runner
         (:identity) (if (:base-link card) :neutral-runner :neutral-corp)
         ; else
         (throw (Exception. (str "what side am i??? " card)))))]
    [(= :neutral-c (:faction card)) :neutral-corp]
    [(= :neutral-r (:faction card)) :neutral-runner]
    [(= :hb (:faction card)) :haas-bioroid]
    [(= :weyland (:faction card)) :weyland-consortium]
    [:else (:faction card)]))

(defn set-deck-limit
  [card]
  (cond+
    [(= :identity (:type card)) 1]
    [(:deck-limit card)]
    [(re-find #"Limit 1 per deck" (or (:text card) "")) 1]
    [:else 3]))

(defn add-fields
  [card]
  (-> card
      (card-title)
      (assoc :advancement-requirement (when (= :agenda (:type card))
                                        (or (:advancement-requirement card) (:cost card)))
             :deck-limit (set-deck-limit card)
             :faction (convert-faction card)
             :influence-cost (when-not (= :identity (:type card))
                               (or (:influence-value card) (:influence-cost card) 0))
             :flavor (->> [(:flavor card)
                           (when (:attribution card)
                             (str "<champion>" (:attribution card) "</champion>"))]
                          (filter identity)
                          (str/join "\n")
                          (not-empty))
             :side (or (:side card)
                       (case (:type card)
                         (:agenda :asset :ice :operation :upgrade) :corp
                         (:event :hardware :program :resource) :runner
                         (:identity) (if (:base-link card) :runner :corp)
                         ; else
                         (throw (Exception. (str "what side am i??? " card))))))
      (dissoc (when (= :agenda (:type card)) :cost)
              (when (and (= :agenda (:type card)) (not= :neutral (:faction card))) :influence-cost))))


(defn string-trim
  [s]
  ((fnil str/trim "") s))

(defn string-replace
  [s]
  (-> (or (not-empty s) "")
      (str/replace "\\n" "\n")
      (str/replace "{b}" "<strong>")
      (str/replace "{/b}" "</strong>")
      (str/replace "{i}" "<em>")
      (str/replace "{/i}" "</em>")
      (str/replace "{ra}" "")
      (str/replace "{/ra}" "")
      (str/replace "{c}" "[credit]")
      (str/replace "{!}" "[interrupt]")
      (str/replace "{click}" "[click]")
      (str/replace "{recurring}" "[recurring-credit]")
      (str/replace "{MU}" "[mu]")
      (str/replace "{trash}" "[trash]")
      (str/replace "{sub}" "[subroutine]")
      (str/replace #"Trace\s*\[(\d+)\]" "<trace>Trace $1</trace>")
      (str/replace "’" "'")
      (str/replace "…" "...")))

(defn clean-text
  [s]
  (->> s
       (string-replace)
       (string-trim)
       (str/split-lines)
       (map string-trim)
       (remove empty?)
       (str/join "\n")
       (#(str/split % #" "))
       (remove empty?)
       (str/join " ")
       (not-empty)))

(defn process-adv-req
  [s]
  (try (sc/->int s)
       (catch Exception _
         0)))

(defn process-inf-limit
  [s]
  (try (sc/->int s)
       (catch Exception _
         0)))

(defn make-types-fn
  [types]
  (fn [raw-val]
    (when (not-empty raw-val)
      (let [row-val (key-slug raw-val)
            card-type (:code (get types row-val))]
        (or card-type
            (println (str raw-val " contains a malformed type")))))))

(defn make-subtypes-fn
  [subtypes]
  (fn [row-val]
    (when (not-empty row-val)
      (->> (convert-subtypes row-val)
           (filter #(or (get subtypes %)
                        (println (str row-val " contains a malformed subtype"))))
           (into [])
           not-empty))))

(defn process-cost
  [cost]
  (let [converted-cost (edn/read-string cost)]
    (if (symbol? converted-cost)
      (str converted-cost)
      converted-cost)))


(defn make-cast-fns
  [types-fn subtypes-fn]
  {:advancement-requirement process-adv-req
   :agenda-points sc/->int
   :attribution clean-text
   :base-link sc/->int
   :cost process-cost
   :deck-limit sc/->int
   :faction key-slug
   :influence-limit process-inf-limit
   :influence-cost sc/->int
   :influence-value sc/->int
   :memory-cost sc/->int
   :minimum-deck-size sc/->int
   :position sc/->int
   :quantity sc/->int
   :set-id slugify
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

(defn rename-keys-from-gold
  [row]
  (set/rename-keys row {
                        :artist :illustrator
                        :unique :uniqueness
                        :subtypes :subtype
                        :influence :influence-cost
                        :flavour-text :flavor
                        :card-text :text
                        :memory :memory-cost
                        :id-mu :memory
                        :id-link :base-link
                        :id-influence :influence-limit
                        :points :agenda-points
                        :deck-size :minimum-deck-size
                        }))

(defn build-cards
  [path cast-columns delimiter]
  (->> (sc/slurp-csv path :parser-opts {:delimiter delimiter})
       (map rename-keys-from-gold)
       (map cast-columns)
       (map add-fields)
       (map prune-null-fields)
       (add-stripped-card-text)))

(defn build-set-cards
  [cards code-num set-name]
  (->> (for [card cards]
         {:card-id (:id card)
          :position (:position card)
          :quantity (if (= :identity (:type card)) 1 3)
          :set-id (slugify (or (:set-id card) set-name))
          :flavor (clean-text (:flavor card))
          :illustrator (clean-text (:illustrator card))})
       (map prune-null-fields)
       (sort-by :position)
       (map-indexed (fn [idx itm] (assoc itm :position (inc idx) :code (str (+ code-num (inc idx))))))
       (sort-by :code)
       (group-by :set-id)))

(def allowed-keys
  [:minimum-deck-size :faction :base-link :influence-limit :memory-cost :type
   :stripped-text :title :influence-cost :uniqueness :agenda-points :strength :id
   :stripped-title :side :trash-cost :cost :advancement-requirement :deck-limit
   :subtype :text])

(defn select-allowed-keys
  [card]
  (select-keys card allowed-keys))

(defn load-cycles [set-name]
  (->> (io/file "edn" "cycles.edn")
       (slurp)
       (edn/read-string)
       (find-first #(= set-name (:id %)))))

(def sets-to-prepend
  #{
    "proxies"
    })

(defn build-from-csv-impl
  ([set-name] (build-from-csv-impl set-name \,))
  ([set-name delimiter]
   (let [set-name (or set-name "system-update-2021")
         _ (println "Loading existing data")
         cycle-data (load-cycles set-name)

         types (load-data "types")
         types-fn (make-types-fn types)

         subtypes (load-data "subtypes")
         subtypes-fn (make-subtypes-fn subtypes)

         cast-fns (make-cast-fns types-fn subtypes-fn)
         cast-columns (manually-cast-columns cast-fns)

         _ (println "Building cards")
         cards (build-cards (str set-name ".csv") cast-columns delimiter)

         position (* 1000 (if (= "proxies" set-name) 99 (:position cycle-data)))

         _ (println "Building set-cards")
         set-cards (build-set-cards cards position set-name)

         prepend-set-name (when-let [s (get sets-to-prepend set-name)] (str s "-"))]

     (println "Writing set-cards")
     (doseq [[title s] set-cards]
       (spit (str "edn/set-cards/" title ".edn")
             (str (zp/zprint-str (into [] s)) "\n")))

     (println "Writing cards")
     (doseq [card (map select-allowed-keys cards)]
       (spit (str "edn/cards/" prepend-set-name (:id card) ".edn")
             (str (zp/zprint-str card) "\n")))
     (println "Done!"))))

(defn build-from-csv [& _]
  (build-from-csv-impl nil)
  (System/exit 0))

(comment
  (build-from-csv-impl "system-gateway")
  (build-from-csv-impl "system-update-2021")
  (build-from-csv-impl "proxies")
  )
