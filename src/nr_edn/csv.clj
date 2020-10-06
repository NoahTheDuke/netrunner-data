(ns nr-edn.csv
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [semantic-csv.core :as sc]
            [zprint.core :as zp]
            [nr-edn.download :refer [convert-subtypes]]
            [nr-edn.utils :refer [slugify prune-null-fields]]))

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
      card-title
      (assoc :influence-cost (:influence-value card)
             :faction (if (= :neutral (:faction card))
                        (if (= :corp (:side card))
                          :neutral-corp
                          :neutral-runner)
                        (:faction card))
             :deck-limit (when-not (= :identity (:type card))
                           (or (:deck-limit card)
                               3)))
      (dissoc :influence-value :subtitle)))

(defn string-trim
  [s]
  ((fnil string/trim "") s))

(defn string-replace
  [s]
  ((fnil string/replace "") s "\\n" "\n"))

(defn clean-text
  [s]
  (->> s
       string-replace
       string-trim
       string/split-lines
       (map string-trim)
       (remove empty?)
       (string/join "\n")
       (#(string/split % #" "))
       (remove empty?)
       (string/join " ")
       )
  )

(defn build-cards
  [path]
  (->> (sc/slurp-csv path
                     :parser-opts {:delimiter \;}
                     :cast-fns {
                                :advancement-requirement sc/->int
                                :agenda-points sc/->int
                                :base-link sc/->int
                                :cost edn/read-string
                                :deck-limit sc/->int
                                :faction key-slug
                                :influence-limit sc/->int
                                :influence-value sc/->int
                                :memory-cost sc/->int
                                :minimum-deck-size sc/->int
                                :position sc/->int
                                :quantity sc/->int
                                :side key-slug
                                :strength edn/read-string
                                :subtype convert-subtypes
                                :subtitle string-trim
                                :text clean-text
                                :title string-trim
                                :trash-cost sc/->int
                                :type key-slug
                                :uniqueness (fn [v] (= "TRUE" v))
                                })
       (map add-fields)
       (map prune-null-fields)))

(defn build-set-cards
  [cards code-num set-name]
  (->> (for [card cards]
         {:card-id (:id card)
          :position (:position card)
          :quantity (or (:quantity card) 3)
          :set-id (slugify set-name)})
       (sort-by :position)
       (map-indexed (fn [idx itm] (assoc itm :position (inc idx) :code (str (+ code-num (inc idx))))))
       (sort-by :code)
       (group-by :set-id)))

(defn build-from-csv
  []
  (let [
        code-num 29000
        set-name "System Gateway"
        _ (println "Building cards")
        cards (build-cards "system-gateway.csv")
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
            :when (:quantity card)]
      (spit (str "edn/cards/system-gateway-" (:id card) ".edn")
            (str (zp/zprint-str card) "\n")))
    (println "Done!")))
