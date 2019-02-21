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

(defn add-fields
  [card]
  (assoc card
         :id (slugify (:title card))
         :faction (if (= :neutral (:faction card))
                    (if (= :corp (:side card))
                      :neutral-corp
                      :neutral-runner)
                    (:faction card))
         :deck-limit (when-not (= :identity (:type card))
                       (or (:deck-limit card)
                           3))))

(defn build-cards
  [path]
  (->> (sc/slurp-csv path
                     :parser-opts {:delimiter \;}
                     :cast-fns {:advancement-requirement sc/->int
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
                                :text (comp string/trim #(string/replace % "\\n" "\n"))
                                :title string/trim
                                :trash-cost sc/->int
                                :type key-slug
                                :uniqueness (fn [v] (= "TRUE" v))})
       (map add-fields)
       (map prune-null-fields)))

(defn build-set-cards
  [cards code-num set-name]
  (->> (for [card cards]
         {:card-id (:id card)
          :code (str (+ code-num (:position card)))
          :position (:position card)
          :quantity (or (:quantity card) 3)
          :set-id (slugify
                    (str set-name
                         (if (>= 65 (:position card))
                           "Downfall"
                           "Uprising")))})
       (sort-by :code)
       (group-by :set-id)))

(defn build-from-csv
  []
  (let [
        code-num 26000
        set-name "Ashes: "
        _ (println "Building cards")
        cards (build-cards "ashes.csv")
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
      (spit (str "edn/cards/ashes-" (:id card) ".edn")
            (str (zp/zprint-str card) "\n")))))
