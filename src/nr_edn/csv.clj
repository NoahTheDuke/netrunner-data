(ns nr-edn.csv
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [semantic-csv.core :as sc]
            [zprint.core :as zp]
            [nr-edn.combine :refer [prune-null-fields]]
            [nr-edn.download :refer [convert-subtypes]]
            [nr-edn.utils :refer [slugify]]))

(defn key-slug
  [v]
  (keyword (slugify v)))

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
       (map #(assoc % :id (slugify (:title %))))
       (map prune-null-fields)))

(defn build-set-cards
  [cards code-num set-name]
  (->> (for [card cards]
         {:card-id (:id card)
          :code (str (+ code-num (:position card)))
          :position (:position card)
          :quantity (:quantity card)
          :set-id (slugify
                    (str set-name
                         (if (>= 65 (:position card))
                           1 2)))})
       (sort-by :code)
       (group-by :set-id)))

(defn build-from-csv
  []
  (let [
        code-num 26000
        set-name "Set "
        cards (build-cards "cards.csv")
        set-cards (build-set-cards cards code-num set-name)
        ]

    (zp/set-options!
      {:style :community
       :map {:comma? false
             :force-nl? true}
       :width 1000})

    (doseq [[title s] set-cards]
      (spit (str "edn/set-cards/" title ".edn")
            (zp/zprint-str (into [] s))))

    (doseq [card (map #(dissoc % :position) cards)]
      (spit (str "edn/cards/" (:id card) ".edn")
            (zp/zprint-str card)))))
