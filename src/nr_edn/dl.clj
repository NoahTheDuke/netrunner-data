(ns nr-edn.dl
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :refer [rename-keys]]
            [org.httpkit.client :as http]))

(defmacro vals->vec
  [coll]
  `(into [] (vals ~coll)))

(defn read-edn-file
  [file-path]
  ((comp edn/read-string slurp) file-path))

(defn load-edn-from-dir
  [file-path]
  (->> (io/file file-path)
       file-seq
       (filter #(.isFile %))
       (map read-edn-file)
       flatten
       (into [])))

(defn cards->map
  ([cards] (cards->map :code cards))
  ([kw cards]
   (into {} (map (juxt kw identity) cards))))

(defn load-data
  ([filename] (load-data filename {:id :code}))
  ([filename kmap]
   (cards->map
    (for [m (read-edn-file (str "edn/" filename ".edn"))]
      (rename-keys m kmap)))))

(defn load-sets
  [cycles]
  (cards->map
    (for [s (read-edn-file "edn/sets.edn")
          :let [cy (get cycles (:cycle-id s))]]
      {:available (or (:date-release s) "4096-01-01")
       :bigbox (> (or (:size s) -1) 20)
       :code (:id s)
       :cycle (:name cy)
       :cycle_code (:cycle-id s)
       :cycle_position (:position cy)
       :ffg-id (:ffg-id s)
       :name (:name s)
       :position (:position s)
       :rotated (:rotated cy)
       :size (:size s)})))

(defn merge-sets-and-cards
  [set-cards raw-cards]
  (map #(merge % (get raw-cards (:card-id %))) set-cards))

(defn- prune-null-fields
  [card]
  (apply dissoc card (for [[k v] card :when (nil? v)] k)))

(defn load-cards
  [sides factions types subtypes sets]
  (let [set-cards (load-edn-from-dir "edn/set-cards")
        raw-cards (cards->map :id (load-edn-from-dir "edn/cards"))
        cards (merge-sets-and-cards set-cards raw-cards)]
    (->>
      (for [card cards
            :let [s (get sets (:set-id card))]]
        {:advancementcost (:advancement-requirement card)
         :agendapoints (:agenda-points card)
         :baselink (:base-link card)
         :code (:code card)
         :cost (or (:cost card) 0)
         :cycle_code (:cycle_code s)
         :faction (:name (get factions (:faction card)))
         :factioncost (:influence-value card)
         :image_url (:image-url card)
         :influencelimit (:influence-limit card)
         :limited (:deck-limit card)
         :memoryunits (:memory-cost card)
         :minimumdecksize (:minimum-deck-size card)
         :normalizedtitle (:id card)
         :number (:position card)
         :packquantity (:quantity card)
         :replaced_by (:replaced-by card)
         :replaces (:replaces card)
         :rotated (:rotated s)
         :set_code (:set-id card)
         :setname (:name s)
         :side (:name (get sides (:side card)))
         :strength (or (:strength card) 0)
         :subtype (when (seq (:subtype card))
                    (string/join " - " (map #(:name (get subtypes %)) (:subtype card))))
         :text (:text card)
         :title (:title card)
         :trash (:trash-cost card)
         :type (:name (get types (:type card)))
         :uniqueness (:uniqueness card)})
      (map prune-null-fields)
      cards->map)))
