(ns nr-edn.combine
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :refer [rename-keys]]
            [org.httpkit.client :as http]
            [nr-edn.utils :refer [cards->map vals->vec]]))

(defn read-edn-file
  [file-path]
  ((comp edn/read-string slurp) file-path))

(defn load-edn-from-dir
  [file-path]
  (->> (io/file file-path)
       file-seq
       (filter #(and (.isFile %)
                     (string/ends-with? % ".edn")))
       (map read-edn-file)
       flatten
       (into [])))

(defn load-data
  ([filename] (load-data filename {:id :code}))
  ([filename kmap]
   (cards->map
     (for [m (read-edn-file (str "edn/" filename ".edn"))]
       (rename-keys m kmap)))))

(defn load-sets
  [cycles]
  (cards->map :id
    (for [s (read-edn-file "edn/sets.edn")
          :let [cy (get cycles (:cycle-id s))]]
      {:available (or (:date-release s) "4096-01-01")
       :bigbox (> (or (:size s) -1) 20)
       :code (:code s)
       :cycle (:name cy)
       :cycle_code (:cycle-id s)
       :cycle_position (:position cy)
       :ffg-id (:ffg-id s)
       :id (:id s)
       :name (:name s)
       :position (:position s)
       :rotated (:rotated cy)
       :size (:size s)})))

(defn merge-sets-and-cards
  [set-cards raw-cards]
  (map #(merge % (get raw-cards (:card-id %))) set-cards))

(defn- get-cost
  [card]
  (or (:cost card)
      (case (:type card)
        (:asset :event :hardware :operation :program :resource :upgrade) 0
        nil)))

(defn- get-strength
  [card]
  (or (:strength card)
      (case (:type card)
        (:ice :program) 0
        nil)))

(defn- prune-null-fields
  [card]
  (apply dissoc card (for [[k v] card :when (nil? v)] k)))

(defn load-cards
  [sides factions types subtypes sets]
  (let [set-cards (load-edn-from-dir "edn/set-cards")
        raw-cards (cards->map :id (load-edn-from-dir "edn/cards"))
        cards (merge-sets-and-cards set-cards raw-cards)]
    (->> (for [card cards
               :let [s (get sets (:set-id card))]]
           {:advancementcost (:advancement-requirement card)
            :agendapoints (:agenda-points card)
            :baselink (:base-link card)
            :code (:code card)
            :cost (get-cost card)
            :cycle_code (:cycle_code s)
            :faction (:name (get factions (:faction card)))
            :factioncost (:influence-value card)
            :normalizedtitle (:id card)
            :image_url (:image-url card)
            :influencelimit (:influence-limit card)
            :limited (:deck-limit card)
            :memoryunits (:memory-cost card)
            :minimumdecksize (:minimum-deck-size card)
            :number (:position card)
            :packquantity (:quantity card)
            :replaced_by (:replaced-by card)
            :replaces (:replaces card)
            :rotated (:rotated s)
            :set_code (:code s)
            :setname (:name s)
            :side (:name (get sides (:side card)))
            :strength (get-strength card)
            :subtype (when (seq (:subtype card))
                       (string/join " - " (map #(:name (get subtypes %)) (:subtype card))))
            :text (:text card)
            :title (:title card)
            :trash (:trash-cost card)
            :type (:name (get types (:type card)))
            :uniqueness (:uniqueness card)})
         (map prune-null-fields)
         cards->map)))

(defn compile-for-jnet
  []
  (try
    (let [mwls (load-data "mwls" {:id :code
                                  :date-start :date_start})
          sides (load-data "sides")
          factions (load-data "factions")
          types (load-data "types")
          subtypes (load-data "subtypes")
          cycles (load-data "cycles")
          sets (load-sets cycles)
          cards (load-cards sides factions types subtypes sets)
          promos (read-edn-file "edn/promos.edn")]
      (spit (io/file "edn" "raw_data.edn")
            (sorted-map
              :mwls (vals->vec mwls)
              :cycles (vals->vec cycles)
              :sets (vals->vec sets)
              :cards (vals->vec cards)
              :promos promos))
      (println "Generated raw_data.edn"))
    (catch Exception e
      (println "Import data failed:" (.getMessage e)))))
