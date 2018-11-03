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
       :date-release (:date-release s)
       :ffg-id (:ffg-id s)
       :id (:id s)
       :name (:name s)
       :position (:position s)
       ; :rotated (:rotated cy)
       :size (:size s)})))

(defn merge-sets-and-cards
  [set-cards raw-cards]
  (map #(merge % (get raw-cards (:card-id %))) set-cards))

(defn get-cost
  [card]
  (or (:cost card)
      (case (:type card)
        (:asset :event :hardware :operation :program :resource :upgrade) 0
        nil)))

(defn get-strength
  [card]
  (or (:strength card)
      (case (:type card)
        (:ice :program) 0
        nil)))

(defn generate-formats
  [cards formats mwls]
  (let [set->cards (reduce (fn [m [set-id card-id]]
                             (if (contains? m set-id)
                               (assoc m set-id (conj (get m set-id) card-id))
                               (assoc m set-id #{card-id})))
                           {}
                           (map (juxt :set-id :card-id) cards))
        format->cards (into
                        {}
                        (for [[k f] formats]
                          {k (apply clojure.set/union
                                    (for [cy (:cycles f)]
                                      (get set->cards cy)))}))]
    (into
      {}
      (for [card cards
            :let [id (:id card)]]
        {id
         (into
           {}
           (for [[f cs] format->cards
                 :let [mwl (get-in formats [f :mwl])]]
             {f (cond
                  ;; gotta check mwl first
                  (get-in mwls [mwl :cards id])
                  (if (= :deck-limit (first (keys (get-in mwls [mwl :cards id]))))
                    :banned
                    :restricted)
                  ;; then we can check if the card is on the list
                  (contains? cs id)
                  :legal
                  ;; neither mwl nor in the format
                  :else
                  :rotated)}))}))))

(defn prune-null-fields
  [card]
  (apply dissoc card (for [[k v] card :when (nil? v)] k)))

(defn load-cards
  [sides factions types subtypes sets formats mwls]
  (let [
        set-cards (load-edn-from-dir "edn/set-cards")
        raw-cards (cards->map :id (load-edn-from-dir "edn/cards"))
        cards (merge-sets-and-cards set-cards raw-cards)
        card->formats (generate-formats cards formats mwls)
        ]
    (->> (for [card cards
               :let [s (get sets (:set-id card))]]
           {:advancementcost (:advancement-requirement card)
            :agendapoints (:agenda-points card)
            :baselink (:base-link card)
            :code (:code card)
            :cost (get-cost card)
            :cycle_code (:cycle_code s)
            :date-release (:date-release s)
            :deck-limit (:deck-limit card)
            :faction (:name (get factions (:faction card)))
            :factioncost (:influence-value card)
            :format (get card->formats (:id card))
            :image_url (:image-url card)
            :influencelimit (:influence-limit card)
            :memoryunits (:memory-cost card)
            :minimumdecksize (:minimum-deck-size card)
            :normalizedtitle (:id card)
            :number (:position card)
            :quantity (:quantity card)
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
         (sort-by :date-release)
         (map #(dissoc % :date-release))
         (group-by :normalizedtitle)
         (map #(last (val %)))
         cards->map)))

(defn combine-for-jnet
  []
  (try
    (let [
          mwls (load-data "mwls" {:id :code})
          sides (load-data "sides")
          factions (load-data "factions")
          types (load-data "types")
          subtypes (load-data "subtypes")
          formats (load-data "formats")
          cycles (load-data "cycles")
          sets (load-sets cycles)
          cards (load-cards sides factions types subtypes sets formats mwls)
          promos (read-edn-file "edn/promos.edn")
          ]
      (print "Writing edn/raw_data.edn...")
      (spit (io/file "edn" "raw_data.edn")
            (sorted-map
              :cycles (vals->vec :position cycles)
              :sets (vals->vec :position sets)
              :cards (vals->vec :code cards)
              :formats (vals->vec :date-release formats)
              :mwls (vals->vec :date-start mwls)
              :promos promos))
      (println "Done!"))
    (catch Exception e
      (println "Import data failed:" (.getMessage e)))))
