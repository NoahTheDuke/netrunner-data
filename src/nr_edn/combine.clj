(ns nr-edn.combine
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :refer [rename-keys]]
            [org.httpkit.client :as http]
            [cheshire.core :refer [generate-string]]
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
  [sets cards formats mwls]
  (let [set->cards (reduce (fn [m [set-id card-id]]
                             (if (contains? m set-id)
                               (assoc m set-id (conj (get m set-id) card-id))
                               (assoc m set-id #{card-id})))
                           {}
                           (map (juxt :set-id :card-id) cards))
        cycle->sets (into {}
                          (for [[f sts] (group-by :cycle_code (vals sets))]
                            {f (into #{} (map :id sts))}))
        format->cards (into {}
                            (for [[k f] formats]
                              {k (apply clojure.set/union
                                        (for [cy (:cycles f)
                                              sts (get cycle->sets cy)]
                                          (get set->cards sts)))}))]
    (into {}
          (for [card cards
                :let [id (:id card)]]
            {id (into {}
                      (for [[f cs] format->cards
                            :let [mwl (get-in formats [f :mwl])]]
                        {(keyword f)
                         (cond
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

(defn make-image-url
  "Create a URI to the card in CardGameDB"
  [card s]
  (if (:ffg-id s)
    (str "https://www.cardgamedb.com/forums/uploads/an/med_ADN" (:ffg-id s) "_" (:position card) ".png")
    (str "https://netrunnerdb.com/card_image/" (:code card) ".png")))

(defn get-uri
  "Figure out the card art image uri"
  [card s]
  (if (contains? card :image-url)
    (:image-url card)
    (make-image-url card s)))

(defn link-previous-versions
  [[title cards]]
  (if (= 1 (count cards))
    (first cards)
    (assoc (last cards)
           :previous-versions
           (->> cards
                butlast
                (mapv :code)))))

(defn load-cards
  [sides factions types subtypes sets formats mwls]
  (let [
        set-cards (load-edn-from-dir "edn/set-cards")
        raw-cards (cards->map :id (load-edn-from-dir "edn/cards"))
        cards (merge-sets-and-cards set-cards raw-cards)
        card->formats (generate-formats sets cards formats mwls)
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
            :image_url (get-uri card s)
            :influencelimit (:influence-limit card)
            :memoryunits (:memory-cost card)
            :minimumdecksize (:minimum-deck-size card)
            :normalizedtitle (:id card)
            :number (:position card)
            :quantity (:quantity card)
            :rotated (= :rotated (:standard (get card->formats (:id card))))
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
         (sort-by :code)
         (map #(dissoc % :date-release))
         (group-by :normalizedtitle)
         (map link-previous-versions)
         cards->map)))

(defn build-system-core-2019
  [[title cards]]
  (when (= "System Core 2019" (:setname (last cards)))
    (merge (last cards)
           (->> cards
                butlast
                last
                (select-keys
                  [:illustrator
                   :position
                   :quantity])))))

(defn get-json-cost
  [card]
  (or (:cost card)
      (case (:type card)
        (:asset :event :hardware :operation :program :resource :upgrade) "null"
        nil)))

(defn get-json-faction
  [card]
  (if (= :neutral (:faction card))
    (if (= :corp (:side card))
      "neutral-corp"
      "neutral-runner")
    (:faction card)))

(defn get-json-strength
  [card]
  (or (:strength card)
      (case (:type card)
        (:ice :program) "null"
        nil)))

(defn convert-to-json
  []
  (let [
        mwls (load-data "mwls" {:id :code})
        sides (load-data "sides")
        factions (load-data "factions")
        types (load-data "types")
        subtypes (load-data "subtypes")
        formats (load-data "formats")
        cycles (load-data "cycles")
        sets (load-sets cycles)
        set-cards (load-edn-from-dir "edn/set-cards")
        raw-cards (cards->map :id (load-edn-from-dir "edn/cards"))
        cards (merge-sets-and-cards set-cards raw-cards)
        card->formats (generate-formats sets cards formats mwls)
        ]
    (->> (for [card cards
               :let [s (get sets (:set-id card))]]
           {:advancement_cost (:advancement-requirement card)
            :agenda_points (:agenda-points card)
            :base_link (:base-link card)
            :code (:code card)
            :cost (get-json-cost (:cost card))
            :date-release (:date-release s)
            :deck_limit (:deck-limit card)
            :faction_code (get-json-faction card)
            :faction_cost (:influence-value card)
            :illustrator (:illustrator card)
            :influence_limit (:influence-limit card)
            :keywords (when (seq (:subtype card))
                        (string/join " - " (map #(:name (get subtypes %)) (:subtype card))))
            :memory_cost (:memory-cost card)
            :minimum_deck_size (:minimum-deck-size card)
            :pack_code (:code s)
            :position (:position card)
            :quantity (:quantity card)
            :setname (:name s)
            :side_code (:side card)
            :strength (get-json-strength card)
            :text (:text card)
            :title (:title card)
            :trash_cost (:trash-cost card)
            :type_code (:type card)
            :uniqueness (:uniqueness card)})
         (map prune-null-fields)
         (sort-by :code)
         (group-by :title)
         (map build-system-core-2019)
         (map #(dissoc % :setname :date-release))
         (filter identity)
         (sort-by :code)
         (#(generate-string % {:pretty true}))
         )))

(defn combine-for-jnet
  []
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
    (println "Done!")))
