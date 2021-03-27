(ns nr-data.combine
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.edn :as edn]
    [clojure.set :refer [rename-keys union]]
    [org.httpkit.client :as http]
    [cheshire.core :as json]
    [cond-plus.core :refer [cond+]]
    [nr-data.utils :refer [cards->map vals->vec prune-null-fields]]))

(defn read-edn-file
  [file-path]
  ((comp edn/read-string slurp) file-path))

(defn load-edn-from-dir
  [file-path]
  (->> (io/file file-path)
       (file-seq)
       (filter #(and (.isFile %)
                     (str/ends-with? % ".edn")))
       (map read-edn-file)
       (flatten)
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
       :bigbox (:deluxe s)
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
  (map #(merge (get raw-cards (:card-id %)) %) set-cards))

(defn get-cost
  [card]
  (let [cost (:cost card)]
    (cond+
      [(= "X" cost) 0]
      [cost]
      [(case (:type card)
         (:asset :event :hardware :operation :program :resource :upgrade) 0
         nil)])))

(defn get-strength
  [card]
  (or (:strength card)
      (case (:type card)
        (:ice :program) 0
        nil)))

(defn get-set->cards
  [cards]
  (reduce (fn [m [set-id card-id]]
            (if (contains? m set-id)
              (assoc m set-id (conj (get m set-id) card-id))
              (assoc m set-id #{card-id})))
          {}
          (map (juxt :set-id :card-id) cards)))

(defn get-cycle->sets
  [sets]
  (into {}
        (for [[f sts] (group-by :cycle_code (vals sets))]
          {f (into #{} (map :id sts))})))

(defn get-format->cards
  [formats set->cards cycle->sets]
  (into {}
        (for [[k f] formats
              :let [cards (:cards f)
                    sets (:sets f)
                    cycles (:cycles f)]]
          {k (apply union
                    (concat
                      (into #{} cards)
                      (for [s sets]
                        (get set->cards s))
                      (for [cy cycles
                            s (get cycle->sets cy)]
                        (get set->cards s))))})))

(defn generate-formats
  [sets cards formats mwls]
  (let [set->cards (get-set->cards cards)
        cycle->sets (get-cycle->sets sets)
        format->cards (get-format->cards formats set->cards cycle->sets)]
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

(defn link-previous-versions
  [[title cards]]
  (if (= 1 (count cards))
    (first cards)
    (assoc (last cards)
           :previous-versions
           (->> cards
                butlast
                (mapv :code)))))

(defn print-null-subtypes
  [subtypes card subtype-keyword]
  (let [subtype-string (get subtypes subtype-keyword)]
    (when-not subtype-string
      (println (:title card) "has a malformed subtype:" subtype-keyword))
    (:name subtype-string)))

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
            :factioncost (:influence-cost card)
            :format (get card->formats (:id card))
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
                       (str/join " - " (map #(print-null-subtypes subtypes card %) (:subtype card))))
            :subtypes (mapv #(print-null-subtypes subtypes card %) (:subtype card))
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

(defn combine-for-jnet
  [& _]
  (let [
        mwls (load-data "mwls" {:id :code})
        sides (load-data "sides")
        factions (load-data "factions")
        types (load-data "types")
        subtypes (load-data "subtypes")
        formats (load-data "formats")
        cycles (load-data "cycles")
        sets (load-sets cycles)
        cards (load-cards sides factions types subtypes sets formats mwls)]
    (print "Writing edn/raw_data.edn...")
    (spit (io/file "edn" "raw_data.edn")
          (sorted-map
            :cycles (vals->vec :position cycles)
            :sets (vals->vec :position sets)
            :cards (vals->vec :code cards)
            :formats (vals->vec :date-release formats)
            :mwls (vals->vec :date-start mwls)))
    (println "Done!")))

(comment
  (combine-for-jnet)
  )
