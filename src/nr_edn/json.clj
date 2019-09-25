(ns nr-edn.json
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :refer [rename-keys]]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [nr-edn.combine :refer [load-data load-sets load-edn-from-dir
                                    generate-formats merge-sets-and-cards]]
            [nr-edn.utils :refer [cards->map vals->vec prune-null-fields slugify]]))

(defn build-system-core-2019
  [[title cards]]
  (when (= "System Core 2019" (:setname (last cards)))
    (apply merge
           (last cards)
           (flatten
             (map #(select-keys %
                                [:flavor
                                 :illustrator])
                  (butlast cards))))))

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

(defn get-json-influence-limit
  [card]
  (or (:influence-limit card)
      (when (= :identity (:type card)) "null")
      nil))

(defn get-json-strength
  [card]
  (or (:strength card)
      (when (= :ice (:type card))
        "null")
      (when (and (= :program (:type card))
                 (some #{:icebreaker} (:subtype card)))
        "null")
      nil))

(defn get-json-mwl-code
  [mwl]
  (case (:code mwl)
    "napd-mwl-1-0" "NAPD_MWL_1.0"
    "napd-mwl-1-1" "NAPD_MWL_1.1"
    "napd-mwl-1-2" "NAPD_MWL_1.2"
    "napd-mwl-2-0" "NAPD_MWL_2.0"
    "napd-mwl-2-1" "NAPD_MWL_2.1"
    "napd-mwl-2-2" "NAPD_MWL_2.2"
    "standard-mwl-3-1" "standard-mwl-3.1"
    "standard-mwl-3-2" "standard-mwl-3.2"
    (:code mwl)))

(defn get-json-mwl
  [cards mwls]
  (for [m (filter #(= (:format %) "standard") (vals mwls))]
    (assoc m
           :code (get-json-mwl-code m)
           :cards
           (into {}
                 (for [[k v] (:cards m)
                       :let [t (first (keys v))
                             amt (first (vals v))]]
                   (into {}
                         (for [c (map :code (get cards k))]
                           [c {(slugify t "_") amt}])))))))

(defn get-json-code
  [cy]
  (case (:code cy)
    "revised-core" "core2"
    "napd-multiplayer" "napd"
    "system-core-2019" "sc19"
    (:code cy)))

(defn get-json-name
  [cy]
  (case (:name cy)
    "Core" "Core Set"
    "Revised Core" "Revised Core Set"
    (:name cy)))

(defn convert-to-json
  []
  (let [
        mwls (load-data "mwls" {:id :code})
        sides (load-data "sides")
        subtypes (load-data "subtypes")
        formats (load-data "formats")
        cycles (load-data "cycles")
        sets (load-sets cycles)
        set-cards (load-edn-from-dir "edn/set-cards")
        raw-cards (cards->map :id (load-edn-from-dir "edn/cards"))
        cards (merge-sets-and-cards set-cards raw-cards)
        card->formats (generate-formats sets cards formats mwls)
        mwls (get-json-mwl (group-by :card-id cards) mwls)
        pretty {:pretty {:indentation 4
                         :indent-arrays? true
                         :object-field-value-separator ": "}}
        ]
    (io/make-parents "json/pack/temp")

    ;; mwl.json
    (->> (for [mwl mwls]
           {:cards (into (sorted-map) (:cards mwl))
            :code (:code mwl)
            :date_start (:date-start mwl)
            :name (:name mwl)})
         (sort-by :date_start)
         (map #(into (sorted-map) %))
         (#(json/generate-string % pretty))
         (#(str % "\n"))
         (spit (io/file "json" "mwl.json")))

    ;; cycles.json
    (->> (for [[k cy] cycles]
           {:code (get-json-code cy)
            :name (get-json-name cy)
            :position (:position cy)
            :rotated (:rotated cy)
            :size (:size cy)})
         (sort-by :position)
         (map #(into (sorted-map) %))
         (#(json/generate-string % pretty))
         (#(str % "\n"))
         (spit (io/file "json" "cycles.json")))

    ;; packs.json
    (->> (for [[k s] sets]
           {:code (:code s)
            :cycle_code (get-json-code {:code (:cycle_code s)})
            :date_release (:date-release s)
            :ffg_id (:ffg-id s)
            :name (:name s)
            :position (:position s)
            :size (:size s)})
         (sort-by :code)
         (map #(into (sorted-map) %))
         (#(json/generate-string % pretty))
         (#(str % "\n"))
         (spit (io/file "json" "packs.json")))

    ;; pack/*.json
    (let [packs
          (->> (for [card cards
                     :let [s (get sets (:set-id card))]]
                 {:advancement_cost (:advancement-requirement card)
                  :agenda_points (:agenda-points card)
                  :base_link (:base-link card)
                  :code (:code card)
                  :cost (get-json-cost card)
                  :deck_limit (:deck-limit card)
                  :faction_code (get-json-faction card)
                  :faction_cost (:influence-cost card)
                  :flavor (:flavor card)
                  :illustrator (:illustrator card)
                  :influence_limit (get-json-influence-limit card)
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
               (sort-by :code)
               (map prune-null-fields)
               (filter identity)
               (map (fn [card]
                      (if-let [pairs (seq (flatten (for [[k v] card :when (= v "null")] [k nil])))]
                        (apply assoc card pairs)
                        card)))
               (sort-by :position)
               (group-by :setname))]
      (doseq [[k pack] packs]
        (->> pack
             (map #(dissoc % :setname))
             (map #(into (sorted-map) %))
             (#(json/generate-string % pretty))
             (#(str % "\n"))
             (spit (io/file "json" "pack" (str (:pack_code (first pack)) ".json"))))))))
