(ns nr-data.data
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :refer [rename-keys union]]
   [clojure.string :as str]
   [cond-plus.core :refer [cond+]]
   [nr-data.utils :refer [cards->map prune-null-fields]]
   [medley.core :refer [find-first]]))

(defn read-edn-file
  [file-path]
  (edn/read-string (slurp file-path)))

(defn load-edn-from-dir
  [file-path]
  (->> (io/file file-path)
       (file-seq)
       (filter (fn [file] (and (.isFile file)
                               (str/ends-with? file ".edn"))))
       (map read-edn-file)
       (flatten)
       (into [])))

(comment
  (load-edn-from-dir "edn/set-cards")
  )

(defn load-data
  ([filename] (load-data filename {:id :code}))
  ([filename kmap]
   (cards->map
     (for [m (read-edn-file (str "edn/" filename ".edn"))]
       (rename-keys m kmap)))))

; Localized data is expected to have a bunch of empty data, so filter out
; everything that's nil, and remove the entry if only the code remains.
(defn reduce-edn-file
  [file-path]
  (->> (read-edn-file file-path)
       (map #(into {} (filter val %)))
       (remove #(= (keys %) '(:code)))))

(defn load-localized-data
  []
  (->> (str "edn/")
       (io/file)
       (file-seq)
       (filter #(and (.isFile %)
                     (str/starts-with? % "edn/cards-")
                     (str/ends-with? % ".edn")))
       (map #(let [key (str/replace (str/replace (.getName %) #".edn" "") #"cards-" "")]
               {key (cards->map (reduce-edn-file %))}))
       (into {})))

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
                            :let [mwl (get-in formats [f :mwl])
                                  banned-subtypes (get-in mwls [mwl :subtypes] {})]]
                        {(keyword f)
                         (cond
                           ;; gotta check mwl first
                           (get-in mwls [mwl :cards id])
                           (let [restrictions (get-in mwls [mwl :cards id])]
                             (merge
                               (when (:deck-limit restrictions)
                                 {:banned true})
                               (when (:is-restricted restrictions)
                                 {:legal true :restricted true})
                               (when (:points restrictions)
                                 {:legal true :points (:points restrictions)})))
                           (seq (keep banned-subtypes (:subtype card)))
                           (let [restrictions (into {} (keep banned-subtypes) (:subtype card))]
                             (merge
                               (when (:deck-limit restrictions)
                                 {:banned true})
                               (when (:is-restricted restrictions)
                                 {:legal true :restricted true})
                               (when (:points restrictions)
                                 {:legal true :points (:points restrictions)})))
                           ;; then we can check if the card is on the list
                           (contains? cs id)
                           {:legal true}
                           ;; as a special case, throwback treats every rotated card as restricted,
                           ;; barring specifically the terminal directive campaign (not real cards)
                           (and (= f "throwback")
                                (not= (:set-id card) "terminal-directive-campaign"))
                           {:legal true :restricted true}
                           ;; neither mwl nor in the format
                           :else
                           {:rotated true})}))}))))

(defn link-previous-versions
  [[_ cards]]
  (if (= 1 (count cards))
    (first cards)
    (assoc (last cards)
           :previous-versions
           (->> cards
                butlast
                (mapv #(select-keys % [:code :set_code]))))))

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
            :faces (:faces card)
            :named-faces (->> card :faces
                              (map (fn [{:keys [index title]}]
                                     [(str index) title]))
                              (into {}))
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
         (cards->map))))

(defn mwls [] (load-data "mwls" {:id :code}))
(defn sides [] (load-data "sides"))
(defn factions [] (load-data "factions"))
(defn types [] (load-data "types"))
(defn subtypes [] (load-data "subtypes"))
(defn formats [] (load-data "formats"))
(defn cycles [] (load-data "cycles"))
(defn sets
  ([] (load-sets (cycles)))
  ([cycles] (load-sets cycles)))
(defn localized-data [] (load-localized-data))
(defn combined-cards [] (load-cards (sides) (factions) (types) (subtypes) (sets) (formats) (mwls)))

(defn set-cards [] (load-edn-from-dir "edn/set-cards"))
(defn raw-cards [] (cards->map :id (load-edn-from-dir "edn/cards")))
(defn cards [] (merge-sets-and-cards (set-cards) (raw-cards)))
(defn card->formats [] (generate-formats (sets) (cards) (formats) (mwls)))
