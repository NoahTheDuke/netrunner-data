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
         (sort-by :date-release)
         (map #(dissoc % :date-release))
         (group-by :normalizedtitle)
         (map link-previous-versions)
         cards->map)))

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
    (println "Done!"))
  ; (catch Exception e
  ;   (println "Import data failed:" (.getMessage e))))
  )








; (require '[clojure.java.io :as io])

; (def old->new [["03049" "25058"] ["20040" "25043"] ["03040" "25051"] ["20020" "25023"]
;                ["06003" "25094"] ["06068" "25123"] ["20128" "25142"] ["13040" "25083"]
;                ["20090" "25136"] ["05006" "25088"] ["06095" "25021"] ["20112" "25109"]
;                ["13031" "25069"] ["08033" "25077"] ["05035" "25027"] ["20029" "25036"]
;                ["20078" "25124"] ["04004" "25025"] ["20115" "25114"] ["20069" "25076"]
;                ["08115" "25120"] ["06066" "25111"] ["20100" "25095"] ["20006" "25008"]
;                ["20116" "25115"] ["08094" "25106"] ["20109" "25104"] ["20099" "25093"]
;                ["08023" "25022"] ["20045" "25049"] ["20009" "25011"] ["20122" "25121"]
;                ["13033" "25072"] ["20097" "25090"] ["20019" "25020"] ["20132" "25146"]
;                ["20005" "25007"] ["04081" "25004"] ["20088" "25134"] ["04054" "25092"]
;                ["20003" "25005"] ["20108" "25103"] ["20043" "25046"] ["20098" "25091"]
;                ["02031" "25085"] ["02091" "25065"] ["04009" "25064"] ["20041" "25044"]
;                ["20120" "25118"] ["22010" "25032"] ["20028" "25035"] ["20075" "25082"]
;                ["06052" "25002"] ["20068" "25075"] ["01031" "25039"] ["20052" "25056"]
;                ["20016" "25017"] ["03028" "25041"] ["20023" "25026"] ["02032" "25086"]
;                ["20079" "25126"] ["20105" "25100"] ["20077" "25122"] ["13028" "25067"]
;                ["13006" "25033"] ["02066" "25053"] ["06014" "25014"] ["20013" "25015"]
;                ["08078" "25127"] ["20121" "25119"] ["20015" "25016"] ["20102" "25098"]
;                ["20114" "25113"] ["20059" "25062"] ["20070" "25078"] ["20125" "25141"]
;                ["20048" "25052"] ["20010" "25012"] ["20056" "25059"] ["13057" "25147"]
;                ["04074" "25097"] ["20037" "25040"] ["04090" "25081"] ["09003" "25105"]
;                ["08117" "25129"] ["20084" "25130"] ["06120" "25063"] ["13003" "25029"]
;                ["20082" "25128"] ["20066" "25074"] ["20051" "25055"] ["01038" "25048"]
;                ["02084" "25028"] ["06048" "25139"] ["04102" "25003"] ["20064" "25070"]
;                ["20107" "25102"] ["10001" "25006"] ["02107" "25050"] ["20110" "25107"]
;                ["08079" "25135"] ["20086" "25132"] ["20063" "25068"] ["13050" "25133"]
;                ["01007" "25010"] ["20042" "25045"] ["20085" "25131"] ["20026" "25031"]
;                ["20129" "25143"] ["08058" "25125"] ["20113" "25112"] ["20024" "25030"]
;                ["20091" "25138"] ["20093" "25084"] ["20018" "25019"] ["20038" "25042"]
;                ["20011" "25013"] ["20130" "25144"] ["03052" "25060"] ["20106" "25101"]
;                ["20033" "25038"] ["20119" "25117"] ["20071" "25079"] ["13053" "25140"]
;                ["02055" "25110"] ["20058" "25061"] ["20096" "25089"] ["20131" "25145"]
;                ["22004" "25009"] ["20095" "25087"] ["20021" "25024"] ["20044" "25047"]
;                ["20072" "25080"] ["13008" "25034"] ["20049" "25054"] ["20104" "25099"]
;                ["20061" "25066"] ["02079" "25137"] ["20065" "25071"] ["20101" "25096"]
;                ["02110" "25073"] ["20117" "25116"] ["06086" "25108"] ["20017" "25018"]
;                ["20032" "25037"] ["20001" "25001"] ["03051" "25057"]])

; (defn copy-file
;   [source-path dest-path]
;   (io/copy (io/file source-path) (io/file dest-path)))

; (defn make-png
;   [code]
;   (str "resources/public/img/cards/" code ".png"))

; (doseq [[old new] old->new]
;   (copy-file (make-png old) (make-png new)))
