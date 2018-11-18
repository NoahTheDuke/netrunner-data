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
         (map #(last (val %)))
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

; (def old->new [["03049" "26058"] ["20040" "26043"] ["03040" "26051"] ["20020" "26023"]
;                ["06003" "26094"] ["06068" "26123"] ["20128" "26142"] ["13040" "26083"]
;                ["20090" "26136"] ["05006" "26088"] ["06095" "26021"] ["20112" "26109"]
;                ["13031" "26069"] ["08033" "26077"] ["05035" "26027"] ["20029" "26036"]
;                ["20078" "26124"] ["04004" "26025"] ["20115" "26114"] ["20069" "26076"]
;                ["08115" "26120"] ["06066" "26111"] ["20100" "26095"] ["20006" "26008"]
;                ["20116" "26115"] ["08094" "26106"] ["20109" "26104"] ["20099" "26093"]
;                ["08023" "26022"] ["20045" "26049"] ["20009" "26011"] ["20122" "26121"]
;                ["13033" "26072"] ["20097" "26090"] ["20019" "26020"] ["20132" "26146"]
;                ["20005" "26007"] ["04081" "26004"] ["20088" "26134"] ["04054" "26092"]
;                ["20003" "26005"] ["20108" "26103"] ["20043" "26046"] ["20098" "26091"]
;                ["02031" "26085"] ["02091" "26065"] ["04009" "26064"] ["20041" "26044"]
;                ["20120" "26118"] ["22010" "26032"] ["20028" "26035"] ["20075" "26082"]
;                ["06052" "26002"] ["20068" "26075"] ["01031" "26039"] ["20052" "26056"]
;                ["20016" "26017"] ["03028" "26041"] ["20023" "26026"] ["02032" "26086"]
;                ["20079" "26126"] ["20105" "26100"] ["20077" "26122"] ["13028" "26067"]
;                ["13006" "26033"] ["02066" "26053"] ["06014" "26014"] ["20013" "26015"]
;                ["08078" "26127"] ["20121" "26119"] ["20015" "26016"] ["20102" "26098"]
;                ["20114" "26113"] ["20059" "26062"] ["20070" "26078"] ["20125" "26141"]
;                ["20048" "26052"] ["20010" "26012"] ["20056" "26059"] ["13057" "26147"]
;                ["04074" "26097"] ["20037" "26040"] ["04090" "26081"] ["09003" "26105"]
;                ["08117" "26129"] ["20084" "26130"] ["06120" "26063"] ["13003" "26029"]
;                ["20082" "26128"] ["20066" "26074"] ["20051" "26055"] ["01038" "26048"]
;                ["02084" "26028"] ["06048" "26139"] ["04102" "26003"] ["20064" "26070"]
;                ["20107" "26102"] ["10001" "26006"] ["02107" "26050"] ["20110" "26107"]
;                ["08079" "26135"] ["20086" "26132"] ["20063" "26068"] ["13050" "26133"]
;                ["01007" "26010"] ["20042" "26045"] ["20085" "26131"] ["20026" "26031"]
;                ["20129" "26143"] ["08058" "26125"] ["20113" "26112"] ["20024" "26030"]
;                ["20091" "26138"] ["20093" "26084"] ["20018" "26019"] ["20038" "26042"]
;                ["20011" "26013"] ["20130" "26144"] ["03052" "26060"] ["20106" "26101"]
;                ["20033" "26038"] ["20119" "26117"] ["20071" "26079"] ["13053" "26140"]
;                ["02055" "26110"] ["20058" "26061"] ["20096" "26089"] ["20131" "26145"]
;                ["22004" "26009"] ["20095" "26087"] ["20021" "26024"] ["20044" "26047"]
;                ["20072" "26080"] ["13008" "26034"] ["20049" "26054"] ["20104" "26099"]
;                ["20061" "26066"] ["02079" "26137"] ["20065" "26071"] ["20101" "26096"]
;                ["02110" "26073"] ["20117" "26116"] ["06086" "26108"] ["20017" "26018"]
;                ["20032" "26037"] ["20001" "26001"] ["03051" "26057"]])

; (defn copy-file
;   [source-path dest-path]
;   (io/copy (io/file source-path) (io/file dest-path)))

; (defn make-png
;   [code]
;   (str "resources/public/img/cards/" code ".png"))

; (doseq [[old new] old->new]
;   (copy-file (make-png old) (make-png new)))
