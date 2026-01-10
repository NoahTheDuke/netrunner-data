(ns nr-data.download
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.set]
   [clojure.set :as set]
   [clojure.string :as str]
   [nr-data.data :as data]
   [nr-data.text :refer [add-stripped-card-text]]
   [nr-data.utils :refer [cards->map slugify apply-to-faces-too]]
   [org.httpkit.client :as http]
   [zprint.core :as zp]))

(defn strip-typesetting-chars
  ;; strip out any typesetting characters from card titles
  [target-str]
  (when target-str
    (str/replace target-str #"[ʼ’“”]" {"’" "'"
                                       "ʼ" "'"
                                       "“" "\""
                                       "”" "\""})))

(defn underscore->hyphen
  [s]
  (when s (str/replace s "_" "-")))

(defn remove-nil-values
  [m]
  (into {} (remove (fn [[_ v]] (nil? v)) m)))

(defn parse-response
  [body]
  (json/parse-string body true))

(defn attributes-with-id
  [data]
  (map (fn [item] (assoc (:attributes item) :id (:id item))) data))

(def large-endpoints
  "Endpoints that need pagination (>1000 items)"
  #{"cards" "printings"})

(defn download-nrdb-data
  [path]
  (let [needs-pagination (some #(str/starts-with? path %) large-endpoints)
        url (str "https://api-preview.netrunnerdb.com/api/v3/public/" path
                 (when needs-pagination "?page[size]=2500"))
        data (http/get url)
        {:keys [status body error]} @data]
    (cond
      error (throw (Exception. (str "Failed to download file " error)))
      (= 200 status) (-> body parse-response :data attributes-with-id)
      :else (throw (Exception. (str "Failed to download file " url ", status " status))))))

(defn read-json-file
  [file-path]
  ((comp parse-response slurp) file-path))

(defn read-local-data
  [base-path filename]
  (read-json-file (str base-path "/" filename ".json")))

(defn read-card-dir
  [base-path]
  (->> (str base-path "/pack")
       (io/file)
       (file-seq)
       (filter #(and (.isFile %)
                     (str/ends-with? % ".json")))
       (map read-json-file)
       (flatten)
       (parse-response)))

(defn translate-fields
  "Modify NRDB json data to our schema"
  [fields data]
  (reduce-kv (fn [m k v]
               (if (contains? fields k)
                 (let [[new-k new-v] ((get fields k) [k v])]
                   (assoc m new-k new-v))
                 m))
             {} data))

(defmacro rename
  "Rename a card field"
  ([new-name]
   `(fn [[k# v#]] [~new-name v#]))
  ([new-name f]
   `(fn [[k# v#]] [~new-name (~f v#)])))

(defn convert-cycle
  [v]
  (case v
    "Core Set" "Core"
    "core-set" "core"
    "core2" "revised-core"
    "napd" "napd-multiplayer"
    "Revised Core Set" "Revised Core"
    "revised-core-set" "revised-core"
    "sc19" "system-core-2019"
    v))

(def cycle-fields
  {:name (rename :name convert-cycle)
   :position identity
   :card_set_ids identity})

(defn add-cycle-fields
  [active-standard-cycle-ids cy]
  (let [cycle-id (slugify (:name cy))
        size (count (:card_set_ids cy))
        rotated (if (= "draft" cycle-id)
                  false
                  (not (contains? active-standard-cycle-ids cycle-id)))]
    (-> cy
        (assoc :id cycle-id
               :size size
               :rotated rotated)
        (dissoc :card_set_ids))))

(def set-fields
  {:id (rename :id underscore->hyphen)
   :legacy_code (rename :code)
   :card_cycle_id (rename :cycle-id (comp convert-cycle underscore->hyphen))
   :date_release (rename :date-release)
   :name identity
   :position identity
   :size identity})

(defn deluxe-set?
  [s]
  (case (:cycle-id s)
    ("core" "revised-core" "system-core-2019"
     "creation-and-control" "honor-and-profit" "order-and-chaos" "data-and-destiny"
     "terminal-directive" "reign-and-reverie") true
    ;; else
    false))

(defn set-type?
  [s]
  (case (slugify (:name s))
    ("core-set" "revised-core-set" "system-core-2019"
     "system-gateway" "system-update-2021") :core
    ("creation-and-control" "honor-and-profit"
     "order-and-chaos" "data-and-destiny"
     "reign-and-reverie") :deluxe
    ("magnum-opus" "magnum-opus-reprint" "uprising-booster-pack") :expansion
    "draft" :draft
    "napd-multiplayer" :promo
    ("terminal-directive" "terminal-directive-campaign") :campaign
    ;; else
    :data-pack))

(defn normalize-id [{:as m, :keys [id]} from-k]
  ;; jnet converts ' to - in id, but nrdb removes them altogether
  (let [normalized-id (slugify (get m from-k))]
    (if (= normalized-id id)
      m
      (-> m
          (assoc :original-id id)
          (assoc :id normalized-id)))))

(defn add-set-fields
  [s]
  (-> s
      (normalize-id :name)
      (assoc :deluxe (deluxe-set? s)
             :set-type (set-type? s))))

(defn convert-subtypes
  [subtype]
  (when (seq subtype)
    (->> (str/split subtype #" - ")
         (map slugify)
         (map keyword)
         (into []))))

(defn id->keyword
  [id]
  (when id
    (-> id underscore->hyphen keyword)))

(defn convert-card-type
  "Convert card type, normalizing identity types"
  [type-id]
  (case type-id
    ("corp_identity" "runner_identity") :identity
    (id->keyword type-id)))

(defn parse-int
  [s]
  ;; base-link in cards is string, but in faces is string
  (if (int? s)
    s
    (when (and s (string? s))
      (try (Integer/parseInt s)
           (catch NumberFormatException _ nil)))))

(def card-fields
  {
   :id (rename :id underscore->hyphen)
   :advancement_requirement (rename :advancement-requirement parse-int)
   :agenda_points (rename :agenda-points)
   :base_link (rename :base-link parse-int)
   :cost (rename :cost parse-int)
   :deck_limit (rename :deck-limit)
   :faction_id (rename :faction id->keyword)
   :influence_cost (rename :influence-cost)
   :influence_limit (rename :influence-limit)
   :display_subtypes (rename :subtype convert-subtypes)
   :memory_cost (rename :memory-cost)
   :minimum_deck_size (rename :minimum-deck-size)
   :side_id (rename :side id->keyword)
   :strength identity
   :text identity
   :title (rename :title strip-typesetting-chars)
   :trash_cost (rename :trash-cost)
   :card_type_id (rename :type convert-card-type)
   :is_unique (rename :uniqueness)
   :num_extra_faces (rename :num-extra-faces)
   :faces identity
   :index identity
   })

(defn normalize-variable-values
  [card]
  (cond-> card
    ;; X-cost cards have nil cost
    (and (not (contains? card :cost))
         (contains? #{:operation :event :hardware :resource :program} (:type card)))
    (assoc :cost nil)

    ;; -1 strength means variable
    (= -1 (:strength card))
    (assoc :strength nil)

    ;; Agendas without fixed advancement requirement
    (and (= :agenda (:type card))
         (not (contains? card :advancement-requirement)))
    (assoc :advancement-requirement nil)

    ;; Identities without fixed influence limit
    (and (= :identity (:type card))
         (not (contains? card :influence-limit)))
    (assoc :influence-limit nil)))

(defn strip-extra-faces
  [card]
  (apply dissoc card (when (or (nil? (:num-extra-faces card))
                               (zero? (:num-extra-faces card)))
                       [:num-extra-faces :faces])))

(defn strip-influence-cost
  [card]
  (dissoc card (when (or (and (= :agenda (:type card))
                              (not (or (= :neutral-corp (:faction card))
                                       (= :neutral-runner (:faction card)))))
                         (= :identity (:type card)))
                 :influence-cost)))

(defn add-card-fields
  [card]
  (-> card
      (normalize-id :title)
      remove-nil-values
      normalize-variable-values
      strip-extra-faces
      strip-influence-cost))

(defn add-set-card-fields
  [printing]
  (-> printing
      remove-nil-values
      strip-extra-faces))

(def set-card-fields
  {
   :id (rename :code)
   :flavor identity
   :display_illustrators (rename :illustrator)
   :position identity
   :quantity identity
   :title (rename :card-id (comp slugify strip-typesetting-chars))
   :card_set_name (rename :set-id slugify)
   :attribution identity
   :num_extra_faces (rename :num-extra-faces)
   :faces identity
   :index identity
   :copy_quantity (rename :copy-quantity)
   })

(def mwl-fields
  {:code (rename :id)
   :date_start (rename :date-start)
   :format_id (rename :format underscore->hyphen)
   :name identity
   :point_limit (rename :point-limit)
   :verdicts identity})

(defn convert-verdicts
  [lookup-id verdicts]
  (let [banned         (for [card-id (:banned verdicts)]
                         [(lookup-id (underscore->hyphen card-id)) {:deck-limit 0}])
        restricted     (for [card-id (:restricted verdicts)]
                         [(lookup-id (underscore->hyphen card-id)) {:is-restricted 1}])
        universal-fc   (for [[card-id cost] (:universal_faction_cost verdicts)]
                         [(lookup-id (underscore->hyphen (name card-id))) {:universal-faction-cost cost}])
        global-penalty (for [card-id (:global_penalty verdicts)]
                         [(lookup-id (underscore->hyphen card-id)) {:global-penalty 1}])
        points         (for [[card-id pts] (:points verdicts)]
                         [(lookup-id (underscore->hyphen (name card-id))) {:points pts}])]
    (into {} (concat banned restricted universal-fc global-penalty points))))

(defn make-lookup-id [cards]
  (let [original-ids (->> cards
                          (map #(when (:original-id %)
                                  [(:original-id %) (:id %)]))
                          (into {}))]
    (fn [id]
      (get original-ids id id))))

(defn convert-mwl
  [lookup-id mwl]
  (-> mwl
      (assoc :cards (convert-verdicts lookup-id (:verdicts mwl))
             :id (slugify (:name mwl)))
      (dissoc :verdicts)
      remove-nil-values))

(defn merge-mwls
  [current-mwls api-mwls]
  (let [api-mwls-ids (set (map :id api-mwls))]
    (->> (concat current-mwls api-mwls)
         (group-by :id)
         (map #(last (second %)))
         (map #(if (api-mwls-ids (:id %))
                 %
                 (assoc % :custom true))))))

(defn mwl-sort-k
  [{:keys [format date-start]}]
  (str (condp = format
         "standard" "0"
         "throwback" "1"
         "eternal" "2"
         "3")
       "-"
       date-start))

(defn sort-and-group-set-cards
  [set-cards]
  (->> set-cards
       (sort-by :position)
       (group-by :set-id)))

(defn fetch-data
  "Read NRDB json data. Modify function is mapped to all elements in the data collection."
  ([download-fn m] (fetch-data download-fn m identity))
  ([download-fn {:keys [path fields]} add-fields-function]
   (let [translate-function (partial translate-fields fields)]
     (->> (download-fn path)
          (map (apply-to-faces-too translate-function))
          (map (apply-to-faces-too add-fields-function))))))

(def tables
  {:cycle {:path "card_cycles" :fields cycle-fields}
   :set {:path "card_sets" :fields set-fields}
   :card {:path "cards" :fields card-fields}
   :set-card {:path "printings" :fields set-card-fields}
   :mwl {:path "restrictions" :fields mwl-fields}
   })

(defn snapshot-handler
  [download-fn]
  (print "Downloading and processing snapshots... ")
  (let [active-cycle-ids (->> (download-fn "snapshots")
                              (filter #(and (:active %) (= "standard" (:format_id %))))
                              first
                              :card_cycle_ids
                              (map underscore->hyphen)
                              set)]
    (println "Done")
    active-cycle-ids))

(defn cycle-handler
  [line-ending download-fn active-cycle-ids]
  (print "Downloading and processing cycles... ")
  (let [cycles (->> (fetch-data download-fn (:cycle tables) (partial add-cycle-fields active-cycle-ids))
                    (sort-by :position)
                    (into []))
        path (str "edn/cycles.edn")]
    (io/make-parents path)
    (println "Saving" path)
    (spit path (str (zp/zprint-str cycles) line-ending))
    cycles))

(defn set-handler
  [line-ending download-fn]
  (print "Downloading and processing sets... ")
  (let [sets (->> (fetch-data download-fn (:set tables) add-set-fields)
                  (sort-by :date-release)
                  (into []))
        path (str "edn/sets.edn")]
    (io/make-parents path)
    (println "Saving" path)
    (spit path (str (zp/zprint-str sets) line-ending))
    sets))

(defn card-handler
  [line-ending download-fn]
  (print "Downloading and processing cards... ")
  (let [cards (->> (fetch-data download-fn (:card tables) add-card-fields)
                   (add-stripped-card-text))]
    (println "Saving edn/cards")
    (doseq [[path card] (cards->map :id cards)
            :let [path (str "edn/cards/" path ".edn")]]
      (io/make-parents path)
      (spit path (str (zp/zprint-str card) line-ending)))
    cards))

(defn set-cards-handler
  [line-ending download-fn]
  (print "Downloading and processing set cards... ")
  (let [raw-set-cards (fetch-data download-fn (:set-card tables) add-set-card-fields)
        set-cards (sort-and-group-set-cards raw-set-cards)]
    (println "Saving edn/set-cards")
    (doseq [[path set-card] set-cards
            :let [path (str "edn/set-cards/" path ".edn")]]
      (io/make-parents path)
      (spit path (str (zp/zprint-str set-card) line-ending)))
    set-cards))

(defn mwl-handler
  [line-ending download-fn lookup-id]
  (print "Downloading and processing mwls... ")
  (let [path "edn/mwls.edn"
        current-mwls (data/load-edn-from-dir path)
        mwls (->> (fetch-data download-fn (:mwl tables) (partial convert-mwl lookup-id))
                  (merge-mwls current-mwls)
                  (sort-by mwl-sort-k))]
    (io/make-parents path)
    (println "Saving" path)
    (spit path (str (zp/zprint-str (into [] mwls)) line-ending))
    mwls))

(defn download-from-nrdb
  [& args]
  (let [line-ending "\n"
        use-local (some #{"--local"} args)
        localpath (first (remove #(and % (str/starts-with? % "--")) args))
        download-fn (if use-local
                      (partial read-local-data localpath)
                      download-nrdb-data)

        active-cycle-ids (snapshot-handler download-fn)

        _cycles (cycle-handler line-ending download-fn active-cycle-ids)

        _sets (set-handler line-ending download-fn)

        card-download-fn (if use-local
                           (partial read-card-dir localpath)
                           download-nrdb-data)

        cards (card-handler line-ending card-download-fn)

        _set-cards (set-cards-handler line-ending download-fn)

        _lookup-id (make-lookup-id cards)

        ;; don't replace mwls, they are manually edited
        ;;_mwls (mwl-handler line-ending download-fn lookup-id)
        ]

    (println "Done!")))

(comment
  (download-from-nrdb)
  ,)
