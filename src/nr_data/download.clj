(ns nr-data.download
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.set :refer [rename-keys]]
    [org.httpkit.client :as http]
    [cheshire.core :as json]
    [zprint.core :as zp]
    [nr-data.scratch :refer [clean-card-text]]
    [nr-data.utils :refer [slugify cards->map]]))

(defn parse-response
  [body]
  (json/parse-string body true))

(defn download-nrdb-data
  [path]
  (let [data (http/get (str "http://www.netrunnerdb.com/api/2.0/public/" path))
        {:keys [status body error] :as resp} @data]
    (cond
      error (throw (Exception. (str "Failed to download file " error)))
      (= 200 status) (:data (parse-response body))
      :else (throw (Exception. (str "Failed to download file, status " status))))))

(defn read-json-file
  [file-path]
  ((comp parse-response slurp) file-path))

(defn read-local-data
  [base-path filename]
  (read-json-file (str base-path "/" filename ".json")))

(defn read-card-dir
  [base-path]
  (->> (str base-path "/pack")
       io/file
       file-seq
       (filter #(and (.isFile %)
                     (str/ends-with? % ".json")))
       (map read-json-file)
       flatten
       parse-response))

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
    "core2" "revised-core"
    "napd" "napd-multiplayer"
    "Revised Core Set" "Revised Core"
    "sc19" "system-core-2019"
    v))

(def cycle-fields
  {:name (rename :name convert-cycle)
   :position identity
   :rotated identity
   :size identity})

(defn add-cycle-fields
  [cy]
  (assoc cy :id (slugify (:name cy))))

(def set-fields
  {:code identity
   :cycle_code (rename :cycle-id convert-cycle)
   :date_release (rename :date-release)
   :ffg_id (rename :ffg-id)
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

(defn add-set-fields
  [s]
  (-> s
      (assoc :id (slugify (:name s))
             :deluxe (deluxe-set? s)
             :set-type (set-type? s))))

(defn convert-subtypes
  [subtype]
  (when (seq subtype)
    (->> (str/split subtype #" - ")
         (map slugify)
         (map keyword)
         (into []))))

(def card-fields
  {
   :advancement_cost (rename :advancement-requirement)
   :agenda_points (rename :agenda-points)
   :base_link (rename :base-link)
   :cost identity
   :deck_limit (rename :deck-limit)
   :faction_code (rename :faction keyword)
   :faction_cost (rename :influence-cost)
   :influence_limit (rename :influence-limit)
   :keywords (rename :subtype convert-subtypes)
   :memory_cost (rename :memory-cost)
   :minimum_deck_size (rename :minimum-deck-size)
   :side_code (rename :side keyword)
   :strength identity
   :text identity
   :title identity
   :trash_cost (rename :trash-cost)
   :type_code (rename :type keyword)
   :uniqueness identity
   })

(defn add-card-fields
  [card]
  (-> card
      (assoc :id (slugify (:title card)))
      (dissoc (when (or (and (= :agenda (:type card))
                             (not (or (= :neutral-corp (:faction card))
                                      (= :neutral-runner (:faction card)))))
                        (= :identity (:type card)))
                :influence-cost))))

(def set-card-fields
  {
   :code identity
   :flavor identity
   :illustrator identity
   :pack_code (rename :pack-code)
   :position identity
   :quantity identity
   :title (rename :card-id slugify)
   :text identity
   })

(defn add-set-card-fields
  [cards set-map c]
  (let [s (get set-map (:pack-code c))]
    (-> c
        (dissoc :pack-code :text)
        (assoc :set-id (:id s)))))

(def mwl-fields
  {:cards identity
   :code identity
   :date_start (rename :date-start)
   :name identity})

(defn convert-mwl
  [set-cards-map mwl]
  (-> mwl
      (assoc :cards (reduce-kv
                      (fn [m k v]
                        (let [c (name k)
                              s (get set-cards-map c)]
                          (assoc m
                                 (:card-id s)
                                 (reduce-kv
                                   (fn [m_ k_ v_]
                                     (assoc m_ (-> k_ slugify keyword) v_))
                                   {}
                                   v))))
                      {}
                      (:cards mwl))
                 :id (-> mwl :name slugify))
      (dissoc :code)))

(defn sort-and-group-set-cards
  [set-cards]
  (->> set-cards
       (sort-by :position)
       (group-by :set-id)))

(defn fetch-data
  "Read NRDB json data. Modify function is mapped to all elements in the data collection."
  ([download-fn m] (fetch-data download-fn m identity))
  ([download-fn {:keys [path fields]} add-fields-function]
   (->> (download-fn path)
        (map (partial translate-fields fields))
        (map add-fields-function))))

(def tables
  {:cycle {:path "cycles" :fields cycle-fields}
   :set {:path "packs" :fields set-fields}
   :card {:path "cards" :fields card-fields}
   :set-card {:path "cards" :fields set-card-fields}
   :mwl {:path "mwl" :fields mwl-fields}
   })

(defn cycle-handler
  [line-ending download-fn]
  (print "Downloading and processing cycles... ")
  (let [cycles (->> (fetch-data download-fn (:cycle tables) add-cycle-fields)
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
  [line-ending download-fn sets]
  (let [raw-cards (download-fn (-> tables :card :path))
        card-stub (fn [path] raw-cards)
        cards (->> (fetch-data card-stub (:card tables) add-card-fields)
                   (clean-card-text)
                   (cards->map :id))
        raw-set-cards (fetch-data card-stub
                                  (:set-card tables)
                                  (partial add-set-card-fields cards (cards->map sets)))]
    (println "Saving edn/cards")
    (doseq [[path card] cards
            :let [path (str "edn/cards/" path ".edn")]]
      (io/make-parents path)
      (spit path (str (zp/zprint-str card) line-ending)))
    [cards raw-set-cards]))

(defn set-cards-handler
  [line-ending raw-set-cards]
  (let [set-cards (sort-and-group-set-cards raw-set-cards)]
    (println "Saving edn/set-cards")
    (doseq [[path set-card] set-cards
            :let [path (str "edn/set-cards/" path ".edn")]]
      (io/make-parents path)
      (spit path (str (zp/zprint-str set-card) line-ending)))
    set-cards))

(defn mwl-handler
  [line-ending download-fn raw-set-cards]
  (print "Downloading and processing mwls... ")
  (let [mwls (fetch-data download-fn
                         (:mwl tables)
                         (partial convert-mwl
                                  (cards->map raw-set-cards)))]
    (let [path (str "edn/mwls.edn")]
      (io/make-parents path)
      (println "Saving" path)
      (spit path (str (zp/zprint-str (into [] mwls)) line-ending)))
    mwls))

(defn download-from-nrdb
  [& args]
  (println "args" args)
  (let [line-ending "\n"
        use-local (some #{"--local"} args)
        localpath (first (remove #(and % (str/starts-with? % "--")) args))
        download-fn (if use-local
                      (partial read-local-data localpath)
                      download-nrdb-data)

        cycles (cycle-handler line-ending download-fn)

        sets (set-handler line-ending download-fn)

        _ (print "Downloading and processing cards... ")
        ;; So this is fucked up, because unlike the old jnet system, we need to keep
        ;; some of the old fields around for splitting between cards and set-cards.
        ;; Instead of downloading stuff twice, we can download it once, stub a dl
        ;; function to return it, and pass that in to fetch-data. ezpz
        card-download-fn (if use-local
                           (partial read-card-dir localpath)
                           download-nrdb-data)

        [cards raw-set-cards] (card-handler line-ending card-download-fn sets)

        set-cards (set-cards-handler line-ending raw-set-cards)

        ; mwls (mwl-handler line-ending download-fn raw-set-cards)
        ]

    (println "Done!")))

(comment
  (download-from-nrdb)
  ,)
