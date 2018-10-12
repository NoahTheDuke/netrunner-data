(ns nr-edn.download
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.set :refer [rename-keys]]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [zprint.core :as zp]
            [nr-edn.utils :refer [slugify cards->map]]))

(def ^:const base-url "http://www.netrunnerdb.com/api/2.0/public/")
(def ^:const cgdb-image-url "https://www.cardgamedb.com/forums/uploads/an/")
(def ^:const nrdb-image-url "https://netrunnerdb.com/card_image/")

(defn parse-response
  [body]
  (json/parse-string body true))

(defn download-nrdb-data
  [path]
  (let [{:keys [status body error] :as resp} @(http/get (str base-url path))]
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
                     (string/ends-with? % ".json")))
       (map read-json-file)
       flatten
       parse-response))

(defn make-image-url
  "Create a URI to the card in CardGameDB"
  [card set]
  (if (:ffg-id set)
    (str cgdb-image-url "med_ADN" (:ffg-id set) "_" (:position card) ".png")
    (str nrdb-image-url (:code card) ".png")))

(defn get-uri
  "Figure out the card art image uri"
  [card set]
  (if (contains? card :image-url)
    (:image-url card)
    (make-image-url card set)))

(defn make-map-by-code
  "Make a map of the items in the list using the :code as the key"
  [kw cards]
  (into {} (map (juxt kw identity) cards)))

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

(defn cycle-converter
  [[k v]]
  [k (case v
       "Core Set" "Core"
       "core2" "revised-core"
       "Revised Core Set" "Revised Core"
       v)])

(def cycle-fields
  {:name cycle-converter
   :position identity
   :rotated identity
   :size identity})

(defn add-cycle-fields
  [cy]
  (assoc cy :id (slugify (:name cy))))

(def set-fields
  {:code identity
   :cycle_code (rename :cycle-id)
   :date_release (rename :date-release)
   :ffg_id (rename :ffg-id)
   :name identity
   :position identity
   :size identity})

(defn add-set-fields
  [s]
  (assoc s :id (slugify (:name s))))

(defn convert-subtypes
  [subtype]
  (when subtype
    (->> (string/split subtype #" - ")
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
   :faction_cost (rename :influence-value)
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
      (assoc :id (slugify (:title card)))))

(def set-card-fields
  {
   :code identity
   :flavor identity
   :illustrator identity
   :image_url (rename :image-url)
   :pack_code (rename :pack-code)
   :position identity
   :quantity identity
   :replaced_by (rename :replaced-by)
   :replaces identity
   :title (rename :card-id slugify)
   })

(defn add-set-card-fields
  [set-map c]
  (let [s (get set-map (:pack-code c))]
    (-> c
        (dissoc :pack-code)
        (assoc :image-url (get-uri c s)
               :set-id (:id s)))))

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
                 :id (-> mwl :name slugify keyword))
      (dissoc :code)))

(defn rotate-cards
  "Added rotation fields to cards"
  [acc [title prev curr]]
  (-> acc
      (assoc-in [prev :replaced_by] curr)
      (assoc-in [curr :replaces] prev)))

(defn if-rotated
  [[c1 c2]]
  (if (< (Integer/parseInt (:code c1))
         (Integer/parseInt (:code c2)))
    [(:title c1) (:code c1) (:code c2)]
    [(:title c1) (:code c2) (:code c1)]))

(defn rotate-and-replace-cards
  [cards]
  (->> cards
       (group-by :title)
       (filter (fn [[k v]] (>= (count v) 2)))
       vals
       (map if-rotated)
       (reduce rotate-cards (make-map-by-code :code cards))
       vals))

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

(defn update-from-nrdb
  "Import data from NetrunnerDB.
  Can accept `--local <path>` to use the `netrunner-card-json` project locally,
  otherwise pulls data from NRDB.
  Specifying `--no-card-images` will not attempt to download images for cards."
  [& args]
  (try
    (let [use-local (some #{"--local"} args)
          localpath (first (remove #(string/starts-with? % "--") args))
          download-fn (if use-local
                        (partial read-local-data localpath)
                        download-nrdb-data)
          cycles (->> (fetch-data download-fn (:cycle tables) add-cycle-fields)
                      (sort-by :position)
                      (into []))

          sets (->> (fetch-data download-fn (:set tables) add-set-fields)
                    (sort-by :date-release)
                    (into []))

          ;; So this is fucked up, because unlike the old jnet system, we need to keep
          ;; some of the old fields around for splitting between cards and set-cards.
          ;; Instead of downloading stuff twice, we can download it once, stub a dl
          ;; function to return it, and pass that in to fetch-data. ezpz
          card-download-fn (if use-local
                             (partial read-card-dir localpath)
                             download-nrdb-data)
          raw-cards (card-download-fn (-> tables :card :path))

          card-stub (fn [path] raw-cards)
          cards (make-map-by-code :id
                  (fetch-data card-stub (:card tables) add-card-fields))

          set-card-stub (fn [path] (rotate-and-replace-cards raw-cards))
          raw-set-cards (fetch-data set-card-stub
                                    (:set-card tables)
                                    (partial add-set-card-fields
                                             (make-map-by-code :code sets)))

          mwls (fetch-data download-fn
                           (:mwl tables)
                           (partial convert-mwl
                                    (make-map-by-code :code raw-set-cards)))

          set-cards (sort-and-group-set-cards raw-set-cards)]

      (let [path (str "edn/cycles.edn")]
        (io/make-parents path)
        (spit path (zp/zprint-str cycles)))

      (let [path (str "edn/sets.edn")]
        (io/make-parents path)
        (spit path (zp/zprint-str sets)))

      (doseq [[path card] cards
              :let [path (str "edn/cards/" path ".edn")]]
        (io/make-parents path)
        (spit path (zp/zprint-str card)))

      (doseq [[path set-card] set-cards
              :let [path (str "edn/set-cards/" path ".edn")]]
        (io/make-parents path)
        (spit path (zp/zprint-str set-card)))

      (let [path (str "edn/mwls.edn")]
        (io/make-parents path)
        (spit path (zp/zprint-str (into [] mwls)))))
    (catch Exception e (do
                         (println "Import data failed:" (.getMessage e))
                         (.printStackTrace e)))))
