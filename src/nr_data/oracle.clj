(ns nr-data.oracle
  (:require
   [clojure.string :as str]
   [nr-data.data :as data]
   [nr-data.text :refer [add-stripped-text]]
   [nr-data.utils :refer [slugify]]
   [semantic-csv.core :as sc]
   [zprint.core :as zp]))

(defn convert-tags [text]
  (-> text
      (str/replace "{b}" "<strong>")
      (str/replace "{/b}" "</strong>")
      (str/replace "{i}" "<em>")
      (str/replace "{/i}" "</em>")
      (str/replace "{click}" "[click]")
      (str/replace "{c}" "[credit]")
      (str/replace "{interrupt}" "[interrupt]")
      (str/replace "{link}" "[link]")
      (str/replace "{mu}" "[mu]")
      (str/replace "{MU}" "[mu]")
      (str/replace "{recurring-credit}" "[recurring-credit]")
      (str/replace "{sub}" "[subroutine]")
      (str/replace "{trash}" "[trash]")
      (str/replace #"\{(haas-bioroid|jinteki|nbn|weyland-consortium)}" "[$1]")
      (str/replace "\\n" "\n")))

(defn load-oracle-csv []
  (->> (sc/slurp-csv "oracle.csv")
       (map #(-> %
                 (assoc :name (slugify (:CardName %)))
                 (dissoc :CardName)))
       (map #(-> %
                 (assoc :text (convert-tags (:CurrentOfficialText %)))
                 (dissoc :CurrentOfficialText)))
       (map (juxt :name :text))
       (into {})))

(defn save-oracle-cards [oracle-cards raw-cards]
  (doseq [[id text] oracle-cards
          :let [card (-> (get raw-cards id)
                         (assoc :text text)
                         (add-stripped-text))]]
    (if (:id card)
      (spit (str "edn/cards/" (:id card) ".edn")
            (str (zp/zprint-str card) "\n"))
      (prn card))))

(comment
  (save-oracle-cards (load-oracle-csv) (data/raw-cards))
  )
