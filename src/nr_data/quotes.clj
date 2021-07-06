(ns nr-data.quotes
  (:require
   [clojure.string :as string]
   [medley.core :refer [filter-vals map-vals]]
   [semantic-csv.core :as sc]
   [zprint.core :as zp]))

(defn quote-parser
  [quotes]
  (->> quotes
       (filter-vals seq)
       (map-vals (fn [text]
                   (as-> text text
                     (string/split text #"@@")
                     (filter seq text)
                     (mapv string/trim text))))))

(comment
  (quote-parser {:a ""
                 :b "@@123@@456"
                 :c "hello@@world"
                 :d "asdf "}))

(defn id-merger
  [path]
  (into {}
        (for [c (sc/slurp-csv path :keyify false)
              :let [k (get c "")
                    v (dissoc c "")]]
          [k (quote-parser v)])))



(comment (get (id-merger "quotes/quotes-corp.csv") "Asa Group: Security Through Vigilance"))
(comment (get (id-merger "quotes/quotes-runner.csv") "Sunny Lebeau: Security Specialist"))


(defn build-quotes [& _]
  (let [
        corp (id-merger "quotes/quotes-corp.csv")
        runner (id-merger "quotes/quotes-runner.csv")
        ]

    (spit "quotes/quotes-corp.edn" (str (zp/zprint-str corp) "\n"))
    (spit "quotes/quotes-runner.edn" (str (zp/zprint-str runner) "\n"))))

(comment
  (build-quotes))
