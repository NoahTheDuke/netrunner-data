(ns nr-data.quotes
  (:require [clojure.string :as string]
            [semantic-csv.core :as sc]
            [medley.core :refer :all]
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



(comment (get (id-merger "jnet-corp-quotes.csv") "Asa Group: Security Through Vigilance"))
(comment (get (id-merger "jnet-runner-quotes.csv") "Sunny Lebeau: Security Specialist"))


(defn build-quotes []
  (let [
        corp (id-merger "jnet-corp-quotes.csv")
        runner (id-merger "jnet-runner-quotes.csv")
        ]

    (spit "quotes/quotes-corp.edn" (str (zp/zprint-str corp) "\n"))
    (spit "quotes/quotes-runner.edn" (str (zp/zprint-str runner) "\n"))))
