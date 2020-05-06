(ns nr-edn.quotes
  (:require [clojure.string :as string]
            [semantic-csv.core :as sc]
            [medley.core :refer :all]
            [zprint.core :as zp]))

(defn quote-parser
  [quotes]
  (->> quotes
       (filter-vals seq)
       (map-vals (fn [text] (string/split text #"@@")))))

(defn id-merger
  [path]
  (into {}
        (for [c (sc/slurp-csv path :keyify false)
              :let [k (get c "Title")
                    v (dissoc c "Title")]]
          [k (quote-parser v)])))

(def corp (id-merger "jnet-corp-quotes.csv"))

(comment (get corp "The Foundry: Refining the Process"))

(def runner (id-merger "jnet-runner-quotes.csv"))

(comment (get runner "Sunny Lebeau: Security Specialist"))

(zp/set-options!
  {:style :community
   :map {:comma? false
         :force-nl? true}
   :width 88})

(spit "quotes/quotes-corp.edn" (str (zp/zprint-str corp) "\n"))

(spit "quotes/quotes-runner.edn" (str (zp/zprint-str runner) "\n"))
