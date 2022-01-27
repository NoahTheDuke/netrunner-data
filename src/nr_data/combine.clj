(ns nr-data.combine
  (:require
   [clojure.java.io :as io]
   [nr-data.data :as data]
   [nr-data.utils :refer [vals->vec]]))

(defn combine-for-jnet
  [& _]
  (let [cycles (data/cycles)
        sets (data/sets)
        formats (data/formats)
        mwls (data/mwls)
        cards (data/combined-cards)]
    (print "Writing edn/raw_data.edn...")
    (spit (io/file "edn" "raw_data.edn")
          (sorted-map
            :cycles (vals->vec :position cycles)
            :sets (vals->vec :position sets)
            :cards (vals->vec :code cards)
            :formats (vals->vec :date-release formats)
            :mwls (vals->vec :date-start mwls)))
    (println "Done!")))

(comment
  (combine-for-jnet)
  )
