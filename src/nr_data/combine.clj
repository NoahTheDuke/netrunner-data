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
        cards (data/combined-cards)
        localized-data (data/localized-data)]
    (print "Writing edn/raw_data.edn...")
    (spit (io/file "edn" "raw_data.edn")
          (into (sorted-map) (concat
                 [{:cycles (vals->vec :position cycles)}
                  {:sets (vals->vec :position sets)}
                  {:cards (vals->vec :code cards)}
                  {:formats (vals->vec :date-release formats)}
                  {:mwls (vals->vec :date-start mwls)}]
                 (into [] (map (fn [v] {(keyword (str "cards-" (key v))) (vals->vec :code (val v))})
                               localized-data)))))
    (println "Done!")))

(comment
  (combine-for-jnet)
  )
