(ns nr-data.core
  (:require
    [nr-data.csv :refer [build-from-csv]]
    [nr-data.combine :refer [combine-for-jnet]]
    [nr-data.download :refer [download-from-nrdb]]
    [nr-data.json :refer [convert-to-json]]))

(defn run-everything []
  (combine-for-jnet)
  (convert-to-json))

(comment
  (run-everything)
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (greet {:name (first args)}))
