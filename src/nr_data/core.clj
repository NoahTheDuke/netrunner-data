(ns nr-data.core
  (:require
   [nr-data.combine :refer [combine-for-jnet]]
   [nr-data.json :refer [convert-to-json]]))

(defn run-everything []
  (combine-for-jnet)
  (convert-to-json))

(comment
  (run-everything)
  )
