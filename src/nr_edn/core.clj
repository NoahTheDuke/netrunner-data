(ns nr-edn.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [zprint.core :as zp]
            [nr-edn.combine :refer [combine-for-jnet convert-to-json]]
            [nr-edn.download :refer [download-from-nrdb]]
            [nr-edn.csv :refer [build-from-csv]]))

(defn -main
  [& args]
  (zp/set-options!
    {:style :community
     :map {:comma? false
           :force-nl? true}
     :width 1000})

  (case (first args)
    nil (combine-for-jnet)
    "download" (download-from-nrdb)
    "combine" (combine-for-jnet)
    "csv" (build-from-csv)
    "json" (convert-to-json)
    (println "You didn't choose correctly, fool."))
  ; (catch Exception e
  ;   (println "Import data failed:" (.getMessage e))))
  )
