(ns nr-edn.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [zprint.core :as zp]
            [nr-edn.combine :refer [combine-for-jnet]]
            [nr-edn.download :refer [download-from-nrdb]]))

(defn -main
  [& args]
  (zp/set-options!
    {:style :community
     :map {:comma? false
           :force-nl? true}
     :width 1000})

  (try
    (case (first args)
      nil (do (download-from-nrdb)
              (combine-for-jnet))
      "download" (download-from-nrdb)
      "combine" (combine-for-jnet)
      (println "You didn't choose correctly, fool."))
    (catch Exception e
      (println "Import data failed:" (.getMessage e)))))
