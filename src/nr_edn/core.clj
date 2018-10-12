(ns nr-edn.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [zprint.core :as zp]
            [nr-edn.combine :refer [compile-for-jnet]]
            [nr-edn.download :refer [update-from-nrdb]]))

(defn -main
  [& args]
  (zp/set-options!
    {:style :community
     :map {:comma? false
           :force-nl? true}
     :width 1000})

  (try
    (case (first args)
      nil (do (update-from-nrdb)
              (compile-for-jnet))
      "jnet" (compile-for-jnet)
      "nrdb" (update-from-nrdb)
      (println "You didn't choose correctly, fool."))
    (catch Exception e
      (println "Import data failed:" (.getMessage e)))))
