(ns nr-edn.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [nr-edn.combine :refer [compile-for-jnet]]
            [nr-edn.transform :refer [update-from-nrdb]]))

(defn -main
  [& args]
  (try
    (case (first args)
      nil (do (update-from-nrdb)
              (compile-for-jnet))
      "jnet" (compile-for-jnet)
      "nrdb" (update-from-nrdb)
      (println "You didn't choose correctly, fool."))
    (catch Exception e
      (println "Import data failed:" (.getMessage e)))))
