(ns nr-edn.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [nr-edn.combine :refer [compile-for-jnet]]))

(defn -main
  [& args]
  (try
    (compile-for-jnet)
    (catch Exception e
      (println "Import data failed:" (.getMessage e)))))
