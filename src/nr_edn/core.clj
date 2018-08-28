(ns nr-edn.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [nr-edn.dl :refer :all]))

(defn -main
  "Import data from local edn files."
  [& args]
  (try
    (let [mwls (load-data "mwls" {:id :code
                                  :date-start :date_start})
          sides (load-data "sides")
          factions (load-data "factions")
          types (load-data "types")
          subtypes (load-data "subtypes")
          cycles (load-data "cycles")
          sets (load-sets cycles)
          cards (load-cards sides factions types subtypes sets)]
      (spit (io/file "edn" "raw_data.edn")
            (sorted-map
              :mwls (vals->vec mwls)
              :cycles (vals->vec cycles)
              :sets (vals->vec sets)
              :cards (vals->vec cards)))
      (println "Generated raw_data.edn"))
    (catch Exception e
      (println "Import data failed:" (.getMessage e)))))
