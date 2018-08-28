(ns nr-edn.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [zprint.core :as zp]
            [nr-edn.nrdb :refer :all]))

(defn -main
  "Import data from NetrunnerDB.
  Can accept `--local <path>` to use the `netrunner-card-json` project locally,
  otherwise pulls data from NRDB.
  Specifying `--no-card-images` will not attempt to download images for cards."
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
          cards (load-cards sides factions types subtypes sets)

          ; mwls (fetch-data download-fn (:mwls tables))
          ; cycles (fetch-data download-fn (:cycles tables))
          ; sets (fetch-data download-fn (:sets tables) (partial add-set-fields cycles))
          ; factions (fetch-data download-fn (:factions tables))
          ; subtypes (fetch-data download-fn (:subtypes tables))
          ; cards (fetch-cards card-download-fn (:cards tables)
          ;                    sets factions subtypes
          ;                    (not (some #{"--no-card-images"} args)))
          ]
      (spit (io/file "edn" "raw_data.edn")
            (zp/zprint-str (sorted-map
                             :mwls (vals mwls)
                             :cycles (vals cycles)
                             :sets (vals sets)
                             :cards (vals cards))
                           {:style :community
                            :map {:comma? false
                                  :force-nl? true}
                            :width 450}))
      (println "Spitted file"))
    (catch Exception e (do
                         (println "Import data failed:" (.getMessage e))))))
