#!/usr/bin/env bb

(ns move
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn copy-file [source-path dest-path]
  (io/copy (io/file source-path) (io/file dest-path)))

(def paths (->> (io/file "edn/set-cards/system-gateway.edn")
                (slurp)
                (edn/read-string)
                (mapv :card-id)))

(println paths)

(doseq [path paths]
  (copy-file
    (str "/Users/noah/Personal/netrunner-data-future/edn/cards/" path ".edn")
    (str "/Users/noah/Personal/netrunner-data/edn/cards/" path ".edn")))
