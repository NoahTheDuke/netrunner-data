(ns nr-edn.scratch
  (:require [clojure.string :as s]
            [nr-edn.combine :refer :all]
            [nr-edn.utils :refer [cards->map]]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

(defn mwls [] (load-data "mwls" {:id :code}))
(defn sides [] (load-data "sides"))
(defn factions [] (load-data "factions"))
(defn types [] (load-data "types"))
(defn subtypes [] (load-data "subtypes"))
(defn formats [] (load-data "formats"))
(defn cycles [] (load-data "cycles"))
(defn sets [] (load-sets (cycles)))
(defn combined-card [] (load-cards (sides) (factions) (types) (subtypes) (sets) (formats) (mwls)))

(defn set-cards [] (load-edn-from-dir "edn/set-cards"))
(defn raw-cards [] (cards->map :id (load-edn-from-dir "edn/cards")))
(defn cards [] (merge-sets-and-cards (set-cards) (raw-cards)))
(defn card->formats [] (generate-formats (sets) (cards) (formats) (mwls)))

(defn ability-costs [card]
  (when (seq (:text card))
    (->> (:text card)
         s/split-lines
         (filter #(s/includes? % ":"))
         (map s/lower-case)
         (map #(s/replace % #"<strong>" ""))
         (map #(s/replace % #"<\/strong>" ""))
         (map #(s/replace % #"\d\[credit]" "[credit]"))
         (map #(s/replace % #"x\[credit]" "[credit]"))
         (map #(s/replace % #"\d " ""))
         (map #(s/replace % #"return .*? to your grip" "return this card to your grip"))
         (map #(s/replace % #"remove .*? from the game" "remove this card from the game"))
         (map #(s/replace % #"suffer .*? damage" "suffer damage"))
         (map #(s/replace % #"tokens" "token"))
         (map #(s/replace % #"counters" "counter"))
         (map #(s/split % #":"))
         (map first)
         (map s/trim)
         (map #(s/split % #","))
         (map #(map s/trim %))
         (filter #(< 1 (count %)))
         seq)))

(defn multi-costs [cards]
  (mapcat identity (keep ability-costs cards)))

(defn make-directed-graph [graph costs]
  (reduce
    (fn [graph [c1 c2]] (uber/add-directed-edges graph [c1 c2]))
    graph
    (into #{} (for [cost costs
                    [c1 c2] (partition 2 1 cost)
                    :when (not= c1 c2)]
                [c1 c2]))))

(defn spend-vs-pay [card]
  (when (seq (:text card))
    (->> (:text card)
         s/lower-case
         (re-find #"spend.*?\[click].*?\[credit]"))))
