(ns nr-data.dag
  (:require
    [clojure.string :as str]
    [ubergraph.core :as uber]))

(defn ability-costs [card]
  (when (seq (:text card))
    (->> (:text card)
         (str/split-lines)
         (filter #(str/includes? % ":"))
         (map #(-> %
                   (str/lower-case)
                   (str/replace #"<strong>" "")
                   (str/replace #"<\/strong>" "")
                   (str/replace #"\d\[credit]" "[credit]")
                   (str/replace #"x\[credit]" "[credit]")
                   (str/replace #"\d " "")
                   (str/replace #"return .*? to your grip" "return this card to your grip")
                   (str/replace #"remove .*? from the game" "remove this card from the game")
                   (str/replace #"suffer .*? damage" "suffer damage")
                   (str/replace #"tokens" "token")
                   (str/replace #"counters" "counter")
                   (str/split #":")
                   (first)
                   (str/trim)
                   (str/split % #",")
                   (map str/trim %)
                   ))
         (filter #(< 1 (count %)))
         (seq))))

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
