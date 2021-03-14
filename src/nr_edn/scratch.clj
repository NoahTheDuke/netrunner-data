(ns nr-edn.scratch
  (:require [clojure.string :as s]
            [nr-edn.combine :refer :all]
            [nr-edn.json :refer [convert-to-json]]
            [nr-edn.utils :refer [cards->map normalize-text]]
            [zprint.core :as zp]
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
         (s/split-lines)
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
         (s/lower-case)
         (re-find #"spend.*?\[click].*?\[credit]"))))

(defn quantify [n s]
  (if (or (= "1" n)
          (= "-1" n))
    (str n " " s)
    (str n " " s "s")))

(defn clean-text
  [card]
  (some-> (not-empty (:text card))
          (s/replace #"</?(strong|trace|errata|em|i|ul)>" "")
          (s/replace #"</li>" "")
          (s/replace #"<li>" " * ")
          (s/replace #"\n" " ")
          (s/replace #"\[click\]\[click\]\[click\]" "click click click")
          (s/replace #"\[click\]\[click\]" "click click")
          (s/replace #"\[click\]" "click")
          (s/replace #"(\d+|X)\s*\[credit\]" #(quantify (%1 1) "credit"))
          (s/replace #"(\d+|X)\s*\[recurring-credit]" #(quantify (%1 1) "recurring credit"))
          (s/replace #"(\d+|X)\s*\[mu]" "$1 mu")
          (s/replace #"(\d+|X)\s*\[link]" "$1 link")
          (s/replace #"\[link\]" "link")
          (s/replace #"\[subroutine]\s*" "Subroutine ")
          (s/replace #"\[trash]" "trash")
          (s/replace #"\[interrupt]" "Interrupt")
          (s/replace #"\[(anarch)]|\[(criminal)]|\[(shaper)]" "$1")
          (s/replace #"\[(haas-bioroid)]|\[(jinteki)]|\[(nbn)]|\[(weyland-consortium)]" "$1")))

(defn add-stripped-text [card]
  (if-let [plain-text (normalize-text (clean-text card))]
    (assoc card :stripped-text plain-text)
    card))

(defn add-stripped-title [card]
  (->> (:title card)
       (normalize-text)
       (assoc card :stripped-title)))

(defn clean-card-text [cards]
  (->> cards
       (map add-stripped-text)
       (map add-stripped-title)))

(defn save-cards [cards]
  (doseq [card (clean-card-text cards)]
    (spit (str "edn/cards/" (:id card) ".edn")
          (str (zp/zprint-str card) "\n"))))

(def c (vals (raw-cards)))

(comment
  (save-cards c)
  (convert-to-json)
  )
