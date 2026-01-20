(ns nr-data.text
  (:require
   [clojure.string :as str]
   [nr-data.data :refer [raw-cards]]
   [nr-data.utils :refer [normalize-text quantify apply-to-faces-too]]
   [zprint.core :as zp]))

(defn clean-text
  [card]
  (some-> (not-empty (:text card))
          (str/replace #"</?(strong|trace|errata|em|i|ul)>" "")
          (str/replace #"</li>" "")
          (str/replace #"<li>" " * ")
          (str/replace #"\n" " ")
          (str/replace "–" "-")
          (str/replace "→" "->")
          (str/replace #"\[click\]\[click\]\[click\]" "click click click")
          (str/replace #"\[click\]\[click\]" "click click")
          (str/replace #"\[click\]" "click")
          (str/replace #"(\d+|X)\s*\[credit\]" #(quantify (%1 1) "credit"))
          (str/replace #"(\d+|X)\s*\[recurring-credit]" #(quantify (%1 1) "recurring credit"))
          (str/replace #"(\d+|X)\s*\[mu]" "$1 mu")
          (str/replace #"(\d+|X)\s*\[link]" "$1 link")
          (str/replace #"\[link\]" "link")
          (str/replace #"\[subroutine]\s*" "Subroutine ")
          (str/replace #"\[trash]" "trash")
          (str/replace #"\[interrupt]" "Interrupt")
          (str/replace #"\[(anarch|criminal|shaper)]" "$1")
          (str/replace #"\[(haas-bioroid|jinteki|nbn|weyland-consortium)]" "$1")))

(defn add-stripped-text [card]
  (if-let [plain-text (normalize-text (clean-text card))]
    (assoc card :stripped-text plain-text)
    card))

(defn add-stripped-title [card]
  (->> (:title card)
       (normalize-text)
       (assoc card :stripped-title)))

(defn add-stripped-card-text [cards]
  (->> cards
       (map (apply-to-faces-too add-stripped-text))
       (map (apply-to-faces-too add-stripped-title))))

(defn fix-arrows [card]
  (if (:text card)
    (assoc card :text (-> (:text card)
                          (str/replace " > " " → ")
                          (str/replace " -> " " → ")
                          ))
    card))

(defn clean-card-text [cards]
  (->> cards
       (map fix-arrows)
       (add-stripped-card-text)))

(defn save-cards [cards]
  (doseq [card (clean-card-text cards)]
    (spit (str "edn/cards/" (:id card) ".edn")
          (str (zp/zprint-str card) "\n"))))

(comment
  (save-cards (vals (raw-cards)))
  )
