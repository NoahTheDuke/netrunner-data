(ns nr-data.utils
  (:require
   [clojure.string :as str]))

(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

(defmacro vals->vec
  ([coll]
   `(into [] (vals ~coll)))
  ([order coll]
   `(into [] (sort-by ~order (vals ~coll)))))

(defn cards->map
  ([cards] (cards->map :code cards))
  ([kw cards]
   (into {} (map (juxt kw identity) cards))))

(defn normalize-text [s]
  (some-> (not-empty s)
          (name)
          (java.text.Normalizer/normalize java.text.Normalizer$Form/NFD)
          (str/replace #"[\P{ASCII}]+" "")
          (str/trim)))

(defn slugify
  "As defined here: https://you.tools/slugify/"
  ([s] (slugify s "-"))
  ([s sep]
   (if (nil? s) ""
     (as-> s s
       (normalize-text s)
       (str/lower-case s)
       (str/split s #"[\p{Space}\p{Punct}]+")
       (filter seq s)
       (str/join sep s)))))

(defn prune-null-fields
  [card]
  (apply dissoc card (for [[k v] card :when (nil? v)] k)))

(defn quantify [n s]
  (if (or (= "1" n)
          (= "-1" n))
    (str n " " s)
    (str n " " s "s")))

(defn spend-vs-pay [card]
  (when (seq (:text card))
    (->> (:text card)
         (str/lower-case)
         (re-find #"spend.*?\[click].*?\[credit]"))))
