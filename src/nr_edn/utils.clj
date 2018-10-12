(ns nr-edn.utils
  (:require [clojure.string :as string]))

(defmacro vals->vec
  ([coll]
   `(into [] (vals ~coll)))
  ([order coll]
   `(into [] (sort-by ~order (vals ~coll)))))

(defn cards->map
  ([cards] (cards->map :code cards))
  ([kw cards]
   (into {} (map (juxt kw identity) cards))))

(defn slugify
  "As defined here: https://you.tools/slugify/"
  ([s] (slugify s "-"))
  ([s sep]
   (if (nil? s) ""
     (as-> s s
       (name s)
       (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFD)
       (string/replace s #"[\P{ASCII}]+" "")
       (string/lower-case s)
       (string/trim s)
       (string/split s #"[\p{Space}\p{Punct}]+")
       (filter seq s)
       (string/join sep s)))))
