(defproject netrunner-cards-edn "1.0"
  :description "Android: Netrunner card data in edn format"
  :url "https://github.com/noahtheduke/netrunner-cards-edn"

  :license {:name "The MIT License (MIT)"
            :url "https://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [cheshire "5.8.1"]
                 [semantic-csv "0.2.1-alpha1"]
                 [http-kit "2.3.0"]
                 [zprint "0.4.10"]
                 [com.stuartsierra/dependency "1.0.0"]
                 [ubergraph "0.8.2"]
                 [medley "1.3.0"]]

  :main ^:skip-aot nr-edn.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
