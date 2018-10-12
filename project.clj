(defproject netrunner-cards-edn "1.0"
  :description "Android: Netrunner card data in edn format"
  :url "https://github.com/noahtheduke/netrunner-cards-edn"

  :license {:name "The MIT License (MIT)"
            :url "https://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [cheshire "5.6.3"]
                 [http-kit "2.3.0"]
                 [zprint "0.4.10"]]

  :main ^:skip-aot nr-edn.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
