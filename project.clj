(defproject simple-clojure-app "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [org.clojure/core.async "0.4.474"]
                 [org.clojure/java.jdbc "0.7.6"]
                 [com.novemberain/langohr "5.0.0"]
                 [compojure "1.6.1"]
                 [clj-time "0.14.4"]
                 ;; [clj-http "3.9.0"] ;; make request
                 [ring "1.7.0-RC1"]
                 [com.taoensso/timbre "4.10.0"]
                 [com.taoensso/sente "1.12.0"]
                 [com.taoensso/carmine "2.18.1"]
                 [com.taoensso/tufte "2.0.1"]
                 [com.taoensso/nippy "2.14.0"] ;; serialization
                 [mount "0.1.11"]
                 [org.clojure/data.priority-map "0.0.9"]
                 ]
  :plugins [
;;            [lein-pprint         "1.1.2"]
;;            [lein-ancient        "0.6.10"]
;;            [com.cemerick/austin "0.1.6"]
;;            [cider/cider-nrepl   "0.17.0"] ;; Optional, for use with Emacs

            [cider/cider-nrepl "0.18.0-snapshot"]
            ]
  :jvm-opts ["--add-modules" "java.xml.bind"] ;; for Java 10
  :main ^:skip-aot simple-clojure-app.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
