(defproject simple-clojure-app "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [org.clojure/core.async "0.3.441"]
                 [com.taoensso/timbre "4.8.0"]            ;; Profiling
                 [com.taoensso/tufte "1.1.1"]
                 [com.taoensso/encore "2.91.0"]
                 [com.taoensso/sente "1.11.0"]            ;; for websocket
                 [mount "0.1.11"]
                 ]
  :plugins [[lein-pprint         "1.1.2"]
            [lein-ancient        "0.6.10"]
            [com.cemerick/austin "0.1.6"]
            [lein-cljsbuild      "1.1.4"]
            [cider/cider-nrepl   "0.12.0"] ; Optional, for use with Emacs
            ]
  :jvm-opts ["--add-modules" "java.xml.bind"]
  :main ^:skip-aot simple-clojure-app.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
