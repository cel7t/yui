(defproject yui "0.1.0-SNAPSHOT"
  :description "A Discord Bot written in Clojure!"
  :url "https://github.com/cel7t/yui"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clj-time "0.15.2"]
                 [clj-http "3.12.3"]
                 [cheshire "5.10.2"]
                 [com.github.discljord/discljord "1.3.1"]]
  :repl-options {:init-ns yui.core}
  :main yui.core
  :jar-name "yui.jar"
  :uberjar-name "yui-standalone.jar")
