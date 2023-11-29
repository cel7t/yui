(defproject yui "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clj-http "3.12.3"]
                 [clojure.java-time "1.3.0"]
                 [cheshire "5.10.2"]
                 [com.github.discljord/discljord "1.3.1"]]
  :repl-options {:init-ns yui.core}
  :main yui.core
  :jar-name "yui.jar"
  :uberjar-name "yui-standalone.jar")
