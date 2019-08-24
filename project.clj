(defproject aflscraper "0.1.0-SNAPSHOT"
  :description "A tool for scraping data from afltables.com"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [clj-http "3.10.0"]
                 [incanter "1.9.3"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/data.json "0.2.6"]
                 [com.rpl/specter "1.1.2"]
                 [hickory/hickory "0.7.1"]]
  :main ^:skip-aot aflscraper.core
  :repl-options {:init-ns aflscraper.core})
