(defproject artengine "0.0.1"
  :description "vector drawing program"
  :main artengine.core
  :extra-classpath-dirs ~(.listFiles (File. "jogamp"))
  :dependencies [[org.clojure/clojure "1.3.0"]
		 [seesaw "1.4.0"]])
