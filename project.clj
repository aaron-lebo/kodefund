(defproject codestarter "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [compojure "1.0.4"]
                 [ring-anti-forgery "0.1.3"]
                 [noir "1.2.2"]
                 [korma "0.3.0-beta7"]
                 [postgresql "9.1-901.jdbc4"]
                 [org.mozilla/rhino "1.7R3"]
                 [com.cemerick/friend "0.0.8"]
                 [clj-time "0.4.2"]
                 [org.ocpsoft.prettytime/prettytime "1.0.8.Final"]
                 [rome "1.0"]
                 [org.apache.commons/commons-email "1.2"]
                 [clj-oauth "1.3.1-SNAPSHOT" :exclusions [clj-http]]
                 [clojure-twitter "1.2.6-SNAPSHOT" :exclusions [clj-http]]
                 [oauthentic "0.0.6"]]
  :plugins [[lein-ring "0.7.0"]]
  :ring {:handler codestarter.core/app
         :adapter {:ssl? true :keystore "keystore" :key-password "password"}})