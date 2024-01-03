(ns blackjack.core
  (:gen-class)
  (:require [business.business :refer :all :as b]))

(defn -main
  "Runs the game."
  []
  (b/start-game))
