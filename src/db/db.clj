(ns db.db
  (:require [next.jdbc :as jdbc]))

(def db
  {:dbtype "h2"
   :dbname "bj"
   :user "lr"
   :password ""
   :subprotocol "h2:tcp"
   :subname "//localhost:9000/bj;webAllowOthers=true;
             AUTO_SERVER=TRUE"
   })

(def datasource (jdbc/get-datasource db))

(defn get-connection
  []
  (jdbc/get-connection datasource))

(defn execute-script
  []
  (let [script (slurp "resources/blackjack.sql")]
    (jdbc/with-transaction [tx (get-connection)]
                           (try
                             (jdbc/execute! tx [script])
                             (catch Exception ex
                               (println (.getMessage ex)))))))
;(execute-script)

(defn insert-game
  [sum-player-hand dealer-hand move]
  (jdbc/execute! (get-connection)
                 ["INSERT INTO game_session (sum_player_hand, dealer_card, move) VALUES (?,?,?)"
                  sum-player-hand dealer-hand move]))
(defn select-game
  []
  (jdbc/execute! (get-connection) ["SELECT * FROM game_session"]))

