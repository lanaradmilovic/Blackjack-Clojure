(ns business.business)

(def suits ["club" "heart" "spade" "diamond"])
(def numbers (range 1 11))
(def faces ["ace" "jack" "queen" "king"])
(def values (concat numbers faces))

(defn generate-card
  [value suit]
  {:value value :suit suit})
(def initial-deck
  (for [v values s suits]
    (generate-card v s)))
(defn read-card-from-keyboard []
  (loop [i 1
         player-cards []
         dealer-card []]
    (if (< i 3)
      (do (println "Enter YOUR " i ". card: ")
          (println "Face: ")
          (flush)
          (let [face (read-line)]
            (println "Suit: ")
            (flush)
            (let [suit (read-line)]
              (recur (inc i) (conj player-cards (generate-card face suit)) dealer-card))))
      (if (= i 3)
        (do (println "Enter DEALER'S covered card: ")
            (flush)
            (println "Face: ")
            (flush)
            (let [face (read-line)]
              (println "Suit: ")
              (flush)
              (let [suit (read-line)]
                (recur (inc i) player-cards (conj dealer-card (generate-card face suit)))))
            )
        {:player-cards player-cards
         :dealer-card  dealer-card}))))

(def current-cards (atom (read-card-from-keyboard)))

(def player-starting-hand (atom {:card-1 (first (get @current-cards :player-cards))
                                 :card-2 (second (get @current-cards :player-cards))}))
(defn get-nth-player-card
  [card n]
  ((keyword (str "card-" n)) card))

(defn get-dealer-card
  [card]
  (let [dealer-card (get card :dealer-card)]
    (first dealer-card)))

(defn read-new-card
  []
  (println "Face: ")
  (flush)
  (let [face (read-line)]
    (println "Suit: ")
    (flush)
    (let [suit (read-line)]
      [face suit])))

(defn add-new-card
  [c new-c]
  (assoc c (keyword (str "card-" (inc (count c)))) new-c))
(defn add-new-card-atom
  [c new-c]
  (swap! c add-new-card {:value (nth new-c 0) :suit (nth new-c 1)}))
