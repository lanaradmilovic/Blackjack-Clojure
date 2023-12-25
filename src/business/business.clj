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

(defn add-new-player-card
  "Adds new card to player's starting hand."
  [c new-c]
  (assoc c (keyword (str "card-" (inc (count c)))) new-c))
(defn add-new-player-card!
  [c new-c]
  (swap! c add-new-player-card {:value (nth new-c 0) :suit (nth new-c 1)}))
(defn add-new-current-card
  "Adds new card to current game session cards."
  [c new-c]
  (update c :player-cards (fn
                               [existing-cards]
                               (conj existing-cards new-c))))

(defn add-new-current-card!
  [c new-c]
  (swap! c add-new-current-card {:value (nth new-c 0) :suit (nth new-c 1)}))

(defn add-both
  "Adds new card to both the player's starting hand and current game session cards."
  [c p]
  (let [new-c (read-new-card)]
    (add-new-player-card! p new-c)
    (add-new-current-card! c new-c)))

(defn get-player-value
  [c n]
  (:value (get-nth-player-card c n)))

(defn get-dealer-value
  [c]
  (:value (get-dealer-card c)))

(defn card-value
  "Converts a card's value into an Integer according to Blackjack rules."
  [value]
  (case (clojure.string/trim (clojure.string/lower-case value))
    "jack" 10
    "queen" 10
    "king" 10
    "ace" 11
    (if (string? value)
      (try
        (Integer/parseInt value)
        (catch NumberFormatException _ (throw (IllegalArgumentException. "Invalid card value."))))
      value)))

(defn convert-card-value
  ([c n]
   (let [value (get-player-value c n)]
             (card-value value)))
  ([c]
   (let [value (get-dealer-value c)]
     (card-value value))))

(defn player-values-for-cheat-sheet
  [hand]
  "Returns player cards with converted value by calling 'convert-card-value'."
  (loop [i 1 n (count hand) values hand]
    (if (> i n)
      values
      (recur (inc i) n (assoc-in values [(keyword (str "card-" i)) :value] (convert-card-value hand i))))))
(defn dealer-values-for-cheat-sheet
  [hand]
  "Returns dealer card with converted value by calling 'convert-card-value'."
  (assoc (get-dealer-card hand) :value (convert-card-value hand)))

(defn get-player-values
  [hand]
  "Returns list of original player cards values."
  (loop [i 1 n (count hand) values '()]
    (if (> i n)
      values
      (recur (inc i) n (conj values (get-player-value hand i))))))
(defn get-all-values
  [hand]
  "Returns list of both dealer and player original cards values."
  (concat (map #(get % :value) (:player-cards hand))
          [(str (:value (first (:dealer-card hand))))]))
