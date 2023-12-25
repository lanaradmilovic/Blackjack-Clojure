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
(defn read-card-from-keyboard
  "Reads player's and dealer's cards from the keyboard input.
  Prompts the user to enter the face and suit of each card."
  []
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
  "Reads new card from the keyboard input."
  []
  (println "Face: ")
  (flush)
  (let [face (read-line)]
    (println "Suit: ")
    (flush)
    (let [suit (read-line)]
      [face suit])))

(defn add-new-player-card
  "Adds a new card to the player's starting hand."
  [hand new-card]
  (assoc hand (keyword (str "card-" (inc (count hand)))) new-card))

(defn add-new-player-card!
  "Atomically adds a new card to the player's starting hand using 'add-new-player-card'."
  [hand new-card]
  (swap! hand add-new-player-card new-card))

(defn add-new-current-card
  "Adds a new card to the current game session cards."
  [game-session new-card]
  (update game-session :player-cards (fn [existing-cards] (conj existing-cards new-card))))

(defn add-new-current-card!
  "Atomically adds a new card to the current game session cards using 'add-new-current-card'."
  [game-session new-card]
  (swap! game-session add-new-current-card new-card))

(defn add-both
  "Adds a new card to both the player's starting hand and the current game session cards."
  [game-session player-hand]
  (let [new-card (read-new-card)]
    (add-new-player-card! player-hand new-card)
    (add-new-current-card! game-session new-card)))

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
  "Returns player cards with converted value by invoking 'convert-card-value'."
  (loop [i 1 n (count hand) values hand]
    (if (> i n)
      values
      (recur (inc i) n (assoc-in values [(keyword (str "card-" i)) :value] (convert-card-value hand i))))))
(defn dealer-values-for-cheat-sheet
  [hand]
  "Returns dealer card with converted value by invoking 'convert-card-value'."
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


; za generisanje vrednosti za cheat-sheet na osnovu atoma
(defn update-player-card-value
  "Updates the value of a specific player card in the hand."
  [hand card-num new-v]
  (let [key (keyword (str "card-" card-num))]
    (assoc-in hand [key :value] new-v)))

(defn update-player-card-value!
  "Atomically updates the value of a specific player card in the hand."
  [hand card-num new-v]
  (swap! hand update-player-card-value card-num new-v))

(defn player-sum
  "Calculates the sum of the values of player cards."
  [card]
  (apply + (get-player-values (player-values-for-cheat-sheet card))))


(defn get-ace-position
  "Returns a list of positions of cards with a value of 'ace'."
  [cards]
  (loop [i 1
         n (inc (count cards))
         s '()]
    (if (>= i n)
      s
      (if (= "ace" (get-player-value cards i))
        (recur (inc i) n (conj s i))
        (recur (inc i) n s)))))

(defn get-no-ace-position
  "Returns a list of positions of cards with a value other than 'ace'."
  [cards]
  (loop [i 1
         n (inc (count cards))
         s '()]
    (if (>= i n)
      s
      (if (not= "ace" (get-player-value cards i))
        (recur (inc i) n (conj s i))
        (recur (inc i) n s)))))

(defn adjust-ace-value
  "Adjusts the value of an 'ace' card based on the player's card sum.
  If the current 'ace' value is 11 and the player's card sum is over 21, the 'ace' value is changed to 1."
  [card]
  (if (and (some #(= % 11) (get-player-values (player-values-for-cheat-sheet card)))
           (> (player-sum card) 21))
    (update-player-card-value card (first (get-ace-position card)) 1)
    card))


(defn adjust-ace-value!
  "Atomically adjusts the value of an 'ace' card in the player's hand by invoking 'adjust-ace-value'."
  [card]
  (swap! card adjust-ace-value))



