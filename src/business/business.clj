(ns business.business
  (:require [cheat-sheet.cheat-sheet :refer :all :as cs]))

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
(defn read-face
  "Reads the face value of a card from the user's keyboard input."
  []
  (println "Face: ")
  (flush)
  (read-line))
(defn read-suit
  "Reads the suit of a card from the user's keyboard input."
  []
  (println "Suit: ")
  (flush)
  (read-line))

(defn valid-face
  "Validates the face value entered by the user."
  [f]
  (let [f (if (<= (count f) 2)
            (Integer/parseInt f)
            f)]
    (if (some #{f} values) f
                           (do (println "The value must be one of the following: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 'ace', 'jack', 'queen', 'king'.")
                               (valid-face (read-face))))))
(defn valid-suit
  "Validates the suit entered by the user."
  [s]
  (if (some #{s} suits) s
                        (do (println "The suit must be one of the following: 'club', 'heart', 'spade', 'diamond'.")
                            (valid-suit (read-suit)))))

(defn read-card-from-keyboard
  "Reads player's and dealer's cards from the keyboard input.
  Prompts the user to enter the face and suit of each card."
  []
  (loop [i 1 player-cards '() dealer-card '()]
    (if (< i 3)
      (do (println "Enter YOUR " i ". card: ")
          (let [face (read-face)]
            (valid-face face)
            (let [suit (read-suit)]
              (valid-suit suit)
              (recur (inc i) (conj player-cards (generate-card face suit)) dealer-card))))
      (if (= i 3)
        (do (println "Enter DEALER'S covered card: ")
            (flush)
            (let [face (read-face)]
              (valid-face face)
              (let [suit (read-suit)]
                (valid-suit suit)
                (recur (inc i) player-cards (conj dealer-card (generate-card face suit))))))
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
  (try (let [new-card (read-new-card)]
         (add-new-player-card! player-hand new-card)
         (add-new-current-card! game-session new-card))
       (catch ClassCastException e
         (println "Expected an atom."))))

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
        (catch NumberFormatException e
          (println "Invalid card value.")))
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


(defn update-player-card-value
  "Updates the value of a specific player card in the hand.
  Returns nil if the card number does not exist."
  [hand card-num new-v]
  (let [key (keyword (str "card-" card-num))]
    (if (contains? hand key)
      (assoc-in hand [key :value] new-v)
      nil)))


(defn update-player-card-value!
  "Atomically updates the value of a specific player card in the hand."
  [hand card-num new-v]
  (try (swap! hand update-player-card-value card-num new-v)
       (catch Exception e
         (println "Expected an atom."))))

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

(defn adjust-ace-value!
  "Atomically adjusts the value of an 'ace' card based on the player's card sum.
  If the current 'ace' value is 11 and the player's card sum is over 21, the 'ace' value is changed to 1."
  [card]
  (cond
    (= 0 (count (get-ace-position @card))) card            ; No 'ace' cards.
    (= (count (get-ace-position @card)) 1) (if (and (some #(= % 11) (get-player-values (player-values-for-cheat-sheet @card))) ; One 'ace' card.
                                                    (> (player-sum @card) 21))
                                             (update-player-card-value! card (first (get-ace-position @card)) 1)
                                             card)
    (> (count (get-ace-position @card)) 1) (if (and (some #(= % 11) (get-player-values (player-values-for-cheat-sheet @card))) ; More than one 'ace' card.
                                                    (> (player-sum @card) 21))
                                             (do (update-player-card-value! card (first (get-ace-position @card)) 1)
                                                 (adjust-ace-value! card))
                                             card)))

(defn between?
  "Checks if a number is between two given values."
  [n lower-bound upper-bound]
  (and (> n lower-bound) (< n upper-bound)))

(defn ace-to-A
  "Converts a card value from 'ace' to 'A' to match the cheat sheet."
  [dealer-card]
  (if (= "ace" dealer-card) "A" dealer-card))

(defn recommend-move
  "Recommends a move based on the current game state, cheat sheet, and player's hand."
  [cheat-sheet current-cards player-hand]
  (let [current-player-sum (player-sum (adjust-ace-value! (atom player-hand)))
        dealer-card (ace-to-A (get-dealer-value current-cards))
        player-card (str current-player-sum)]

    (cond
      (= current-player-sum 21) (println "Black Jack!")     ; Player has a Blackjack.
      (> (count player-hand) 2) (cond
                                  (< current-player-sum 9) (cs/get-move cheat-sheet "8" dealer-card) ; Player has more than 2 cards and current sum is less than 9.
                                  (between? current-player-sum 8 18) (cs/get-move cheat-sheet player-card dealer-card) ; Player has more than 2 cards and current sum is between 8 and 18.
                                  (> current-player-sum 17) (cs/get-move cheat-sheet "17" dealer-card)) ; Player has more than 2 cards and current sum is greater than 17.
      (and (between? current-player-sum 8 18)
           (not= (get-player-value player-hand 1)
                 (get-player-value player-hand 2))
           (= (count player-hand)
              (count (get-no-ace-position player-hand)))) (cs/get-move cheat-sheet player-card dealer-card) ; Player has 2 cards, and their sum is between 8 and 18, and both cards are different, and no Ace in the hand.
      (= 2 (count (get-ace-position player-hand))) (cs/get-move cheat-sheet "AA" dealer-card) ; Player has 2 Aces.
      (= 1 (count (get-ace-position player-hand)))
      (let [no-ace-card-num (first (get-no-ace-position player-hand))
            player-card (str "A" (get-player-value player-hand no-ace-card-num))]
        (cs/get-move cheat-sheet player-card dealer-card))  ; Player has 1 Ace.
      (and (= (get-player-value player-hand 1)
              (get-player-value player-hand 2))
           (not= (get-player-value player-hand 1)
                 "ace"))
      (let [player-card (str (get-player-value player-hand 1) (get-player-value player-hand 2))] (cs/get-move cheat-sheet player-card dealer-card)) ; Player has 2 cards of the same value (not Ace).
      (< current-player-sum 9) (cs/get-move cheat-sheet "8" dealer-card) ; Player has 2 cards, and current sum is less than 9.
      (> current-player-sum 17) (cs/get-move cheat-sheet "17" dealer-card) ; Player has 2 cards, and current sum is greater than 17.

      :else "Not covered in cheat sheet.")))                ; Default case, not covered by the cheat sheet.




