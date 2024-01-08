(ns business.business
  (:require [cheat-sheet.cheat-sheet :refer :all :as cs]
            [db.db :refer :all :as db]))

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
  (println "Face: (2 3 4 5 6 7 8 9 10 'ace' 'jack' 'queen' 'king')")
  (flush)
  (read-line))
(defn read-suit
  "Reads the suit of a card from the user's keyboard input."
  []
  (println "Suit: ('club' 'heart' 'spade' 'diamond')")
  (flush)
  (read-line))

(defn valid-face
  "Validates the face value entered by the user."
  [f]
  (let [f (if (<= (count f) 2)
            (Integer/parseInt f)
            f)]
    (if (some #{f} values) f
                           (do
                             (println "The value must be one of the following: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 'ace', 'jack', 'queen', 'king'.")
                             (valid-face (read-face))))))
(defn valid-suit
  "Validates the suit entered by the user."
  [s]
  (if (some #{s} suits) s
                        (do
                          (println "The suit must be one of the following: 'club', 'heart', 'spade', 'diamond'.")
                          (valid-suit (read-suit)))))

(defn read-card-from-keyboard
  "Reads player's and dealer's cards from the keyboard input.
  Prompts the user to enter the face and suit of each card."
  []
  (loop [i 1 player-cards '() dealer-card '()]
    (if (< i 3)
      (do
        (println "Enter YOUR " i ". card: ")
        (let [face (read-face)]
          (valid-face face)
          (let [suit (read-suit)]
            (valid-suit suit)
            (recur (inc i) (conj player-cards (generate-card face suit)) dealer-card))))
      (if (= i 3)
        (do
          (println "Enter DEALER'S covered card: ")
          (flush)
          (let [face (read-face)]
            (valid-face face)
            (let [suit (read-suit)]
              (valid-suit suit)
              (recur (inc i) player-cards (conj dealer-card (generate-card face suit))))))
        {:player-cards player-cards
         :dealer-card  dealer-card}))))

;(def current-cards (atom (read-card-from-keyboard)))

;(def player-starting-hand (atom {:card-1 (first (get @current-cards :player-cards))
;                                 :card-2 (second (get @current-cards :player-cards))}))
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
      {:value face :suit suit})))

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

(defn add-both!
  "Adds a new card to both the player's starting hand and the current game session cards."
  [game-session player-hand]
  (try (let [new-card (read-new-card)]
         (add-new-player-card! player-hand new-card)
         (add-new-current-card! game-session new-card))
       (catch ClassCastException _
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
        (catch NumberFormatException _
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
       (catch Exception _
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
    (= 0 (count (get-ace-position @card))) @card            ; No 'ace' cards.
    (= (count (get-ace-position @card)) 1) (if (and (some #(= % 11) (get-player-values (player-values-for-cheat-sheet @card))) ; One 'ace' card.
                                                    (> (player-sum @card) 21))
                                             (update-player-card-value! card (first (get-ace-position @card)) 1)
                                             @card)
    (> (count (get-ace-position @card)) 1) (if (and (some #(= % 11) (get-player-values (player-values-for-cheat-sheet @card))) ; More than one 'ace' card.
                                                    (> (player-sum @card) 21))
                                             (do
                                               (update-player-card-value! card (first (get-ace-position @card)) 1)
                                               (adjust-ace-value! card))
                                             @card)))

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
      (> current-player-sum 21) "Bust!"
      (= current-player-sum 21) "Blackjack!"                ; Player has a Blackjack.
      (> (count player-hand) 2) (cond
                                  (< current-player-sum 9) (cs/get-move cheat-sheet "8" dealer-card) ; Player has more than 2 cards and current sum is less than 9.
                                  (between? current-player-sum 8 18) (cs/get-move cheat-sheet player-card dealer-card) ; Player has more than 2 cards and current sum is between 8 and 18.
                                  (and (> current-player-sum 17)
                                       (< current-player-sum 22)) (cs/get-move cheat-sheet "17" dealer-card)) ; Player has more than 2 cards, and current sum is greater than 17 and less than 22.
      (and (between? current-player-sum 8 18)
           (not= (get-player-value player-hand 1)
                 (get-player-value player-hand 2))
           (= (count player-hand)
              (count (get-no-ace-position player-hand)))) (cs/get-move cheat-sheet player-card dealer-card) ; Player has 2 cards, and their sum is between 8 and 18, and both cards are different, and no Ace in the hand.
      (= 2 (count (get-ace-position player-hand))) (cs/get-move cheat-sheet "AA" dealer-card) ; Player has 2 Aces.
      (= 1 (count (get-ace-position player-hand))) (let [no-ace-card-num (first (get-no-ace-position player-hand))
                                                         player-card (str "A" (get-player-value player-hand no-ace-card-num))]
                                                     (cs/get-move cheat-sheet player-card dealer-card)) ; Player has 1 Ace.
      (and (= (get-player-value player-hand 1)
              (get-player-value player-hand 2))
           (not= (get-player-value player-hand 1)
                 "ace"))
      (let [player-card (str (get-player-value player-hand 1) (get-player-value player-hand 2))] (cs/get-move cheat-sheet player-card dealer-card)) ; Player has 2 cards of the same value (not Ace).
      (< current-player-sum 9) (cs/get-move cheat-sheet "8" dealer-card) ; Player has 2 cards, and current sum is less than 9.
      (and (> current-player-sum 17)
           (< current-player-sum 22)) (cs/get-move cheat-sheet "17" dealer-card) ; Player has 2 cards, and current sum is greater than 17 and less than 22.

      :else "Not covered in cheat sheet.")))                ; Default case, not covered by the cheat sheet.


; Counting probability
(defn contain-element?
  "Checks if a specified element is present in the given list."
  [l e]
  (some #(= % e) l))

(defn generate-list
  "Generates a list of integers starting from 1 up to the specified number `n` (inclusive)."
  [n]
  (if (<= n 0) '()
               (loop [i 1 num n l '()]
                 (if (= (+ n 1) i)
                   l
                   (recur (inc i) num (cons i l))))))

(defn get-all-values
  "Returns list of both dealer and player cards values (Integer) by invoking 'card-value'."
  [hand]
  (try (concat (map #(card-value (get % :value)) (:player-cards hand))
          [(card-value (:value (first (:dealer-card hand))))])
       (catch Exception e
         (println (.getMessage e)))))
(defn suit-count
  "Counts total number of suits."
  [suits]
  (count suits))
(defn num-cards-in-deck
  "Counts total number of cards in deck."
  [initial-deck]
  (count initial-deck))
(defn num-passed-cards
  "Calculates number of player's passed cards in current game session including one dealer's revealed card."
  [player-card]
  (+ (count player-card) 1))

; Probability of player winning = divisor / counter
; The probability calculation varies based on the recommended move (H, DD, P) versus S.

(defn divisor
  "Calculates the number of remaining cards in the deck available to be drawn."
  [player-card initial-deck]
  (- (num-cards-in-deck initial-deck) (num-passed-cards player-card)))

(defn fit-value-hit
  "Calculates the number of cards witch value wouldn't cause the player to go bust."
  [player-card]
  (- 21 (player-sum (adjust-ace-value! (atom player-card)))))

(defn counter-hit
  "Calculates the counter as the total number of cards in the deck that wouldn't lead to player bust.
  Decrements the counter values for cards no longer available due to being drawn by the player or dealer."
  [player-card current-cards suits]
  (let [counter (* (fit-value-hit player-card) (suit-count suits))
        l2 (get-all-values current-cards)
        l1 (generate-list (fit-value-hit player-card))]
    (reduce (fn
              [acc elem]
              (if (contain-element? l1 elem)
                (dec acc)
                acc))
            counter
            l2)))
(defn count-probability-hit
  "Scenario 1: Recommended move: 'H'
  Calculates the probability of player not busting based on the count of fit-values (counter) and
  the total count of cards (divisor). Returns the result as a formatted percentage."
  [player-card current-cards initial-deck suits]
  (try
    (str (format "%.2f" (* 100 (float (/ (counter-hit player-card current-cards suits)
                                         (divisor player-card initial-deck))))) "% of not busting.")
    (catch ArithmeticException _
      (println "Divide by zero exception."))))

(defn value
  "Converts a numeric card value to its corresponding string representation."
  [val]
  (case val
    11 "ace"
    12 "jack"
    13 "queen"
    14 "king"
    val))

(defn subvector
  "Generates a subvector from an input list based on the specified start and stop values."
  [input-list a b]
  (try (let [start-element (value a)
             stop-element (value b)
             start-index (.indexOf input-list start-element)
             stop-index (inc (.indexOf input-list stop-element))]
         (subvec (vec input-list) start-index stop-index))
       (catch IndexOutOfBoundsException _
         (println "Index out of bounds exception."))))

; Game rules for dealer: If the total is 16 or under, they must take a card.
; The dealer must continue to take cards until the total is 17 or more, at which point the dealer must stand.
; If the dealer's revealed card is less than 7, the unrevealed card can take on any value without the risk of the dealer going bust.
; (start-value is 1, end-value is 14, meaning: all values can be considered)
(defn start-value
  "Determines the starting value for generating a sequence of values for the dealer to hit.
   It is calculated as 17 (the minimum sum for the dealer to stand) minus the dealer's card value."
  [current-cards]
  (if (<= (:value (dealer-values-for-cheat-sheet current-cards)) 6) 1
                                                                    (- 17 (:value (dealer-values-for-cheat-sheet current-cards)))))

(defn end-value
  "Determines the ending value for generating a sequence of values for the dealer to hit.
   It is calculated as 21 (the maximum sum before going bust) minus the dealer's card value."
  [current-cards]
  (if (<= (:value (dealer-values-for-cheat-sheet current-cards)) 6) 14
                                                                    (- 21 (:value (dealer-values-for-cheat-sheet current-cards)))))
(defn fit-value-stand
  "Determines the number of cards the dealer must draw (according to game rules for dealer) without going bust."
  [current-cards]
  (count (subvector values (value (start-value current-cards)) (value (end-value current-cards)))))
(defn counter-stand
  "Calculates the counter as the total number of cards in the deck that wouldn't lead to dealer bust.
  Decrements the counter values for cards no longer available due to being drawn by the player or dealer."
  [current-cards suits]
  (let [counter (* (fit-value-stand current-cards) (suit-count suits))
        l2 (get-all-values current-cards)
        l1 (generate-list (fit-value-stand current-cards))]
    (reduce (fn
              [acc elem]
              (if (contain-element? l1 elem)
                (dec acc)
                acc))
            counter
            l2)))

(defn count-probability-s
  "Scenario 2: Recommended move: 'S'
  Calculates the probability of the dealer winning when the player stands."
  [current-cards suits]
  (try (float (/ (counter-stand current-cards suits)
                 (divisor current-cards suits)))
       (catch ArithmeticException _
         (println "Divide by zero exception."))))

(defn count-probability-stand
  "Calculates the probability of the player winning (subtract odds of dealer winning from 1) when the player stands.
  Returns the result as a formatted percentage."
  [current-cards suits]
  (str (format "%.2f" (* 100 (float (- 1 (count-probability-s current-cards suits))))) "% of winning."))


(defn odds
  "Calculates the odds of winning based on the recommended move in a Blackjack game."
  [current-cards player-cards cheat-sheet suits initial-deck]
  (let [move (recommend-move cheat-sheet current-cards player-cards)]
    (cond
      (or (= move "H") (= move "DD") (= move "P"))          ; Odds are calculated the same way for recommended moves 'H', 'P' and 'DD'.
      (count-probability-hit player-cards current-cards initial-deck suits)
      (= move "S")
      (count-probability-stand current-cards suits)
      :else "Unknown move!")))

(defn play
  "Encapsulates game logic."
  [current-cards player-cards cheat-sheet suits initial-deck]
  (let [move (recommend-move @cheat-sheet @current-cards @player-cards)] ; Move recommendation according to Blackjack cheat sheet.
    (db/insert-game (player-sum @player-cards) (get-dealer-value @current-cards) move) ; Persisting the current game session data into the database.
    (println "Play: " move)
    (cond
      (= move "S")                                          ; Stand
      (if (> (player-sum (adjust-ace-value! player-cards)) 21)
        "Bust: 100%"                                        ; If sum of player's hand is greater than 21, player goes bust.
        (if (> (:value (dealer-values-for-cheat-sheet @current-cards)) 6) ; If the dealer's revealed card is less than 7, calculating the odds becomes impossible, as the unrevealed card can take on any value without the risk of the dealer going bust.
          (println (odds @current-cards @player-cards @cheat-sheet suits initial-deck))
          "Can't calculate odds!"))
      (= move "H")                                          ; Hit
      (do
        (println (odds @current-cards @player-cards @cheat-sheet suits initial-deck))
        (add-both! current-cards player-cards)
        (play current-cards player-cards cheat-sheet suits initial-deck))
      (= move "DD")                                         ; Double down
      (do
        (add-both! current-cards player-cards)              ; In case of DD, player can hit only one more card.
        (println (odds @current-cards @player-cards @cheat-sheet suits initial-deck))
        (db/insert-game (player-sum @player-cards) (get-dealer-value @current-cards) move)
        )
      (= move "P")                                          ; Split
      (let [player-1 (atom {:card-1 (:card-1 @player-cards)}) ; Extract the first card for the first split hand.
            player-2 (atom {:card-1 (:card-2 @player-cards)}) ; Extract the second card for the second split hand.
            current-1 (atom {:player-cards (:card-1 @player-1) :dealer-card (:dealer-card @current-cards)})
            current-2 (atom {:player-cards (:card-1 @player-2) :dealer-card (:dealer-card @current-cards)})]
        (do
          (println "! First card ! Play:  H")
          (println (odds @current-1 @player-1 @cheat-sheet suits initial-deck))
          (add-both! current-1 player-1)
          (play current-1 player-1 cheat-sheet suits initial-deck)
          ;(println @player-1)
          (println "! Second card ! Play:  H")
          (println (odds @current-2 @player-2 @cheat-sheet suits initial-deck))
          (add-both! current-2 player-2)
          (play current-2 player-2 cheat-sheet suits initial-deck)
          ; (println @player-2)
          ))
      :else
      "End of game!")))

(defn start-game
  "Runs the game."
  []
  (let [current-cards (atom (read-card-from-keyboard))
        player-starting-hand (atom {:card-1 (first (get @current-cards :player-cards))
                                    :card-2 (second (get @current-cards :player-cards))})]
    (db/execute-script)
    (play current-cards player-starting-hand cs/blackjack-cheat-sheet suits initial-deck)
    (println (db/select-game))))


