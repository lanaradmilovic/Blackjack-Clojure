(ns business.business
  (:require [cheat-sheet.cheat-sheet :refer :all :as cs]
            [db.db :refer :all :as db]
            [probability.probability :refer :all :as p]))

(def suits ["club" "heart" "spade" "diamond"])
(def numbers (range 1 11))
(def faces ["jack" "queen" "king" "ace"])
(def values (concat numbers faces))

(defn generate-card
  [value suit]
  {:value value :suit suit})
(def initial-deck
  (for [v values s suits]
    (generate-card v s)))
(defn read-number-of-decks
  "Reads the number of decks the player wants to play with."
  []
  (println "Enter the number of decks (1 to 8, default is 1): ")
  (let [input (read-line)
        num-decks (try (Integer/parseInt input) (catch Exception _ 1))]
    (if (and (integer? num-decks) (<= 1 num-decks 8)) num-decks
                                                      (do
                                                        (println "Invalid input. Defaulting to 1 deck.")
                                                        1))))

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
  [f remaining-attempts]
  (let [f (if (<= (count f) 2)
            (Integer/parseInt f)
            f)]
    (if (some #{f} values) f
                           (if (<= remaining-attempts 1)
                             (do
                               (println "You've exceeded the maximum number of attempts. Exiting...")
                               (System/exit 1))
                             (do
                               (println (str "Invalid input. You have " (- remaining-attempts 1) " attempts remaining."))
                               (println "The value must be one of the following: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 'ace', 'jack', 'queen', 'king'.")
                               (valid-face (read-face) (dec remaining-attempts)))))))

(defn valid-suit
  "Validates the suit entered by the user."
  [s remaining-attempts]
  (if (some #{s} suits) s
                        (if (<= remaining-attempts 1)
                          (do
                            (println "You've exceeded the maximum number of attempts. Exiting...")
                            (System/exit 1))
                          (do
                            (println (str "Invalid input. You have " (- remaining-attempts 1) " attempts remaining."))
                            (println "The suit must be one of the following: 'club', 'heart', 'spade', 'diamond'.")
                            (valid-suit (read-suit) (dec remaining-attempts))))))

(defn read-card-from-keyboard
  "Reads player's and dealer's cards from the keyboard input.
  Prompts the user to enter the face and suit of each card."
  []
  (loop [i 1 player-cards '() dealer-card '() max-attempts 3]
    (if (< i 3)
      (do
        (println "Enter YOUR " i ". card: ")
        (let [face (read-face)]
          (valid-face face max-attempts)
          (let [suit (read-suit)]
            (valid-suit suit max-attempts)
            (recur (inc i) (conj player-cards (generate-card face suit)) dealer-card max-attempts))))
      (if (= i 3)
        (do
          (println "Enter DEALER'S covered card: ")
          (flush)
          (let [face (read-face)]
            (valid-face face max-attempts)
            (let [suit (read-suit)]
              (valid-suit suit max-attempts)
              (recur (inc i) player-cards (conj dealer-card (generate-card face suit)) max-attempts))))
        {:player-cards player-cards
         :dealer-card  dealer-card}))))



;(def current-cards (atom (read-card-from-keyboard)))
;
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
  (let [face (read-face)
        max-attempts 3]
    (valid-face face max-attempts)
    (let [suit (read-suit)]
      (valid-suit suit max-attempts)
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
  (update game-session :player-cards (fn [existing-cards]
                                       (conj existing-cards new-card))))

(defn add-new-current-card!
  "Atomically adds a new card to the current game session cards using 'add-new-current-card'."
  [game-session new-card]
  (swap! game-session add-new-current-card new-card))

(defn add-both!
  "Adds a new card to both the player's starting hand and the current game session cards."
  [game-session player-hand]
  (try
    (let [new-card (read-new-card)]
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
  (< lower-bound n upper-bound))

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

; Counting probabilities

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

(defn fit-value-hit
  "Determines the highest card value to avoid exceeding 21 in a blackjack game.
  If the current player total is 9 or less, the 'ace' (11) is considered the highest value. Otherwise, the function calculates
  the highest value that won't result in a bust, by subtracting the current player total from 21."
  [player-card]
  (if (> (player-sum (adjust-ace-value! (atom player-card))) 9) (- 21 (player-sum (adjust-ace-value! (atom player-card))))
                                                                11))

(defn count-probability-hit
  "Returns probability of player not busting as a formatted percentage."
  [player-card current-cards initial-deck suits num-decks]
  (let [values (generate-list (fit-value-hit player-card))]
    (str (format "%.2f" (* 100 (float (p/count-probability suits current-cards initial-deck player-card values num-decks)))) "% of not busting.")))

; Game rules for dealer: If the total is 16 or under, they must take a card.
; The dealer must continue to take cards until the total is 17 or more, at which point the dealer must stand.

(defn start-value
  "Determines the starting value for generating a sequence of values for the dealer to hit.
   It is calculated as 17 (the minimum sum for the dealer to stand) minus the dealer's card value."
  [current-cards]
  (if (= (:value (dealer-values-for-cheat-sheet current-cards)) 6) 14 ; Only 'ace' is considered.
                                                                   (- 17 (:value (dealer-values-for-cheat-sheet current-cards)))))

(defn end-value
  "Determines the ending value for generating a sequence of values for the dealer to hit.
   It is calculated as 21 (the maximum sum before going bust) minus the dealer's card value."
  [current-cards]
  (cond
    (= (:value (dealer-values-for-cheat-sheet current-cards)) 6) 14 ; Only 'ace' is considered.
    (= (:value (dealer-values-for-cheat-sheet current-cards)) 11) 13 ; All values instead of 'ace' included.
    (> (:value (dealer-values-for-cheat-sheet current-cards)) 7) 14 ; All values included.
    :else (- 21 (:value (dealer-values-for-cheat-sheet current-cards)))))

(defn get-all-odds-stand
  "Generates odds for dealer's winning values: total > 17, < 22, and surpassing player's hand."
  [suits current-cards initial-deck player-card values num-decks]
  (reduce #(if (>= (+ (card-value %2)
                      (:value (dealer-values-for-cheat-sheet current-cards)))
                   (player-sum player-card))
             (conj %1 (odds-certain-value suits current-cards %2 initial-deck player-card num-decks))
             %1)
          '() values))

(defn count-probability-s
  "Calculates the total probability by summing odds for standing."
  [suits current-cards initial-deck player-card values num-decks]
  (reduce + (get-all-odds-stand suits current-cards initial-deck player-card values num-decks)))

(defn count-probability-stand
  "Calculates the probability of the player winning (subtract odds of dealer winning from 1) when the player stands.
  Returns the result as a formatted percentage."
  [suits current-cards initial-deck player-card num-decks]
  (let [values (p/subvector values (start-value current-cards) (end-value current-cards))]
    (str (format "%.2f" (* 100 (float (- 1 (count-probability-s suits current-cards initial-deck player-card values num-decks))))) "% of winning.")))

; Odds for double down are calculated as odds of player getting a strong hand (total between 18 and 21).
(defn subvec-dd
  "Returns a subvector of card values that bring the player's total from 18 to 21."
  [player-card values]
  (try (let [start (- 19 (player-sum (adjust-ace-value! (atom player-card))))
             end (- 22 (player-sum (adjust-ace-value! (atom player-card))))]
         (p/subvector (vec values) start end))
       (catch IndexOutOfBoundsException _
         (println "Index out of bounds exception."))))

(defn count-probability-double-down
  "Returns probability of player getting strong hand when doubling down as a formatted percentage."
  [suits current-cards initial-deck player-card values num-decks]
  (let [vals (subvec-dd player-card values)]
    (str (format "%.2f" (* 100 (float (count-probability suits current-cards initial-deck player-card vals num-decks))))
         "% of getting strong hand (total >= 18).")))

(defn odds
  "Calculates the odds of winning based on the recommended move in a Blackjack game."
  [current-cards player-cards cheat-sheet suits initial-deck values num-decks]
  (let [move (recommend-move cheat-sheet current-cards player-cards)]
    (cond
      (or (= move "H") (= move "P"))                        ; Odds are calculated the same way for recommended moves 'H' and 'P'.
      (count-probability-hit player-cards current-cards initial-deck suits num-decks)
      (= move "S")
      (count-probability-stand suits current-cards initial-deck player-cards num-decks)
      (= move "DD")
      (count-probability-double-down suits current-cards initial-deck player-cards values num-decks)
      :else "Unknown move!")))

(defn play
  "Encapsulates game logic."
  [current-cards player-cards cheat-sheet suits initial-deck values num-decks]
  (let [move (recommend-move @cheat-sheet @current-cards @player-cards)] ; Move recommendation according to Blackjack cheat sheet.
    (db/insert-game (player-sum (adjust-ace-value! player-cards)) (get-dealer-value @current-cards) move) ; Persisting the current game session data into the database.
    (println "Play: " move)
    (cond
      (= move "S")                                          ; Stand
      (if (> (player-sum (adjust-ace-value! player-cards)) 21)
        "Bust: 100%"                                        ; If sum of player's hand is greater than 21, player goes bust.
        (if (>= (:value (dealer-values-for-cheat-sheet @current-cards)) 6) ; If the dealer's revealed card is less than 6, calculating the odds becomes impossible, as the unrevealed card can take on any value without the chance of the dealer standing.
          (println (odds @current-cards @player-cards @cheat-sheet suits initial-deck values num-decks))
          "Can't calculate odds!"))
      (= move "H")                                          ; Hit
      (do
        (println (odds @current-cards @player-cards @cheat-sheet suits initial-deck values num-decks))
        (add-both! current-cards player-cards)
        (play current-cards player-cards cheat-sheet suits initial-deck values num-decks))
      (= move "DD")                                         ; Double down
      (do
        (println (odds @current-cards @player-cards @cheat-sheet suits initial-deck values num-decks))
        (add-both! current-cards player-cards)              ; In case of DD, player can hit only one more card.
        ;(println "Play: S")                                 ; Then player must stand.
        ;(println (count-probability-stand suits @current-cards initial-deck @player-cards num-decks))
        ;(db/insert-game (player-sum @player-cards) (get-dealer-value @current-cards) move))
        (play current-cards player-cards cheat-sheet suits initial-deck values num-decks))
      (= move "P")                                          ; Split
      (let [player-1 (atom {:card-1 (:card-1 @player-cards)}) ; Extract the first card for the first split hand.
            player-2 (atom {:card-1 (:card-2 @player-cards)}) ; Extract the second card for the second split hand.
            current-1 (atom {:player-cards (list (:card-1 @player-1)) :dealer-card (:dealer-card @current-cards)})
            current-2 (atom {:player-cards (list (:card-1 @player-2)) :dealer-card (:dealer-card @current-cards)})]
        (do
          (println "! First card ! Play:  H")
          (println (odds @current-1 @player-1 @cheat-sheet suits initial-deck values num-decks))
          (add-both! current-1 player-1)
          (play current-1 player-1 cheat-sheet suits initial-deck values num-decks)
          (println "! Second card ! Play:  H")
          (println (odds @current-2 @player-2 @cheat-sheet suits initial-deck values num-decks))
          (add-both! current-2 player-2)
          (play current-2 player-2 cheat-sheet suits initial-deck values num-decks)))
      :else
      "End of game!")))

(defn start-game
  "Runs the game."
  []
  (let [num-decks (read-number-of-decks)
        current-cards (atom (read-card-from-keyboard))
        player-starting-hand (atom {:card-1 (first (get @current-cards :player-cards))
                                    :card-2 (second (get @current-cards :player-cards))})]
    (db/execute-script)
    (play current-cards player-starting-hand cs/blackjack-cheat-sheet suits initial-deck values num-decks)
    (println (db/select-game))))


