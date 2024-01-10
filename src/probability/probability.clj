(ns probability.probability)

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
  (if (empty? player-card) 0
                           (+ (count player-card) 1)))

; Probability of player winning = divisor / counter
; The probability calculation varies based on the recommended move (H, DD, P, S).

(defn divisor
  "Calculates the number of remaining cards in the deck available to be drawn."
  [player-card initial-deck]
  (- (num-cards-in-deck initial-deck) (num-passed-cards player-card)))

(defn get-all-val
  "Returns list of both dealer and player original cards values (String)."
  [hand]
  (try (concat (map #(get % :value) (:player-cards hand))
               [(:value (first (:dealer-card hand)))])
       (catch Exception e
         (println (.getMessage e)))))

(defn num-passed-certain-value
  "Counts occurrences of a specific value in the game session."
  [current-cards value]
  (let [num-passed (atom 0)
        l2 (get-all-val current-cards)]
    (doseq [el l2]
      (when (= el value)
        (swap! num-passed inc)))
    @num-passed))

(defn counter
  "Counts remaining occurrences of a specific value in the game deck."
  [suits current-cards value]
  (- (suit-count suits) (num-passed-certain-value current-cards value)))

(defn odds-certain-value
  "Calculates odds of a specific value occurring."
  [suits current-cards value initial-deck player-card]
  (try (float (/ (counter suits current-cards value)
                 (divisor player-card initial-deck)))
       (catch ArithmeticException e
         (println (.getMessage e)))))

(defn get-all-odds
  "Calculates and compiles a list of probabilities for specific card values."
  [suits current-cards initial-deck player-card values]
  (reduce #(conj %1 (odds-certain-value suits current-cards %2 initial-deck player-card)) '() values))

(defn count-probability
  "Calculates the total probability by summing odds for doubling down."
  [suits current-cards initial-deck player-starting-hand values]
  (reduce + (get-all-odds suits current-cards initial-deck player-starting-hand values)))

(defn value
  "Converts a numeric card value to its corresponding string representation.
  For value 11, returns 'ace'.
  For value 10, returns 'king' (and also 'jack' and 'queen' are covered since 'king' is the last in values).
  For other values, returns the original numeric value."
  [val]
  (case val
    11 "ace"
    10 "king" ; Also includes 'jack' and 'queen' since 'king' is the last in values.
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


