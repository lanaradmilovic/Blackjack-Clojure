(ns probability.probability)

(defn contain-element?
  "Checks if a specified element is present in the given list."
  [l e]
  (some #(= % e) l))

(defn generate-list
  "Generates a list of integers starting from 1 up to the specified number `n` (inclusive)."
  [n]
  (loop [i 1 num n l '()]
    (if (= (+ n 1) i)
      l
      (recur (inc i) num (cons i l)))))

(defn get-all-values
  [hand]
  "Returns list of both dealer and player original cards values."
  (concat (map #(get % :value) (:player-cards hand))
          [(str (:value (first (:dealer-card hand))))]))

(defn decrement-counter-on-match
  "Decrements the values in a counter atom based on the matching elements between
  a generated list and the values obtained from a card. The list of matching elements
  is determined by the specified fit-value."
  [counter card fit-value]
  (let [l2 (get-all-values card)
        l1 (generate-list fit-value)]
    (swap! counter
           (fn
             [c]
             (reduce (fn
                       [acc elem]
                       (if (contain-element? l1 elem)
                         (dec acc)
                         acc))
                     c
                     l2)))))
(defn count-probability-hit
  "Scenario 1: Recommended move: 'H'
  Calculates the probability of player not busting based on the count of fit-values (counter) and
  the total count (divisor). Returns the result as a formatted percentage."
  [counter divisor card fit-value]
  (str (format "%.2f" (* 100 (float (/ (decrement-counter-on-match counter card fit-value)
                                       divisor)))) "% of not busting."))

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
  (let [start-element (value a)
        stop-element (value b)
        start-index (.indexOf input-list start-element)
        stop-index (inc (.indexOf input-list stop-element))]
    (subvec (vec input-list) start-index stop-index)))

(defn count-probability-s
  "Scenario 2: Recommended move: 'S'
  Calculates the probability of the dealer winning when the player stands."
  [counter divisor current-cards fit-value]
  (float (/ (decrement-counter-on-match counter current-cards fit-value)
            divisor)))

(defn count-probability-stand
  "Calculates the probability of the player winning when the player stands.
  Returns the result as a formatted percentage."
  [counter divisor current-cards fit-value]
  (str (format "%.2f" (* 100 (float (- 1 (count-probability-s counter divisor current-cards fit-value))))) "% of winning."))




