; http://adventofcode.com/day/15
(ns aoc.day15)


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input (clojure.string/split-lines (slurp "src/aoc/day15.input")))

; --- Day 15: Science for Hungry People ---

; Today, you set out on the task of perfecting your milk-dunking cookie recipe.
; All you have to do is find the right balance of ingredients.

; Your recipe leaves room for exactly 100 teaspoons of ingredients. You make a
; list of the remaining ingredients you could use to finish the recipe (your
; puzzle input) and their properties per teaspoon:

;   - capacity (how well it helps the cookie absorb milk)
;   - durability (how well it keeps the cookie intact when full of milk)
;   - flavor (how tasty it makes the cookie)
;   - texture (how it improves the feel of the cookie)
;   - calories (how many calories it adds to the cookie)

; You can only measure ingredients in whole-teaspoon amounts accurately, and
; you have to be accurate so you can reproduce your results in the future. The
; total score of a cookie can be found by adding up each of the properties
; (negative totals become 0) and then multiplying together everything except
; calories.

; For instance, suppose you have these two ingredients:

; Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
; Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3

; Then, choosing to use 44 teaspoons of butterscotch and 56 teaspoons of
; cinnamon (because the amounts of each ingredient must add up to 100) would
; result in a cookie with the following properties:

;   - A capacity of 44*-1 + 56*2 = 68
;   - A durability of 44*-2 + 56*3 = 80
;   - A flavor of 44*6 + 56*-2 = 152
;   - A texture of 44*3 + 56*-1 = 76

; Multiplying these together (68 * 80 * 152 * 76, ignoring calories for now)
; results in a total score of 62842880, which happens to be the best score
; possible given these ingredients. If any properties had produced a negative
; total, it would have instead become zero, causing the whole score to multiply
; to zero.

; Given the ingredients in your kitchen and their properties, what is the total
; score of the highest-scoring cookie you can make?

(def test-input [
  "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
  "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"])

(defn parse [input-line]
  "Convert
  'Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8'
  to [-1 -2 6 3 8]."
  (map #(Integer. %) (re-seq #"-?\d+" input-line)))

(assert (= (parse "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8")
           [-1 -2 6 3 8]))

(def ingredients (map parse input))
(def test-ingredients (map parse test-input))

(defn score-for-ingredient [ingredient spoons]
  "For a given ingredient and spoons return the scores per property."
  (map #(* spoons %) ingredient))

(assert (= (score-for-ingredient [-1 -2 6 3 8] 44) [-44 -88 264 132 352]))
(assert (= (score-for-ingredient [2 3 -2 -1 3] 56) [112 168 -112 -56 168]))

(defn scores-per-properties [ingredient-scores]
  "Return the scores per properties for the ingredient scores provided."
  (let [length (count ingredient-scores)
        properties (partition length (apply interleave ingredient-scores))]
    (map (fn [property-scores]
           (let [total (apply + property-scores)]
             (if (> total 0) total 0)))
         properties
         )))

(assert (= (scores-per-properties [[-44 -88 264 132 352]
                                   [112 168 -112 -56 168]])
           [68 80 152 76 520]))

(defn score [ingredients proportion]
  "Return the score for the given ingredients in this proportion."
  (let [recipe (zipmap ingredients proportion)
        ingredient-scores (map (fn [[ingredient spoons]]
                                 (score-for-ingredient ingredient spoons))
                               recipe)
        properties-scores (scores-per-properties ingredient-scores)]
    (apply * (butlast properties-scores)))) ; Calories don't count for score.

(assert (= (score test-ingredients [44 56]) 62842880))

(def all-proportions (for [x (range (inc 100))
                           y (range (inc (- 100 x)))
                           z (range (inc (- 100 x y)))]
                       [x y z (- 100 x y z)]))
(def all-test-proportions (for [x (range (inc 100))]
                            [x (- 100 x)]))

(assert (= (apply max (map #(score test-ingredients %) all-test-proportions))
           62842880))


; The following takes less than 10 seconds on a recent (2015) mbp.
(defn part1 []
  (println
    "The total score of the highest-scoring cookie is"
    (apply max (map #(score ingredients %) all-proportions))))


; --- Part Two ---

; Your cookie recipe becomes wildly popular! Someone asks if you can make
; another recipe that has exactly 500 calories per cookie (so they can use it
; as a meal replacement). Keep the rest of your award-winning process the same
; (100 teaspoons, same ingredients, same scoring system).

; For example, given the ingredients above, if you had instead selected 40
; teaspoons of butterscotch and 60 teaspoons of cinnamon (which still adds to
; 100), the total calorie count would be 40*8 + 60*3 = 500. The total score
; would go down, though: only 57600000, the best you can do in such trying
; circumstances.

; Given the ingredients in your kitchen and their properties, what is the total
; score of the highest-scoring cookie you can make with a calorie total of 500?

(defn score-with-calories [ingredients proportion]
  "Return the score for the given ingredients in this proportion."
  (let [recipe (zipmap ingredients proportion)
        ingredient-scores (map (fn [[ingredient spoons]]
                                 (score-for-ingredient ingredient spoons))
                               recipe)
        properties-scores (scores-per-properties ingredient-scores)]
    [(apply * (butlast properties-scores)) ; Calories don't count for score.
     (last properties-scores)])) ; Calories.

(assert (= (score-with-calories test-ingredients [40 60]) [57600000 500]))

(defn only-500-calories [all-scores-with-calories]
  "Return all the scores that have exactly 500 calories."
  (map first (filter (fn [[score calories]]
            (= calories 500))
          all-scores-with-calories)))

(assert (= (apply max
                  (only-500-calories
                    (map #(score-with-calories test-ingredients %)
                         all-test-proportions)))
           57600000))

; The following takes less than 10 seconds on a recent (2015) mbp.
(defn part2 []
  (println
    "The total score of the highest-scoring cookie with 500 calories is"
    (apply max
           (only-500-calories
             (map #(score-with-calories ingredients %)
                  all-proportions)))))
