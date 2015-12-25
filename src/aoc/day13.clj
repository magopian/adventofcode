; http://adventofcode.com/day/13
(ns aoc.day13
  (:use [clojure.math.combinatorics :only [permutations]]))


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input (clojure.string/split-lines (slurp "src/aoc/day13.input")))

; --- Day 13: Knights of the Dinner Table ---

; In years past, the holiday feast with your family hasn't gone so well. Not
; everyone gets along! This year, you resolve, will be different. You're going
; to find the optimal seating arrangement and avoid all those awkward
; conversations.

; You start by writing up a list of everyone invited and the amount their
; happiness would increase or decrease if they were to find themselves sitting
; next to each other person. You have a circular table that will be just big
; enough to fit everyone comfortably, and so each person will have exactly two
; neighbors.

; For example, suppose you have only four attendees planned, and you calculate
; their potential happiness as follows:

; Alice would gain 54 happiness units by sitting next to Bob.
; Alice would lose 79 happiness units by sitting next to Carol.
; Alice would lose 2 happiness units by sitting next to David.
; Bob would gain 83 happiness units by sitting next to Alice.
; Bob would lose 7 happiness units by sitting next to Carol.
; Bob would lose 63 happiness units by sitting next to David.
; Carol would lose 62 happiness units by sitting next to Alice.
; Carol would gain 60 happiness units by sitting next to Bob.
; Carol would gain 55 happiness units by sitting next to David.
; David would gain 46 happiness units by sitting next to Alice.
; David would lose 7 happiness units by sitting next to Bob.
; David would gain 41 happiness units by sitting next to Carol.

; Then, if you seat Alice next to David, Alice would lose 2 happiness units
; (because David talks so much), but David would gain 46 happiness units
; (because Alice is such a good listener), for a total change of 44.

; If you continue around the table, you could then seat Bob next to Alice (Bob
; gains 83, Alice gains 54). Finally, seat Carol, who sits next to Bob (Carol
; gains 60, Bob loses 7) and David (Carol gains 55, David gains 41). The
; arrangement looks like this:

;      +41 +46
; +55   David    -2
; Carol       Alice
; +60    Bob    +54
;      -7  +83

; After trying every other seating arrangement in this hypothetical scenario,
; you find that this one is the most optimal, with a total change in happiness
; of 330.

; What is the total change in happiness for the optimal seating arrangement of
; the actual guest list?

(def test-input [
  "Alice would gain 54 happiness units by sitting next to Bob."
  "Alice would lose 79 happiness units by sitting next to Carol."
  "Alice would lose 2 happiness units by sitting next to David."
  "Bob would gain 83 happiness units by sitting next to Alice."
  "Bob would lose 7 happiness units by sitting next to Carol."
  "Bob would lose 63 happiness units by sitting next to David."
  "Carol would lose 62 happiness units by sitting next to Alice."
  "Carol would gain 60 happiness units by sitting next to Bob."
  "Carol would gain 55 happiness units by sitting next to David."
  "David would gain 46 happiness units by sitting next to Alice."
  "David would lose 7 happiness units by sitting next to Bob."
  "David would gain 41 happiness units by sitting next to Carol."])

(defn parse-happiness [happiness]
  (let [[change happiness] (clojure.string/split happiness #" ")
        happiness (Integer. happiness)]
    (if (= change "gain")
      happiness
      (- happiness))))

(assert (= (parse-happiness "gain 54") 54))
(assert (= (parse-happiness "lose 54") -54))

(defn parse [input-line]
  "Convert 'Alice would gain 54 happiness units by sitting next to Bob' to
  {[Alice Bob] 54}."
  ; re-find first return the full match (we don't want) and then the groups.
  (let [[pers1 happiness pers2] (rest (re-find #"(\w+) would (\w+ \d+) happiness units by sitting next to (\w+)" input-line))]
    {[pers1 pers2] (parse-happiness happiness)}))

(assert (=
         (parse "Alice would gain 54 happiness units by sitting next to Bob.")
         {["Alice" "Bob"] 54}))
(assert (=
         (parse "Alice would lose 79 happiness units by sitting next to Carol.")
         {["Alice" "Carol"] -79}))

; All the happiness changes between the different family members.
(def happinesses (reduce conj {} (map parse input)))
(def test-happinesses (reduce conj {} (map parse test-input)))

; All the family member names.
(defn get-names [happinesses]
  "Return all the family member names from a 'happinesses' map."
  (set (flatten (keys happinesses))))

(assert (= (get-names test-happinesses) #{"Alice" "Bob" "Carol" "David"}))

(defn get-seating-happiness [happinesses seating]
  "Given a list of family members (the seating), return the total happiness."
  (let [seating (conj seating (first seating)) ; "connect" the circle.
        ; List of "A sits next to B" and "B sits next to A".
        proximities (concat (partition 2 1 seating)
                            (partition 2 1 (reverse seating)))
        changes (concat (map happinesses proximities))]
    (reduce + changes)))

(assert (= (get-seating-happiness test-happinesses
                                  ["Alice" "Bob" "Carol" "David"])
           330))

(defn remove-reverses [coll-of-collections]
  "Given a collection like
  [[Alice Bob Carol] [Bob Alice Carol] [Carol Bob Alice]], filter out the
  elements that are reverses of one another:
  #{[Alice Bob Carol] [Bob Alice Carol]}."
  (reduce (fn [acc coll]
            (if (acc (reverse coll))
              acc  ; Don't keep this collection, its reverse is already there.
              (conj acc coll)))  ; Keep this collection.
          #{}
          coll-of-collections))

(assert (= (remove-reverses (permutations (range 3)))
           #{[0 1 2] [1 0 2] [0 2 1]}))

(defn highest-happiness [happinesses]
  (let [names (get-names happinesses)
        ; We could optimise by cutting the seatings in half: the other half are
        ; just the same seatings, but in reverse order (thus with the same
        ; happiness).
        all-seatings (map vec (permutations names))
        unique-seatings (remove-reverses all-seatings)]
    (apply max (map #(get-seating-happiness happinesses %) unique-seatings))))

(assert (= (highest-happiness test-happinesses) 330))

; The following takes less than 1 second on a recent (2015) mbp.
(defn part1 []
  (println "The total change in happiness for the optimal seating is"
           (highest-happiness happinesses)))


; --- Part Two ---

; In all the commotion, you realize that you forgot to seat yourself. At this
; point, you're pretty apathetic toward the whole thing, and your happiness
; wouldn't really go up or down regardless of who you sit next to. You assume
; everyone else would be just as ambivalent about sitting next to you, too.

; So, add yourself to the list, and give all happiness relationships that
; involve you a score of 0.

; What is the total change in happiness for the optimal seating arrangement
; that actually includes yourself?

(def new-happinesses
  (let [names (get-names happinesses)]
    (reduce (fn [happinesses name]
              (assoc happinesses ["myself" name] 0 [name "myself"] 0))
            happinesses
            names)))

; The following takes less than 10 seconds on a recent (2015) mbp.
(defn part2 []
  (println
    "The total change in happiness for the optimal seating with myself is"
    (highest-happiness new-happinesses)))
