; http://adventofcode.com/day/10
(ns aoc.day10)


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input "3113322113")


; --- Day 10: Elves Look, Elves Say ---

; Today, the Elves are playing a game called look-and-say. They take turns
; making sequences by reading aloud the previous sequence and using that
; reading as the next sequence. For example, 211 is read as "one two, two
; ones", which becomes 1221 (1 2, 2 1s).

; Look-and-say sequences are generated iteratively, using the previous value as
; input for the next step. For each step, take the previous value, and replace
; each run of digits (like 111) with the number of digits (3) followed by the
; digit itself (1).

; For example:

;   - 1 becomes 11 (1 copy of digit 1).
;   - 11 becomes 21 (2 copies of digit 1).
;   - 21 becomes 1211 (one 2 followed by one 1).
;   - 1211 becomes 111221 (one 1, one 2, and two 1s).
;   - 111221 becomes 312211 (three 1s, two 2s, and one 1).

; Starting with the digits in your puzzle input, apply this process 40 times.
; What is the length of the result?

(defn look [input]
  "Given 111221, return [[\1 \1 \1] [\2 \2] [\1]]."
  (partition-by identity input))

(assert (= (look "111221") [[\1 \1 \1] [\2 \2] [\1]]))

(defn say [partitioned]
  "Given [[\1 \1 \1] [\2 \2] [\1]], return 312211."
  (apply str (mapcat (fn [group] [(count group) (first group)]) partitioned)))

(assert (= (say [[\1 \1 \1] [\2 \2] [\1]]) "312211"))

(def look-and-say (comp say look))

(defn part1 []
  (println "The length of the result is"
           (->> (iterate look-and-say input)  ; Over and over.
                (take 41)  ; Apply 40 times. The first only returns the input.
                last  ; We only care about the result of the last call.
                count
                )))


; --- Part Two ---

; Neat, right? You might also enjoy hearing John Conway talking about this
; sequence (that's Conway of Conway's Game of Life fame).

; Now, starting again with the digits in your puzzle input, apply this process
; 50 times. What is the length of the new result?


; This takes less than 10 seconds on a recent (2015) mbp.
(defn part2 []
  (println "The length of the result is"
           (->> (iterate look-and-say input)  ; Over and over.
                (take 51)  ; Apply 40 times. The first only returns the input.
                last  ; We only care about the result of the last call.
                count
                )))
