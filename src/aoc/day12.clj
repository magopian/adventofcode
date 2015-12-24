; http://adventofcode.com/day/12
(ns aoc.day12
  (:use [clojure.data.json :only [read-str write-str]]))


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input (slurp "src/aoc/day12.input"))

; --- Day 12: JSAbacusFramework.io ---

; Santa's Accounting-Elves need help balancing the books after a recent order.
; Unfortunately, their accounting software uses a peculiar storage format.
; That's where you come in.

; They have a JSON document which contains a variety of things: arrays
; ([1,2,3]), objects ({"a":1, "b":2}), numbers, and strings. Your first job is
; to simply find all of the numbers throughout the document and add them
; together.

; For example:

;     [1,2,3] and {"a":2,"b":4} both have a sum of 6.
;     [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
;     {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
;     [] and {} both have a sum of 0.

; You will not encounter any strings containing numbers.

; What is the sum of all numbers in the document?

(defn sum-numbers [input]
  (apply + (map #(Integer. %) (re-seq #"-?\d+" input))))


; The following takes (far) less than 1 second on a recent (2015) mbp.
(defn part1 []
  (println "The sum of all numbers in the document is"
           (sum-numbers input)))


; --- Part Two ---

; Uh oh - the Accounting-Elves have realized that they double-counted
; everything red.

; Ignore any object (and all of its children) which has any property with the
; value "red". Do this only for objects ({...}), not arrays ([...]).

;   - [1,2,3] still has a sum of 6.
;   - [1,{"c":"red","b":2},3] now has a sum of 4, because the middle object is
;     ignored.
;   - {"d":"red","e":[1,2,3,4],"f":5} now has a sum of 0, because the entire
;     structure is ignored.
;   - [1,"red",5] has a sum of 6, because "red" in an array has no effect.

; Idea: load the JSON as a clojure data structure, then walk it, and remove all
; elements with a "red" property.

(def JSON (read-str input))

(defn nullify-reds [input]
  (if (and (map? input)
           (vals input)
           ((set (vals input)) "red"))  ; Is "red" in the set of keys?
    0  ; If so return 0 instead of the structure.
    input))  ; Otherwise return the structure itself.

; The following takes (far) less than 1 second on a recent (2015) mbp.
(defn part2 []
  (println "The sum of all non-red numbers in the document is"
           (sum-numbers (write-str (clojure.walk/prewalk nullify-reds JSON)))))
