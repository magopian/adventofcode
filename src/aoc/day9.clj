; http://adventofcode.com/day/9
(ns aoc.day9
  (:use [clojure.math.combinatorics :only [permutations]]))


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input (clojure.string/split-lines (slurp "src/aoc/day9.input")))


; --- Day 9: All in a Single Night ---

; Every year, Santa manages to deliver all of his presents in a single night.

; This year, however, he has some new locations to visit; his elves have
; provided him the distances between every pair of locations. He can start and
; end at any two (different) locations he wants, but he must visit each
; location exactly once. What is the shortest distance he can travel to achieve
; this?

; For example, given the following distances:

; London to Dublin = 464
; London to Belfast = 518
; Dublin to Belfast = 141

; The possible routes are therefore:

; Dublin -> London -> Belfast = 982
; London -> Dublin -> Belfast = 605
; London -> Belfast -> Dublin = 659
; Dublin -> Belfast -> London = 659
; Belfast -> Dublin -> London = 605
; Belfast -> London -> Dublin = 982

; The shortest of these is London -> Dublin -> Belfast = 605, and so the answer
; is 605 in this example.

; What is the distance of the shortest route?

(def test-input ["London to Dublin = 464"
                 "London to Belfast = 518"
                 "Dublin to Belfast = 141"])

(defn parse [input-line]
  "Convert 'AlphaCentauri to Snowdin = 66' to {#{AlphaCentauri Snowdin} 66}."
  ; re-find first return the full match (we don't want) and then the groups.
  (let [[loc1 loc2 dist] (rest (re-find #"(\w+) to (\w+) = (\d+)" input-line))]
    {#{loc1 loc2} (Integer. dist)}))

(assert (= (parse "AlphaCentauri to Snowdin = 66")
           {#{"AlphaCentauri" "Snowdin"} 66}))
(assert (= (parse "Snowdin to AlphaCentauri = 66")
           {#{"AlphaCentauri" "Snowdin"} 66}))

; All the distances between the different locations.
(def distances (reduce conj {} (map parse input)))
(def test-distances (reduce conj {} (map parse test-input)))

; All the location names.
(defn get-locations [distances]
  "Return all the location names from a 'distances' map."
  (reduce clojure.set/union (keys distances)))

(defn get-route-distance [distances route]
  "Given a list of locations, return the total route distance."
  (let [segments (map set (partition 2 1 route))
        lengths (map distances segments)]
    (reduce + lengths)))

(assert (= (get-route-distance test-distances ["London" "Dublin" "Belfast"])
           605))

(defn shortest-route [distances]
  (let [locations (get-locations distances)
        all-routes (permutations locations)]
    (apply min (map #(get-route-distance distances %) all-routes))))

(assert (= (shortest-route test-distances) 605))

(defn part1 []
  (println "The shortest distance is"
           (shortest-route distances)
           ))

; --- Part Two ---

; The next year, just to show off, Santa decides to take the route with the
; longest distance instead.

; He can still start and end at any two (different) locations he wants, and he
; still must visit each location exactly once.

; For example, given the distances above, the longest route would be 982 via
; (for example) Dublin -> London -> Belfast.

; What is the distance of the longest route?

(defn longest-route [distances]
  (let [locations (get-locations distances)
        all-routes (permutations locations)]
    (apply max (map #(get-route-distance distances %) all-routes))))

(assert (= (longest-route test-distances) 982))

(defn part2 []
  (println "The longest distance is"
           (longest-route distances)
           ))
