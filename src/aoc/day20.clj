; http://adventofcode.com/day/20
(ns aoc.day20)


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input 34000000)

; --- Day 20: Infinite Elves and Infinite Houses ---

; To keep the Elves busy, Santa has them deliver some presents by hand,
; door-to-door. He sends them down a street with infinite houses numbered
; sequentially: 1, 2, 3, 4, 5, and so on.

; Each Elf is assigned a number, too, and delivers presents to houses based on
; that number:

;   - The first Elf (number 1) delivers presents to every house: 1, 2, 3, 4, 5,
;     ....
;   - The second Elf (number 2) delivers presents to every second house: 2, 4,
;     6, 8, 10, ....
;   - Elf number 3 delivers presents to every third house: 3, 6, 9, 12, 15,
;     ....

; There are infinitely many Elves, numbered starting with 1. Each Elf delivers
; presents equal to ten times his or her number at each house.

; So, the first nine houses on the street end up like this:

; House 1 got 10 presents.
; House 2 got 30 presents.
; House 3 got 40 presents.
; House 4 got 70 presents.
; House 5 got 60 presents.
; House 6 got 120 presents.
; House 7 got 80 presents.
; House 8 got 150 presents.
; House 9 got 130 presents.

; The first house gets 10 presents: it is visited only by Elf 1, which delivers
; 1 * 10 = 10 presents. The fourth house gets 70 presents, because it is
; visited by Elves 1, 2, and 4, for a total of 10 + 20 + 40 = 70 presents.

; What is the lowest house number of the house to get at least as many presents
; as the number in your puzzle input?

(defn factors
  "Return the list of factors of a given n. Taken from
  http://rosettacode.org/wiki/Factors_of_an_integer#Clojure"
  [n]
  (set (mapcat (fn [x] [x (/ n x)])
               (filter #(zero? (rem n %)) (range 1 (inc (Math/sqrt n)))))))

(defn num-presents [house-num]
  (reduce + (map (partial * 10) (factors house-num))))

(assert (= (num-presents 4) 70))

(assert (= (first (drop-while (fn [[index n]]
                                (> 121 n))
                              (map-indexed (fn [index n]
                                             [index (num-presents n)])
                                           (range 99))))
           [8 150]))

; This solution is far from clever or optimized: it's just brute force... but
; it works, in just a few lines of code.

; The following takes less than 30 seconds on a recent (2015) mbp.
(defn part1 []
  (println
    "The lowest house number of the house to get at least as the input is"
    (first (drop-while (fn [[index n]]
                         (> input n)) (map-indexed (fn [index n]
                                                     [index (num-presents n)])
                                                   (range))))))


; --- Part Two ---

; The Elves decide they don't want to visit an infinite number of houses.
; Instead, each Elf will stop after delivering presents to 50 houses. To make
; up for it, they decide to deliver presents equal to eleven times their number
; at each house.

; With these changes, what is the new lowest house number of the house to get
; at least as many presents as the number in your puzzle input?


(defn elf-num-filter
  "Given a house number, only return true if the given elf has visited less
  than 50 houses."
  [house-num elf-num]
  (>= (* 50 elf-num) house-num))

(assert (elf-num-filter 50 1))
(assert (not (elf-num-filter 51 1)))
(assert (elf-num-filter 100 2))
(assert (not (elf-num-filter 101 2)))

(defn new-num-presents [house-num]
  (reduce + (map (partial * 11) (filter
                                  (partial elf-num-filter house-num)
                                  (factors house-num)))))

(assert (= (new-num-presents 4) 77))
(assert (= (new-num-presents 49) 627))
(assert (= (new-num-presents 51) 781))

; The following takes less than 30 seconds on a recent (2015) mbp.
(defn part2 []
  (println
    "The new lowest house number of the house to get at least as the input is"
    (first (drop-while (fn [[index n]]
                         (> input n))
                       (map-indexed
                         (fn [index n]
                           [index (new-num-presents n)])
                         (range))))))
