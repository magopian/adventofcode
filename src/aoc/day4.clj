; http://adventofcode.com/day/4
(ns aoc.day4)

(def input "iwrupvqb")

; --- Day 4: The Ideal Stocking Stuffer ---

; Santa needs help mining some AdventCoins (very similar to bitcoins) to use as
; gifts for all the economically forward-thinking little girls and boys.

; To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
; least five zeroes. The input to the MD5 hash is some secret key (your puzzle
; input, given below) followed by a number in decimal. To mine AdventCoins, you
; must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...)
; that produces such a hash.

; For example:

;   - If your secret key is abcdef, the answer is 609043, because the MD5 hash
;     of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the
;     lowest such number to do so.
;   - If your secret key is pqrstuv, the lowest number it combines with to make
;     an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash
;     of pqrstuv1048970 looks like 000006136ef....

(defn digest [s]
  (.digest (doto (java.security.MessageDigest/getInstance "MD5")
                         (.update (.getBytes s)))))

(defn md5 [s]
  (apply str
         (map (partial format "%02x")
              (digest s))))

(defn coin? [num-zeroes secret-key number]
  (let [hashed (md5 (str secret-key number))
        zeroes (repeat num-zeroes \0)]
    (= (take num-zeroes hashed) zeroes)))

(def five-0-coin? (partial coin? 5))

(defn lowest-number [secret-key predicate]
  (first  ; Take the first number that was not dropped.
    (drop-while
      #(not (predicate secret-key %))  ; Drop until we have a good result.
      (range))))  ; Infinite range of numbers.

; This is really slow, uncomment at your own risk.
; (assert (= 609043 (lowest-number "abcdef")))
; (assert (= 1048970 (lowest-number "pqrstuv")))

; This takes less than 10 seconds on a recent (2015) mbp.
(println (lowest-number input five-0-coin?))


; --- Part Two ---

; Now find one that starts with six zeroes.

(def six-0-coin? (partial coin? 6))

; This takes less than 5 minutes on a recent (2015) mbp.
(println (lowest-number input six-0-coin?))
