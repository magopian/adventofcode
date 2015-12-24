; http://adventofcode.com/day/11
(ns aoc.day11)


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input "cqjxjnds")


; --- Day 11: Corporate Policy ---

; Santa's previous password expired, and he needs help choosing a new one.

; To help him remember his new password after the old one expires, Santa has
; devised a method of coming up with a password based on the previous one.
; Corporate policy dictates that passwords must be exactly eight lowercase
; letters (for security reasons), so he finds his new password by incrementing
; his old password string repeatedly until it is valid.

; Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so
; on. Increase the rightmost letter one step; if it was z, it wraps around to
; a, and repeat with the next letter to the left until one doesn't wrap around.

; Unfortunately for Santa, a new Security-Elf recently started, and he has
; imposed some additional password requirements:

;   - Passwords must include one increasing straight of at least three letters,
;     like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd
;     doesn't count.
;   - Passwords may not contain the letters i, o, or l, as these letters can be
;     mistaken for other characters and are therefore confusing.
;   - Passwords must contain at least two different, non-overlapping pairs of
;     letters, like aa, bb, or zz.

; For example:

;   - hijklmmn meets the first requirement (because it contains the straight
;     hij) but fails the second requirement requirement (because it contains i
;     and l).
;   - abbceffg meets the third requirement (because it repeats bb and ff) but
;     fails the first requirement.
;   - abbcegjk fails the third requirement, because it only has one double
;     letter (bb).
;   - The next password after abcdefgh is abcdffaa.
;   - The next password after ghijklmn is ghjaabcc, because you eventually skip
;     all the passwords that start with ghi..., since i is not allowed.

; Given Santa's current password (your puzzle input), what should his next
; password be?

; Idea: use base conversion to easily "increment" the password. We can't use
; "normal" base conversion, because it includes the 10 digits that we don't
; want in our password, so we'll have to first transpose from letters to a
; 0-based alphabet (0 1 2 3 4 5 6 7 8 9 a b c d ... p) before using standard
; base conversion.

(def letters (map char (range (int \a) (inc (int \z)))))
(def int-chars (concat [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9] (take 16 letters)))

(def letters-to-zbased-table
  "Transposition table from letters to '0-based character'."
  (zipmap letters int-chars))

(def zbased-to-letters-table
  "Transposition table from '0-based character' to letters."
  (clojure.set/map-invert letters-to-zbased-table))

(defn transpose-to-zbased [letters]
  (apply str (map letters-to-zbased-table letters)))

(assert (= (transpose-to-zbased "abcz") "012p"))

(defn transpose-to-letters [zbased]
  (apply str (map zbased-to-letters-table zbased)))

(assert (= (transpose-to-letters "012p") "abcz"))

(defn password-to-int [password]
  (let [zbased (transpose-to-zbased password)]
    (Long/parseLong zbased 26)))

(assert (= (password-to-int "a") 0))
(assert (= (password-to-int "z") 25))
(assert (= (password-to-int "ba") 26))
(assert (= (password-to-int "bb") 27))
(assert (= (password-to-int "bz") 51))
(assert (= (password-to-int "ca") 52))

(defn int-to-password [int-password]
  "Convert an integer to its password value. We only want letters, so we need
  to add 10 to our base conversion. And the base used is 36 and not 26 (the
  number of letters in the alphabet)."
  (let [zbased (Long/toString int-password 26)]
    (transpose-to-letters zbased)))

(assert (= (int-to-password 0) "a"))
(assert (= (int-to-password 25) "z"))
(assert (= (int-to-password 26) "ba"))
(assert (= (int-to-password 27) "bb"))
(assert (= (int-to-password 51) "bz"))
(assert (= (int-to-password 52) "ca"))

(defn increment-password [password]
  (-> password
      password-to-int
      inc
      int-to-password))

(assert (= (increment-password "a") "b"))
(assert (= (increment-password "z") "ba"))

(defn has-iol? [password]
  "Does the password contain the letters i, o or l?"
  (some #{\i \o \l} password))

(assert (not (has-iol? "abcdefgh")))
(assert (has-iol? "abidefgh"))
(assert (has-iol? "abodefgh"))
(assert (has-iol? "abldefgh"))

(defn has-suit? [password]
  "Does the password contain three letters in a suit?"
  (let [bytes (map int password)
        triplets (partition 3 1 bytes)]
    (some #(= (range (first %) (+ 3 (first %))) %) triplets)
    )
  )

(assert (has-suit? "abccba"))
(assert (not (has-suit? "abbcba")))

(defn has-two-pairs? [password]
  "Does the password contain two different pairs of letters?"
  (let [pairs (re-seq #"(.)\1" password)]
    ; The "groups" must be different, eg [["aa" "a"] ["bb" "b"]]
    (>= (count (set pairs)) 2)))

(assert (has-two-pairs? "aabcddd"))
(assert (not (has-two-pairs? "aabcdaa")))

(defn legit-password? [password]
  "A password is legit only if it passes all three restrictions."
  (and (not (has-iol? password))
       (has-suit? password)
       (has-two-pairs? password)))

(assert (not (legit-password? "hijklmmn")))
(assert (not (legit-password? "abbceffg")))
(assert (not (legit-password? "abbcegjk")))
(assert (legit-password? "abcdffaa"))
(assert (legit-password? "ghjaabcc"))

(defn next-legit-password [password]
  "Find the next legit password by iteratively incrementing."
  (loop [pass password]
    (if (legit-password? pass)
      pass
      (recur (increment-password pass)))))

; Note: it should be "abcdffaa" (with the leading "a"), but as "a" is the
; equivalent of "0" for passwords, and as I didn't bother dealing with padding,
; then it's ommitted in the final result. It would matter if we had to
; increment "small" passwords starting with "a"s.
(assert (= (next-legit-password "abcdefgh") "bcdffaa"))
(transpose-to-zbased "ghijklmn")
; The following takes less than 30 seconds on a recent (2015) mbp.
; It could be optimized by skipping all the passwords containing the forbidden
; letters, and starting to count again from there. Eg with the following
; example: start counting from "ghjaaaaa".
; (assert (= (next-legit-password "ghijklmn") "ghjaabcc"))

; The following takes less than 2 seconds on a recent (2015) mbp.
(defn part1 []
  (println "The next password is"
           (next-legit-password input)))


; --- Part Two ---

; Santa's password expired again. What's the next one?

; The following takes less than 10 seconds on a recent (2015) mbp.
(defn part2 []
  (println "The next next password is"
           (next-legit-password
             (increment-password (next-legit-password input)))))
