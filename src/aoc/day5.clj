; http://adventofcode.com/day/5
(ns aoc.day5)

(def input (clojure.string/split-lines (slurp "src/aoc/day5.input")))

; --- Day 5: Doesn't He Have Intern-Elves For This? ---

; Santa needs help figuring out which strings in his text file are naughty or
; nice.

; A nice string is one with all of the following properties:

;   - It contains at least three vowels (aeiou only), like aei, xazegov, or
;     aeiouaeiouaeiou.
;   - It contains at least one letter that appears twice in a row, like xx,
;     abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
;   - It does not contain the strings ab, cd, pq, or xy, even if they are part
;     of one of the other requirements.

; For example:

;   - ugknbfddgicrmopn is nice because it has at least three vowels
;     (u...i...o...), a double letter (...dd...), and none of the disallowed
;     substrings.
;   - aaa is nice because it has at least three vowels and a double letter,
;     even though the letters used by different rules overlap.
;   - jchzalrnumimnmhp is naughty because it has no double letter.
;   - haegwjzuvuyypxyu is naughty because it contains the string xy.
;   - dvszwmarrgswjxmb is naughty because it contains only one vowel.

; How many strings are nice?

(defn three-vowels? [s]
  (<= 3 (count (clojure.string/replace s #"[^aeiou]" ""))))

(defn string-contains? [s needles]
  "Given a set of 2 char needles, returns non nil if the string contains at
  least one."
  (let [; All the pairs in the string: "abcd" => ["ab" "bc" "cd"]
        partitioned (partition 2 1 s)]
    (some needles partitioned)))

(defn double-letter? [s]
  (let [letters "abcdefghijklmnopqrstuvwxyz"
        ; #{[\a \a] [\b \b] [\c \c] ...}
        pairs (set (partition 2 (interleave letters letters)))]
    (string-contains? s pairs)))

(defn forbidden? [s]
  (let [forbidden-pairs #{[\a \b] [\c \d] [\p \q] [\x \y]}]
    (string-contains? s forbidden-pairs)))

(defn nice? [s]
  (and (three-vowels? s)
       (double-letter? s)
       (not (forbidden? s))))

(assert (nice? "ugknbfddgicrmopn"))
(assert (nice? "aaa"))
(assert (not (nice? "jchzalrnumimnmhp")))
(assert (not (nice? "haegwjzuvuyypxyu")))
(assert (not (nice? "dvszwmarrgswjxmb")))

(defn num-nice [input]
  (count (filter nice? input)))

(defn part1 []
  (println "There are"
           (num-nice input)
           "nice strings"))


; --- Part Two ---

; Realizing the error of his ways, Santa has switched to a better model of
; determining whether a string is naughty or nice. None of the old rules apply,
; as they are all clearly ridiculous.

; Now, a nice string is one with all of the following properties:

;   - It contains a pair of any two letters that appears at least twice in the
;     string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not
;     like aaa (aa, but it overlaps).
;   - It contains at least one letter which repeats with exactly one letter
;     between them, like xyx, abcdefeghi (efe), or even aaa.

; For example:

;   - qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj)
;     and a letter that repeats with exactly one letter between them (zxz).
;   - xxyxx is nice because it has a pair that appears twice and a letter that
;     repeats with one between, even though the letters used by each rule
;     overlap.
;   - uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with
;     a single letter between them.
;   - ieodomkazucvgmuy is naughty because it has a repeating letter with one
;     between (odo), but no pair that appears twice.

; How many strings are nice under these new rules?


(defn pairs [s]
  "Return all the pairs found in a string."
  ; (re-seq #"(..).*\1" "aaxaa") => (["aaxaa" "aa"])
  (re-seq #"(..).*\1" s))

(defn repeating-letters [s]
  "Return all the pairs of letters with a single letter separating them."
  ; (re-seq #"(.).\1" "abab") => (["aba" "a"])
  (re-seq #"(.).\1" s))

(defn new-nice? [s]
  (and (pairs s)
       (repeating-letters s)))

(assert (new-nice? "qjhvhtzxzqqjkmpb"))
(assert (new-nice? "xxyxx"))
(assert (not (new-nice? "uurcxstgmygtbstg")))
(assert (not (new-nice? "ieodomkazucvgmuy")))

(defn num-new-nice [input]
  (count (filter new-nice? input)))

(defn part2 []
  (println "There are"
           (num-new-nice input)
           "nice strings under the new rules"))
