; http://adventofcode.com/day/16
(ns aoc.day16)


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input (clojure.string/split-lines (slurp "src/aoc/day16.input")))

; --- Day 16: Aunt Sue ---

; Your Aunt Sue has given you a wonderful gift, and you'd like to send her a
; thank you card. However, there's a small problem: she signed it "From, Aunt
; Sue".

; You have 500 Aunts named "Sue".

; So, to avoid sending the card to the wrong person, you need to figure out
; which Aunt Sue (which you conveniently number 1 to 500, for sanity) gave you
; the gift. You open the present and, as luck would have it, good ol' Aunt Sue
; got you a My First Crime Scene Analysis Machine! Just what you wanted. Or
; needed, as the case may be.

; The My First Crime Scene Analysis Machine (MFCSAM for short) can detect a few
; specific compounds in a given sample, as well as how many distinct kinds of
; those compounds there are. According to the instructions, these are what the
; MFCSAM can detect:

;   - children, by human DNA age analysis.
;   - cats. It doesn't differentiate individual breeds.
;   - Several seemingly random breeds of dog: samoyeds, pomeranians, akitas,
;     and vizslas.
;   - goldfish. No other kinds of fish.
;   - trees, all in one group.
;   - cars, presumably by exhaust or gasoline or something.
;   - perfumes, which is handy, since many of your Aunts Sue wear a few kinds.

; In fact, many of your Aunts Sue have many of these. You put the wrapping from
; the gift into the MFCSAM. It beeps inquisitively at you a few times and then
; prints out a message on ticker tape:

; children: 3
; cats: 7
; samoyeds: 2
; pomeranians: 3
; akitas: 0
; vizslas: 0
; goldfish: 5
; trees: 3
; cars: 2
; perfumes: 1

; You make a list of the things you can remember about each Aunt Sue. Things
; missing from your list aren't zero - you simply don't remember the value.

; What is the number of the Sue that got you the gift?


(defn part1 []
  (println
    "The solution was found without code, simply by searching for 'cats: 7'."))


; --- Part Two ---

; As you're about to send the thank you note, something in the MFCSAM's
; instructions catches your eye. Apparently, it has an outdated
; retroencabulator, and so the output from the machine isn't exact values -
; some of them indicate ranges.

; In particular, the cats and trees readings indicates that there are greater
; than that many (due to the unpredictable nuclear decay of cat dander and tree
; pollen), while the pomeranians and goldfish readings indicate that there are
; fewer than that many (due to the modial interaction of magnetoreluctance).

; What is the number of the real Aunt Sue?

(defn parse [input-line]
  "Convert 'Sue 1: goldfish: 9, cars: 0, samoyeds: 9' to
  {goldfish 9
   cars 0
   samoyeds 9}."
  (let [keyvals (re-seq #"(\w+): (\d+)" input-line)
        only-captured (map rest keyvals)
        val-to-int (mapcat (fn [[k v]]
                          [k (Integer. v)])
                        only-captured)]
    (apply hash-map val-to-int)))

(assert (= (parse "Sue 1: goldfish: 9, cars: 0, samoyeds: 9")))

(def aunts-compounds (map parse input))

(defn filter-maybe [pred compound x aunt-compounds]
  "Return true if compound is found and 'pred' to x, or if not found at all."
  (if-let [amount (aunt-compounds compound)]
    (pred x amount)
    true)) ; If not found, still ok.

(def filter-equal (partial filter-maybe =))
(def filter-greater (partial filter-maybe <))
(def filter-fewer (partial filter-maybe >))

(def test-aunt-compounds {"goldfish" 1 "cars" 5 "samoyeds" 9})
(assert (filter-equal "cars" 5 test-aunt-compounds))
(assert (not (filter-equal "cars" 1 test-aunt-compounds)))
(assert (filter-equal "kids" 1 test-aunt-compounds))

(assert (filter-greater "cars" 4 test-aunt-compounds))
(assert (not (filter-greater "cars" 5 test-aunt-compounds)))

(assert (filter-fewer "cars" 6 test-aunt-compounds))
(assert (not (filter-fewer "cars" 5 test-aunt-compounds)))

(defn all-filters [aunt-compounds]
  (if (and ((partial filter-equal "children" 3) aunt-compounds)
           ((partial filter-greater "cats" 7) aunt-compounds)
           ((partial filter-equal "samoyeds" 2) aunt-compounds)
           ((partial filter-fewer "pomeranians" 3) aunt-compounds)
           ((partial filter-equal "akitas" 0) aunt-compounds)
           ((partial filter-equal "vizslas" 0) aunt-compounds)
           ((partial filter-fewer "goldfish" 5) aunt-compounds)
           ((partial filter-greater "trees" 3) aunt-compounds)
           ((partial filter-equal "cars" 2) aunt-compounds)
           ((partial filter-equal "perfumes" 1) aunt-compounds))
    aunt-compounds))


; The following takes (far) less than a second on a recent (2015) mbp.
(defn part2 []
  (println
    "The number of the real Aunt Sue is"
    ; We add 1 because the aunts are numbered from 1, not 0.
    (inc (first (keep-indexed #(if (all-filters %2) %1) aunts-compounds)))))
