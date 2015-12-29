; http://adventofcode.com/day/19
(ns aoc.day19)


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input (clojure.string/split-lines (slurp "src/aoc/day19.input")))

(def molecule (last input))
(def replacements (drop-last (drop-last input)))


; --- Day 19: Medicine for Rudolph ---

; Rudolph the Red-Nosed Reindeer is sick! His nose isn't shining very brightly,
; and he needs medicine.

; Red-Nosed Reindeer biology isn't similar to regular reindeer biology; Rudolph
; is going to need custom-made medicine. Unfortunately, Red-Nosed Reindeer
; chemistry isn't similar to regular reindeer chemistry, either.

; The North Pole is equipped with a Red-Nosed Reindeer nuclear fusion/fission
; plant, capable of constructing any Red-Nosed Reindeer molecule you need. It
; works by starting with some input molecule and then doing a series of
; replacements, one per step, until it has the right molecule.

; However, the machine has to be calibrated before it can be used. Calibration
; involves determining the number of molecules that can be generated in one
; step from a given starting point.

; For example, imagine a simpler machine that supports only the following
; replacements:

; H => HO
; H => OH
; O => HH

; Given the replacements above and starting with HOH, the following molecules
; could be generated:

;   - HOOH (via H => HO on the first H).
;   - HOHO (via H => HO on the second H).
;   - OHOH (via H => OH on the first H).
;   - HOOH (via H => OH on the second H).
;   - HHHH (via O => HH).

; So, in the example above, there are 4 distinct molecules (not five, because
; HOOH appears twice) after one replacement from HOH. Santa's favorite
; molecule, HOHOHO, can become 7 distinct molecules (over nine replacements:
; six from H, and three from O).

; The machine replaces without regard for the surrounding characters. For
; example, given the string H2O, the transition H => OO would result in OO2O.

; Your puzzle input describes all of the possible replacements and, at the
; bottom, the medicine molecule for which you need to calibrate the machine.
; How many distinct molecules can be created after all the different ways you
; can do one replacement on the medicine molecule?

; Idea:
; 1/ split on the pattern: HOH => ["" "O" ""]
; 2/ count the number of resulting segments: 3
; 3/ that means we have 2 different replacements, so interleave with ["H" "HO"]
;    and ["HO" "H"]: only replace once each time (and use the initial pattern
;    for the rest).

(def test-replacements ["H => HO" "H => OH" "O => HH"])

(def replacement-pairs (map #(clojure.string/split % #" => ") replacements))
(def test-replacement-pairs (map #(clojure.string/split % #" => ")
                                 test-replacements))

(defn possible-replacements
  "Given a pattern, a replacement and the length, return the list of all the
  possible replacements."
  [pattern replacement length]
  (let [length (dec length)]  ; If there's X segments we want X-1 replacements.
    (for [x (range length)]
      (concat (apply concat
             (interpose [replacement]
                        (split-at x (repeat (dec length) pattern))))
              ; We "pad" the list with an empty string, or "interleave" will
              ; stop to early.
              [""]))))

(assert (= (possible-replacements "H" "HO" 3) [["HO" "H" ""] ["H" "HO" ""]]))
(assert (= (possible-replacements "O" "HH" 2) [["HH" ""]]))

(defn map-replace
  "Return a vector of the different molecules for the given replacement rule."
  [molecule [pattern replacement]]
  (let [segments (clojure.string/split molecule (re-pattern pattern) -1)]
    (map #(apply str (interleave segments %))
         (possible-replacements pattern replacement (count segments)))))

(assert (= (map-replace "HOH" ["H" "HO"]) ["HOOH" "HOHO"]))
(assert (= (map-replace "HOH" ["O" "HH"]) ["HHHH"]))

(defn map-replace-all [molecule replacement-pairs]
  (set (mapcat (partial map-replace molecule) replacement-pairs)))

(assert (= (map-replace-all "HOH" test-replacement-pairs)
           #{"HOOH" "HOHO" "OHOH" "HHHH"}))

; The following takes (far) less than a second on a recent (2015) mbp.
(defn part1 []
  (println
    "There are that many distinct molecules that can be created:"
    (count (map-replace-all molecule replacement-pairs))))


; --- Part Two ---

; Now that the machine is calibrated, you're ready to begin molecule
; fabrication.

; Molecule fabrication always begins with just a single electron, e, and
; applying replacements one at a time, just like the ones during calibration.

; For example, suppose you have the following replacements:

; e => H
; e => O
; H => HO
; H => OH
; O => HH

; If you'd like to make HOH, you start with e, and then make the following
; replacements:

;   - e => O to get O
;   - O => HH to get HH
;   - H => OH (on the second H) to get HOH

; So, you could make HOH after 3 steps. Santa's favorite molecule, HOHOHO, can
; be made in 6 steps.

; How long will it take to make the medicine? Given the available replacements
; and the medicine molecule in your puzzle input, what is the fewest number of
; steps to go from e to the medicine molecule?

; Idea: take the final molecule, and apply the "reversed replacements". Also,
; be as eager as possible: replace the biggest patterns first. For that,
; reverse everything (the molecule and the replacement patters).

; I failed :'(
; Tried the brute-force solution (applying reverse matches from the final
; molecule down to "e"), but it take far too long.
; Then tried the "eager" solution: replace the bigger patterns first, until
; getting down to "e", but instead got down to molecules with "e" inside.
; So went over to reddit, and saw
; https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju

(def num-elements (count (re-seq #"[A-Z]" molecule)))
(def num-parens (* 2 (count (re-seq #"Ar" molecule))))
(def num-commas (count (re-seq #"Y" molecule))) ; 6

; The following takes (far) less than a second on a recent (2015) mbp.
(defn part2 []
  (println
    "The fewest number of steps to go from e to the medecine molecule is"
    (- num-elements num-parens (* 2 num-commas) 1)))
