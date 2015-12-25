; http://adventofcode.com/day/14
(ns aoc.day14)


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input (clojure.string/split-lines (slurp "src/aoc/day14.input")))

; --- Day 14: Reindeer Olympics ---

; This year is the Reindeer Olympics! Reindeer can fly at high speeds, but must
; rest occasionally to recover their energy. Santa would like to know which of
; his reindeer is fastest, and so he has them race.

; Reindeer can only either be flying (always at their top speed) or resting
; (not moving at all), and always spend whole seconds in either state.

; For example, suppose you have the following Reindeer:

;   - Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
;   - Dancer can fly 16 km/s for 11 seconds, but then must rest for 162
;     seconds.

; After one second, Comet has gone 14 km, while Dancer has gone 16 km. After
; ten seconds, Comet has gone 140 km, while Dancer has gone 160 km. On the
; eleventh second, Comet begins resting (staying at 140 km), and Dancer
; continues on for a total distance of 176 km. On the 12th second, both
; reindeer are resting. They continue to rest until the 138th second, when
; Comet flies for another ten seconds. On the 174th second, Dancer flies for
; another 11 seconds.

; In this example, after the 1000th second, both reindeer are resting, and
; Comet is in the lead at 1120 km (poor Dancer has only gotten 1056 km by that
; point). So, in this situation, Comet would win (if the race ended at 1000
; seconds).

; Given the descriptions of each reindeer (in your puzzle input), after exactly
; 2503 seconds, what distance has the winning reindeer traveled?

; Idea: construct two arrays (fly and rest), with the distance travelled during
; a second for each item in the arrays. So "fly 14 km/s for 10 seconds" is an
; array of ten elements, each of them being '14'. It's then followed by an
; array of x seconds of rest, each item being '0'.

(def test-input [
  "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
  "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
  ])

(defn parse [input-line]
  "Convert
  'Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.'
  to [14 10 127]."
  (map #(Integer. %) (re-seq #"\d+" input-line)))

(assert (= (parse "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.")
           [14 10 127]))

(def specs (map parse input))
(def test-specs (map parse test-input))

(defn get-travel-seconds [[speed fly-time rest-time] seconds]
  "Return an array with the distance to travel for each second."
  (let [fly-seconds (repeat fly-time speed)
        rest-seconds (repeat rest-time 0)
        travel-loop (cycle (concat fly-seconds rest-seconds))]
    (take seconds travel-loop)))

(assert (= (get-travel-seconds [10 2 5] 10) [10 10 0 0 0 0 0 10 10 0]))

(defn get-distance [spec seconds]
  "Given the specs of a reindeer, return the distance traveled after seconds."
  (let [travel-seconds (get-travel-seconds spec seconds)]
    (reduce + travel-seconds)))

(assert (= (get-distance (first test-specs) 1000) 1120))
(assert (= (get-distance (last test-specs) 1000) 1056))

; The following takes (far) less than a second on a recent (2015) mbp.
(defn part1 []
  (println
    "The winning reindeer traveled"
    (apply max (map #(get-distance % 2503) specs))))


; --- Part Two ---

; Seeing how reindeer move in bursts, Santa decides he's not pleased with the
; old scoring system.

; Instead, at the end of each second, he awards one point to the reindeer
; currently in the lead. (If there are multiple reindeer tied for the lead,
; they each get one point.) He keeps the traditional 2503 second time limit, of
; course, as doing otherwise would be entirely ridiculous.

; Given the example reindeer from above, after the first second, Dancer is in
; the lead and gets one point. He stays in the lead until several seconds into
; Comet's second burst: after the 140th second, Comet pulls into the lead and
; gets his first point. Of course, since Dancer had been in the lead for the
; 139 seconds before that, he has accumulated 139 points by the 140th second.

; After the 1000th second, Dancer has accumulated 689 points, while poor Comet,
; our old champion, only has 312. So, with the new scoring system, Dancer would
; win (if the race ended at 1000 seconds).

; Again given the descriptions of each reindeer (in your puzzle input), after
; exactly 2503 seconds, how many points does the winning reindeer have?

; Idea: keep track of the points for each reinder. Replace the reindeers (or
; their specs) by the arrays of 2503 items. Reduce over the total time
; to simulate a loop for each second. For each second, compute all the
; distances for all the reindeers up to this point in time, and increase the
; points for this reindeer.

; Idea: create an array of maps like the following (one map per reindeer):
; {:remaining-travel [<the distance to travel for each of the 2503 seconds>]
;  :travelled <the distance already travelled>
;  :score <the current score>}

(def scores (map (fn [specs]
                   {:remaining-travel (get-travel-seconds specs 2503)
                    :travelled 0
                    :score 0}) specs))
(def test-scores (map (fn [specs]
                        {:remaining-travel (get-travel-seconds specs 1000)
                         :travelled 0
                         :score 0}) test-specs))

(defn advance-time [scores]
  "Advance the time one second, and update the travelled distances."
  (map (fn [score]
         (update
           (update score :travelled + (first (:remaining-travel score)))
           :remaining-travel rest))
       scores))

(assert (= (advance-time [{:remaining-travel [10 0]
                          :travelled 1
                          :score 0}])
           [{:remaining-travel [0]
            :travelled 11
            :score 0}]))

(defn award-point [scores]
  "Award a point to the leading reindeer."
  (let [sorted-scores (vec (sort-by :travelled > scores))]
    (update-in sorted-scores [0 :score] inc)))

(assert (= (award-point [{:remaining-travel []
                          :travelled 1
                          :score 10}
                         {:remaining-travel []
                          :travelled 2
                          :score 5}])
           [{:remaining-travel []
             :travelled 2
             :score 6}
            {:remaining-travel []
             :travelled 1
             :score 10}]
           ))

(defn run-race [scores seconds]
  (reduce (fn [scores _]
            (award-point (advance-time scores)))
          scores
          (range seconds)))

(assert (= (run-race test-scores 1000)))

; The following takes (far) less than a second on a recent (2015) mbp.
(defn part2 []
  (println
    "The points for the winning reindeer is"
    (get-in (vec (sort-by :score > (run-race scores 2503))) [0 :score])
    ))
