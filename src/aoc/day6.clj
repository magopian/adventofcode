; http://adventofcode.com/day/6
(ns aoc.day6)

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input (clojure.string/split-lines (slurp "src/aoc/day6.input")))

(defn to-coordinates [s]
  (let [coords (clojure.string/split s #",")]
    (map #(Integer. %) coords)))

(assert (= (to-coordinates "887,9") [887 9]))

(defn normalize [[instruction coord1 coord2]]
  "Turn strings of coords to coords."
  [instruction
   (to-coordinates coord1)  ; top left of rectangle.
   (to-coordinates coord2)])  ; bottom rightof rectangle.

(assert (= (normalize ["on" "887,9" "959,629"])
           ["on" [887 9] [959 629]]))

(defn strip-useless [s]
  (clojure.string/replace s #"(turn |through )" ""))

(assert (= (strip-useless "turn on 887,9 through 959,629") "on 887,9 959,629"))

(defn to-instructions [lines]
  (->> lines  ; ["turn on 887,9 through 959,629" "..."]
      (map strip-useless)  ; "on 887,9 959,629"
      (map #(clojure.string/split % #" "))  ; ["on" "887,9" "959,629"]
      (map normalize)))  ; [on [887 9] [959 629]]

(assert (= (to-instructions ["turn on 887,9 through 959,629"
                             "turn off 123,123 through 234,456"
                             "toggle 321,123 through 234,456"])
           [["on" [887 9] [959 629]]
            ["off" [123 123] [234 456]]
            ["toggle" [321 123] [234 456]]]))

(def cleaned-input (to-instructions input))

; --- Day 6: Probably a Fire Hazard ---

; Because your neighbors keep defeating you in the holiday house decorating
; contest year after year, you've decided to deploy one million lights in a
; 1000x1000 grid.

; Furthermore, because you've been especially nice this year, Santa has mailed
; you instructions on how to display the ideal lighting configuration.

; Lights in your grid are numbered from 0 to 999 in each direction; the lights
; at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions
; include whether to turn on, turn off, or toggle various inclusive ranges
; given as coordinate pairs. Each coordinate pair represents opposite corners
; of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore
; refers to 9 lights in a 3x3 square. The lights all start turned off.

; To defeat your neighbors this year, all you have to do is set up your lights
; by doing the instructions Santa sent you in order.

; For example:

;   - turn on 0,0 through 999,999 would turn on (or leave on) every light.
;   - toggle 0,0 through 999,0 would toggle the first line of 1000 lights,
;     turning off the ones that were on, and turning on the ones that were off.
;   - turn off 499,499 through 500,500 would turn off (or leave off) the middle
;     four lights.

; After following the instructions, how many lights are lit?

(defn rectangle [[x1 y1] [x2 y2]]
  (set (for [x (range x1 (inc x2))
             y (range y1 (inc y2))]
         [x y])))

(assert (= (rectangle [0 0] [1 1]) #{[0 0] [0 1] [1 0] [1 1]}))

(defn on [lights rect]
  (clojure.set/union lights rect))

(assert (= (on #{} (rectangle [0 0] [1 1])) #{[0 0] [0 1] [1 0] [1 1]}))
(assert (= (on #{[0 0]} (rectangle [0 0] [1 1])) #{[0 0] [0 1] [1 0] [1 1]}))
(assert (= (on #{[2 2]} (rectangle [0 0] [1 1]))
           #{[2 2] [0 0] [0 1] [1 0] [1 1]}))

(defn off [lights rect]
  (clojure.set/difference lights rect))

(assert (= (off #{} (rectangle [0 0] [1 1])) #{}))
(assert (= (off #{[0 0] [2 2]} (rectangle [0 0] [1 1])) #{[2 2]}))
(assert (= (off #{[0 0] [0 1] [1 0] [1 1] [2 2]} (rectangle [0 0] [1 1]))
           #{[2 2]}))

(defn toggle [lights rect]
  (clojure.set/union (clojure.set/difference lights rect)
                     (clojure.set/difference rect lights)))

(assert (= (toggle #{} (rectangle [0 0] [1 0])) #{[0 0] [1 0]}))
(assert (= (toggle #{[0 0]} (rectangle [0 0] [1 0])) #{[1 0]}))
(assert (= (toggle #{[0 0] [1 0]} (rectangle [0 0] [1 0])) #{}))

(defn input-to-function [[instruction coord1 coord2]]
  [(resolve (symbol instruction))  ; Function in the ns with the given name.
   coord1
   coord2])

(assert (= (input-to-function ["on" [887 9] [959 629]])
           [#'aoc.day6/on [887 9] [959 629]]))

(defn illuminate [input]
  (let [input-with-functions (map input-to-function input)]
    (reduce (fn [lights [instruction top-left bottom-right]]
              (instruction lights (rectangle top-left bottom-right)))
            #{}
            input-with-functions)))

(defn num-lit [input]
  (count (illuminate input)))

(assert (= (num-lit [["on" [0 0] [1 1]]]) 4))
(assert (= (num-lit [["on" [0 0] [999 999]]]) 1000000))
(assert (= (num-lit [["toggle" [0 0] [999 0]]]) 1000))
(assert (= (num-lit [["off" [499 499] [500 500]]]) 0))
(assert (= (num-lit [["on" [499 499] [500 500]] ["off" [499 499] [500 500]]])
           0))
(assert (= (num-lit [["on" [498 498] [501 501]] ["off" [499 499] [500 500]]])
           12))

; This takes less than a minute on a recent (2015) mbp.
(defn part1 []
  (println "There are"
           (num-lit cleaned-input)
           "lit lights"))

; --- Part Two ---

; You just finish implementing your winning light pattern when you realize you
; mistranslated Santa's message from Ancient Nordic Elvish.

; The light grid you bought actually has individual brightness controls; each
; light can have a brightness of zero or more. The lights all start at zero.

; The phrase turn on actually means that you should increase the brightness of
; those lights by 1.

; The phrase turn off actually means that you should decrease the brightness of
; those lights by 1, to a minimum of zero.

; The phrase toggle actually means that you should increase the brightness of
; those lights by 2.

; What is the total brightness of all lights combined after following Santa's
; instructions?

; For example:

;   - turn on 0,0 through 0,0 would increase the total brightness by 1.
;   - toggle 0,0 through 999,999 would increase the total brightness by
;     2000000.


;; Idea is: For each instruction, you simply add this instruction to the list
;; of instruction for this coordinates (held in a map).
;; Then go through the map and run the instructions. At the end, sum all the
;; map values.
;; We could easily skip two iterations through the map by:
;; - updating the map value straight away instead of first adding the
;;   instructions
;; - updating a global "brightness" counter at the same time
;; Keeping it this way may be slower, but it would allow us greater
;; flexibility, decoupling and reusability if other "changes of instructions"
;; would arise. Eg if this solution had been used for the first part, the only
;; change for the second part would have been in the "run-instructions"
;; function.

(defn add-instruction-at-coord [instruction lights coord]
  "Add the given instruction to the list of instructions for the light at the
  specified coordinates."
  (update lights coord conj instruction))

(assert (= (add-instruction-at-coord "on" {} [0 0]) {[0 0] '("on")}))
(assert (= (add-instruction-at-coord "off" {[0 0] '("on")} [0 0])
           {[0 0] '("off" "on")}))
(assert (= (add-instruction-at-coord "off" {[0 0] '("on")} [0 1])
           {[0 0] '("on") [0 1] '("off")}))

(defn add-instructions [lights [instruction coord1 coord2]]
  "Call add-instruction for each coordinates in the rect."
  (reduce (partial add-instruction-at-coord instruction)
          lights
          (rectangle coord1 coord2)))

(assert (= (add-instructions {} ["on" [0 0] [1 1]])
           {[0 0] '("on") [0 1] '("on") [1 0] '("on") [1 1] '("on")}))
(assert (= (add-instructions {[0 0] '("on")} ["on" [0 0] [1 1]])
           {[0 0] '("on" "on")
            [0 1] '("on")
            [1 0] '("on")
            [1 1] '("on")}))
(assert (= (add-instructions {[2 2] '("on")} ["off" [0 0] [1 1]])
           {[2 2] '("on")
            [0 0] '("off")
            [0 1] '("off")
            [1 0] '("off")
            [1 1] '("off")}))

(defn run-instruction [current-brightness instruction]
  "Run the instruction and return the new brightness."
  (condp = instruction
    "on" (inc current-brightness)
    "off" (if (> current-brightness 0) (dec current-brightness) 0)
    "toggle" (inc (inc current-brightness))))

(assert (= (run-instruction 0 "on") 1))
(assert (= (run-instruction 1 "on") 2))
(assert (= (run-instruction 0 "off") 0))
(assert (= (run-instruction 1 "off") 0))
(assert (= (run-instruction 0 "toggle") 2))
(assert (= (run-instruction 1 "toggle") 3))

(defn run-instructions [instructions]
  "Run the list of instructions and return a brightness value."
  ;; We want to reverse the instructions because the order matters, and we
  ;; added the instructions in the reverse order. (conj '(1 2) 3) => '(3 1 2).
  (reduce run-instruction 0 (reverse instructions)))

(assert (= (run-instructions '("on" "on")) 2))
(assert (= (run-instructions '("off" "off")) 0))
(assert (= (run-instructions '("on" "off")) 1))
(assert (= (run-instructions '("off" "off" "on")) 0))
(assert (= (run-instructions '("on" "off" "off")) 1))

(defn new-num-lit [input]
  (reduce + (->> input
                (reduce add-instructions {})
                (map (fn [[coord instructions]]
                       (run-instructions instructions))))))

(assert (= (new-num-lit [["on" [0 0] [1 1]]]) 4))
(assert (= (new-num-lit [["on" [0 0] [999 999]]]) 1000000))
(assert (= (new-num-lit [["toggle" [0 0] [999 999]]]) 2000000))
(assert (= (new-num-lit [["toggle" [0 0] [999 0]]]) 2000))
(assert (= (new-num-lit [["off" [499 499] [500 500]]]) 0))
(assert (= (new-num-lit [["on" [499 499] [500 500]]
                         ["off" [499 499] [500 500]]])
           0))
(assert (= (new-num-lit [["on" [498 498] [501 501]]
                         ["off" [499 499] [500 500]]])
           12))

; This takes less than a minute on a recent (2015) mbp.
(defn part2 []
  (println "The total brightness is"
           (new-num-lit cleaned-input)))
