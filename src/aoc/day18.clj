; http://adventofcode.com/day/18
(ns aoc.day18)


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input (clojure.string/split-lines (slurp "src/aoc/day18.input")))

; --- Day 18: Like a GIF For Your Yard ---

; After the million lights incident, the fire code has gotten stricter: now, at
; most ten thousand lights are allowed. You arrange them in a 100x100 grid.

; Never one to let you down, Santa again mails you instructions on the ideal
; lighting configuration. With so few lights, he says, you'll have to resort to
; animation.

; Start by setting your lights to the included initial configuration (your
; puzzle input). A # means "on", and a . means "off".

; Then, animate your grid in steps, where each step decides the next
; configuration based on the current one. Each light's next state (either on or
; off) depends on its current state and the current states of the eight lights
; adjacent to it (including diagonals). Lights on the edge of the grid might
; have fewer than eight neighbors; the missing ones always count as "off".

; For example, in a simplified 6x6 grid, the light marked A has the neighbors
; numbered 1 through 8, and the light marked B, which is on an edge, only has
; the neighbors marked 1 through 5:

; 1B5...
; 234...
; ......
; ..123.
; ..8A4.
; ..765.

; The state a light should have next is based on its current state (on or off)
; plus the number of neighbors that are on:

;   - A light which is on stays on when 2 or 3 neighbors are on, and turns off
;     otherwise.
;   - A light which is off turns on if exactly 3 neighbors are on, and stays
;     off otherwise.

; All of the lights update simultaneously; they all consider the same current
; state before moving to the next.

; Here's a few steps from an example configuration of another 6x6 grid:

; Initial state:
; .#.#.#
; ...##.
; #....#
; ..#...
; #.#..#
; ####..

; After 1 step:
; ..##..
; ..##.#
; ...##.
; ......
; #.....
; #.##..

; After 2 steps:
; ..###.
; ......
; ..###.
; ......
; .#....
; .#....

; After 3 steps:
; ...#..
; ......
; ...#..
; ..##..
; ......
; ......

; After 4 steps:
; ......
; ......
; ..##..
; ..##..
; ......
; ......

; After 4 steps, this example has four lights on.

; In your grid of 100x100 lights, given your initial configuration, how many
; lights are on after 100 steps?


; Idea: this is Conway's game of life. We know how to implement that ;)

(def test-input [".#.#.#"
                 "...##."
                 "#....#"
                 "..#..."
                 "#.#..#"
                 "####.."])

(defn lights-from-input
  "Return the list of lights that are on ('#') from the input."
  [input]
  (set (remove nil?
               (apply concat (map-indexed (fn [y input-line]
                                            (map-indexed (fn [x light]
                                                           (if (= light \#)
                                                             [x y]))
                                                         input-line))
                                          input)))))

(assert (= (lights-from-input ["##." "#.#"]) #{[0 0] [1 0] [0 1] [2 1]}))

(defn neighbors-getter
  "Return a neighbors function, that returns the number of neighbors for a
  given light."
  [width height]
  (fn [[x y]]
    (for [dy [-1 0 1]
          dx [-1 0 1]
          :when (and (not= dx dy 0)  ; Exclude the current light.
                     (<= 0 (+ dx x))  ; Exclude lights that are off the grid.
                     (<= 0 (+ dy y))
                     (>= (dec width) (+ dx x))
                     (>= (dec height) (+ dy y)))]
      [(+ dx x) (+ dy y)])))

(def neighbors (neighbors-getter 100 100))
(def test-neighbors (neighbors-getter 6 6))

(assert (= (neighbors [1 1]) [[0 0] [1 0] [2 0]
                              [0 1]       [2 1]
                              [0 2] [1 2] [2 2]]))
(assert (= (neighbors [0 0]) [      [1 0]
                              [0 1] [1 1]]))
(assert (= (neighbors [99 99]) [[98 98] [99 98]
                                [98 99]]))

(defn stepper
  "Return a step function, that uses the three helper functions to 'step'."
  [neighbors turn-on? keep-on?]
  (fn [lights]
    ; The step function itself: takes a set of lights, return a set of lights.
    (let [light-neighbors (mapcat neighbors lights)
          ; Map of light locations and the number of time they are present in
          ; light-neighbors: this gives us the number of neighbors for each
          ; light.
          num-neighbors (frequencies light-neighbors)]
      (set (for [[light n] num-neighbors
                 :when (if (lights light) ; Is the light already on?
                         (keep-on? n) ; Light is on, keep it on?
                         (turn-on? n))] ; Light is off, should we turn it on?
             light)))))

(def conway-stepper (stepper neighbors #{3} #{2 3}))
(def test-conway-stepper (stepper test-neighbors #{3} #{2 3}))

(assert (= (test-conway-stepper #{[1 1]}) #{}))
(assert (= (test-conway-stepper #{      [0 1]
                                  [1 0] [1 1]})
                                #{[0 0] [0 1]
                                  [1 0] [1 1]}))

(defn display-lights
  "Not useful for the solution, but very handy for debugging visually ;)"
  [lights]
  (for [y (range 6)]
    (for [x (range 6)]
      (if (lights [x y])
        \#
        \.))))

(assert (= (last (take 5 (iterate test-conway-stepper
                                  (lights-from-input test-input))))
           #{[2 2] [2 3]
             [3 2] [3 3]}))


; The following takes less than 2 seconds on a recent (2015) mbp.
(defn part1 []
  (println
    "There are that many lights on after 100 steps:"
    (count (last (take 101 (iterate conway-stepper
                                    (lights-from-input input)))))))


; --- Part Two ---

; You flip the instructions over; Santa goes on to point out that this is all
; just an implementation of Conway's Game of Life. At least, it was, until you
; notice that something's wrong with the grid of lights you bought: four
; lights, one in each corner, are stuck on and can't be turned off. The example
; above will actually run like this:

; Initial state:
; ##.#.#
; ...##.
; #....#
; ..#...
; #.#..#
; ####.#

; After 1 step:
; #.##.#
; ####.#
; ...##.
; ......
; #...#.
; #.####

; After 2 steps:
; #..#.#
; #....#
; .#.##.
; ...##.
; .#..##
; ##.###

; After 3 steps:
; #...##
; ####.#
; ..##.#
; ......
; ##....
; ####.#

; After 4 steps:
; #.####
; #....#
; ...#..
; .##...
; #.....
; #.#..#

; After 5 steps:
; ##.###
; .##..#
; .##...
; .##...
; #.#...
; ##...#

; After 5 steps, this example now has 17 lights on.

; In your grid of 100x100 lights, given your initial configuration, but with
; the four corners always in the on state, how many lights are on after 100
; steps?

(def four-corners #{[0 0] [99 0] [0 99] [99 99]})
(def test-four-corners #{[0 0] [5 0] [0 5] [5 5]})

(defn conway-stepper-with-corners
  "A version of the conway stepper that always has the 4 corners 'on'."
  [lights]
  (let [lights (conway-stepper lights)]
    (clojure.set/union lights four-corners)))

(defn test-conway-stepper-with-corners [lights]
  (let [lights (test-conway-stepper lights)]
    (clojure.set/union lights test-four-corners)))

(assert (= (last (take 6 (iterate test-conway-stepper-with-corners
                                  (clojure.set/union
                                    (lights-from-input test-input)
                                    test-four-corners))))
           #{[0 0] [1 0]        [3 0] [4 0] [5 0]
                   [1 1] [2 1]              [5 1]
                   [1 2] [2 2]
                   [1 3] [2 3]
             [0 4]       [2 4]
             [0 5] [1 5]                    [5 5]}))

; The following takes less than 2 seconds on a recent (2015) mbp.
(defn part2 []
  (println
    "There are that many lights on after 100 steps with the four corners always
    on:"
    (count (last (take 101 (iterate conway-stepper-with-corners
                                    (clojure.set/union
                                      (lights-from-input input)
                                      four-corners)))))))
