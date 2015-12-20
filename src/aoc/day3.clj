; http://adventofcode.com/day/3
(ns aoc.day3)

(def input (slurp "src/aoc/day3.input"))

; --- Day 3: Perfectly Spherical Houses in a Vacuum ---

; Santa is delivering presents to an infinite two-dimensional grid of houses.

; He begins by delivering a present to the house at his starting location, and
; then an elf at the North Pole calls him via radio and tells him where to move
; next. Moves are always exactly one house to the north (^), south (v), east
; (>), or west (<). After each move, he delivers another present to the house
; at his new location.

; However, the elf back at the north pole has had a little too much eggnog, and
; so his directions are a little off, and Santa ends up visiting some houses
; more than once. How many houses receive at least one present?

; For example:

;   - > delivers presents to 2 houses: one at the starting location, and one to
;     the east.
;   - ^>v< delivers presents to 4 houses in a square, including twice to the
;     house at his starting/ending location.
;   - ^v^v^v^v^v delivers a bunch of presents to some very lucky children at
;     only 2 houses.

(defn move [[x y] direction]
  (condp = direction
        \^ [x (inc y)]
        \> [(inc x) y]
        \v [x (dec y)]
        \< [(dec x) y]))

(defn visit [input]
  "Return the positions of all the houses visited, starting with the current."
  (reduce (fn [past-positions direction]
            (let [cur-pos (last past-positions)
                  new-pos (move cur-pos direction) ]
              (conj past-positions new-pos)))
          [[0 0]]  ; Starting position.
          input))

(defn how-many-houses [input]
  (count (set (visit input))))

(assert (= 2 (how-many-houses ">")))
(assert (= 4 (how-many-houses "^>v<")))
(assert (= 2 (how-many-houses "^v^v^v^v^v")))

(defn part1 []
  (println "This year,"
           (how-many-houses input)
           "houses receive at least one present"))


; --- Part Two ---

; The next year, to speed up the process, Santa creates a robot version of
; himself, Robo-Santa, to deliver presents with him.

; Santa and Robo-Santa start at the same location (delivering two presents to
; the same starting house), then take turns moving based on instructions from
; the elf, who is eggnoggedly reading from the same script as the previous
; year.

; This year, how many houses receive at least one present?

; For example:

;   - ^v delivers presents to 3 houses, because Santa goes north, and then
;     Robo-Santa goes south.
;   - ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up
;     back where they started.
;   - ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one
;     direction and Robo-Santa going the other.

(defn how-many-houses-with-robo [input]
  (let [santa-directions (take-nth 2 input)
        robo-directions (take-nth 2 (rest input))]
    (count (set (concat (visit santa-directions) (visit robo-directions))))))

(assert (= 3 (how-many-houses-with-robo "^v")))
(assert (= 3 (how-many-houses-with-robo "^>v<")))
(assert (= 11 (how-many-houses-with-robo "^v^v^v^v^v")))

(defn part2 []
  (println "This year,"
           (how-many-houses-with-robo input)
           "houses receive at least one present"))
