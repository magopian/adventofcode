; http://adventofcode.com/day/17
(ns aoc.day17)


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input (clojure.string/split-lines (slurp "src/aoc/day17.input")))

; --- Day 17: No Such Thing as Too Much ---

; The elves bought too much eggnog again - 150 liters this time. To fit it all
; into your refrigerator, you'll need to move it into smaller containers. You
; take an inventory of the capacities of the available containers.

; For example, suppose you have containers of size 20, 15, 10, 5, and 5 liters.
; If you need to store 25 liters, there are four ways to do it:

;   - 15 and 10
;   - 20 and 5 (the first 5)
;   - 20 and 5 (the second 5)
;   - 15, 5, and 5

; Filling all containers entirely, how many different combinations of
; containers can exactly fit all 150 liters of eggnog?

; Idea: can't use permutations, which would be easier, but there's far too many
; of them. So be cleverer: recursively pick containers. Use maps (keyed by
; index) to hold the containers, this way we don't have to care about multiple
; containers with the same size, and it's easier to "remove" a container from
; the list. Use a set to hold the combinations so we don't end up with
; duplicates (just in a different order).
(def int-to-key (comp keyword str))
(def containers (into {} (map-indexed #(vector (int-to-key %1) %2)
                             (map (fn [input-line]
                                    (Integer. input-line))
                                  input))))
(def test-containers (into {} (map-indexed #(vector (int-to-key %1) %2)
                                           [20 15 10 5 5])))

(defn smaller-containers [containers eggnog]
  "Return the containers that are smaller than the amount of eggnog."
  (into {} (filter #(> eggnog (last %)) containers)))

(assert (= (smaller-containers {:0 10, :1 5, :2 3} 6) {:1 5, :2 3}))
(assert (= (smaller-containers {:0 10, :1 5, :2 3} 3) {}))

(defn perfect-containers [containers eggnog]
  "Return the containers that are the perfect size for the amount of eggnog."
  (into {} (filter #(= eggnog (last %)) containers)))

(assert (= (perfect-containers {:0 10, :1 5, :2 5} 5) {:1 5, :2 5}))
(assert (= (perfect-containers {:0 10, :1 5, :2 3} 6) {}))

(defn find-combinations [used-containers containers eggnog]
  (let [solutions (vec (perfect-containers containers eggnog))
        ok-containers (smaller-containers containers eggnog)
        combinations (keep ; Keep only solutions.
                       identity
                       (mapcat
                         (fn [[n container]]
                           (find-combinations (conj used-containers [n container])
                                              (dissoc ok-containers n)
                                              (- eggnog container)))
                             ok-containers))
        all-solutions (concat (map vector solutions) combinations)]
    (if-not (empty? all-solutions)
      (map #(into used-containers %) all-solutions))))

(defn unique-combinations [combinations]
  (set (map set combinations)))

(assert (= (unique-combinations (find-combinations [] {:0 5 :1 5 :2 15} 15))
           #{#{[:2 15]}}))
(assert (= (unique-combinations (find-combinations [] test-containers 25))
           #{#{[:4 5] [:0 20]}
             #{[:2 10] [:1 15]}
             #{[:3 5] [:0 20]}
             #{[:4 5] [:3 5] [:1 15]}}))


; The following takes less than a minute on a recent (2015) mbp.
(defn part1 []
  (println
    "How many different combinations of containers can fit all 150l of eggnog:"
    (count (unique-combinations (find-combinations [] containers 150)))
    ))


; --- Part Two ---

; While playing with all the containers in the kitchen, another load of eggnog
; arrives! The shipping and receiving department is requesting as many
; containers as you can spare.

; Find the minimum number of containers that can exactly fit all 150 liters of
; eggnog. How many different ways can you fill that number of containers and
; still hold exactly 150 litres?

; In the example above, the minimum number of containers was two. There were
; three ways to use that many containers, and so the answer there would be 3.

(defn number-of-combinations-with-fewest-containers [combinations]
  (->> combinations
       ; Get a map with the key being the count, and the value being the list
       ; of combinations of that length.
       (group-by count)
       (into (sorted-map)) ; Order by increasing keys.
       first
       val
       count))

(assert (= (number-of-combinations-with-fewest-containers
             (unique-combinations (find-combinations [] test-containers 25)))
           3))

; The following takes less than a minute on a recent (2015) mbp.
(defn part2 []
  (println
    "How many different combinations of containers can fit all 150l of eggnog:"
    (number-of-combinations-with-fewest-containers
      (unique-combinations (find-combinations [] containers 150)))))
