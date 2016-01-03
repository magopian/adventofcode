; http://adventofcode.com/day/23
(ns aoc.day23)


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input (clojure.string/split-lines (slurp "src/aoc/day23.input")))

; --- Day 23: Opening the Turing Lock ---

; Little Jane Marie just got her very first computer for Christmas from some
; unknown benefactor. It comes with instructions and an example program, but
; the computer itself seems to be malfunctioning. She's curious what the
; program does, and would like you to help her run it.

; The manual explains that the computer supports two registers and six
; instructions (truly, it goes on to remind the reader, a state-of-the-art
; technology). The registers are named a and b, can hold any non-negative
; integer, and begin with a value of 0. The instructions are as follows:

;   - hlf r sets register r to half its current value, then continues with the
;     next instruction.
;   - tpl r sets register r to triple its current value, then continues with
;     the next instruction.
;   - inc r increments register r, adding 1 to it, then continues with the next
;     instruction.
;   - jmp offset is a jump; it continues with the instruction offset away
;     relative to itself.
;   - jie r, offset is like jmp, but only jumps if register r is even ("jump if
;     even").
;   - jio r, offset is like jmp, but only jumps if register r is 1 ("jump if
;     one", not odd).

; All three jump instructions work with an offset relative to that instruction.
; The offset is always written with a prefix + or - to indicate the direction
; of the jump (forward or backward, respectively). For example, jmp +1 would
; simply continue with the next instruction, while jmp +0 would continuously
; jump back to itself forever.

; The program exits when it tries to run an instruction beyond the ones
; defined.

; For example, this program sets a to 2, because the jio instruction causes it
; to skip the tpl instruction:

; inc a
; jio a, +2
; tpl a
; inc a

; What is the value in register b when the program in your puzzle input is
; finished executing?

(def registers {:a 0 :b 0 :pos 0})

(def test-input ["inc a"
                 "jio a, +2"
                 "tpl a"
                 "inc a"])

(defn one? [x]
  (= 1 x))

(defn parse-instruction [instruction]
  (let [[instruction operand & more]
          (clojure.string/split instruction #",? ")]
    (condp = instruction
      "hlf" [(keyword operand) / 2]
      "tpl" [(keyword operand) * 3]
      "inc" [(keyword operand) + 1]
      "jmp" [:pos + (Integer. operand)]
      "jie" [:pos + [(keyword operand) even? (Integer. (first more))]]
      "jio" [:pos + [(keyword operand) one? (Integer. (first more))]]
      )))

(assert (= (parse-instruction "hlf a") [:a / 2]))
(assert (= (parse-instruction "tpl a") [:a * 3]))
(assert (= (parse-instruction "inc a") [:a + 1]))
(assert (= (parse-instruction "jmp +23") [:pos + 23]))
(assert (= (parse-instruction "jie a, +23") [:pos + [:a even? 23]]))
(assert (= (parse-instruction "jio a, +23") [:pos + [:a one? 23]]))
(assert (= (parse-instruction "jio a, -23") [:pos + [:a one? -23]]))

(def instructions (vec (map parse-instruction input)))
(def test-instructions (vec (map parse-instruction test-input)))

(defn get-offset [predicate offset register]
  (if (predicate register)
    offset
    0))

(assert (= (get-offset even? 23 1) 0))
(assert (= (get-offset one? 23 1) 23))
(assert (= (get-offset one? 23 3) 0))

(defn offset-from-vector
  "Return the offset depending on the content of a register."
  [registers [reg predicate amount]]
  (get-offset predicate amount (reg registers)))

(assert (= (offset-from-vector {:a 1} [:a one? 23]) 23))
(assert (= (offset-from-vector {:a 1} [:a even? 23]) 0))

(defn offset-for-jmp [registers amount]
  (let [offset (if (vector? amount)
                 (offset-from-vector registers amount)
                 amount)]
  (if (zero? offset) 1 offset)))

(assert (= (offset-for-jmp {:a 1 :pos 0} 23) 23))
(assert (= (offset-for-jmp {:a 1 :pos 0} -7) -7))
(assert (= (offset-for-jmp {:a 1 :pos 0} [:a one? 23]) 23))
(assert (= (offset-for-jmp {:a 1 :pos 0} [:a even? 23]) 1))

(defn run-instruction [[reg operator operand] registers]
  (if (= reg :pos)
    (update registers reg operator (offset-for-jmp registers operand))
    (update (update registers reg operator operand) :pos inc)
    ))

(assert (= (run-instruction [:a / 2] {:a 2 :pos 0}) {:a 1 :pos 1}))
(assert (= (run-instruction [:pos + [:a one? 23]] {:a 2 :pos 0})
           {:a 2 :pos 1}))
(assert (= (run-instruction [:pos + [:a even? 23]] {:a 2 :pos 0})
           {:a 2 :pos 23}))
(assert (= (run-instruction [:pos + [:a one? 23]] {:a 2 :pos 0})
           {:a 2 :pos 1}))

(defn run
  "Run all the instructions."
  [instructions registers]
  (let [pos (:pos registers)]
    (if (>= pos (count instructions))
      registers ; The end.
      (recur instructions (run-instruction (instructions pos) registers)))))

(assert (= (run test-instructions registers) {:a 2 :b 0 :pos 4}))


; The following takes (far) less than a second on a recent (2015) mbp.
(defn part1 []
  (println
    "The value in register 'b' is:"
    (:b (run instructions registers))))


; --- Part Two ---

; The unknown benefactor is very thankful for releasi-- er, helping little Jane
; Marie with her computer. Definitely not to distract you, what is the value in
; register b after the program is finished executing if register a starts as 1
; instead?

; The following takes (far) less than a second on a recent (2015) mbp.
(defn part2 []
  (println
    "The value in register 'b' if 'a' starts at 1 is:"
    (:b (run instructions (assoc registers :a 1)))))
