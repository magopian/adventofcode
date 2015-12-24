; http://adventofcode.com/day/7
(ns aoc.day7)

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input (clojure.string/split-lines (slurp "src/aoc/day7.input")))

; --- Day 7: Some Assembly Required ---

; This year, Santa brought little Bobby Tables a set of wires and bitwise logic
; gates! Unfortunately, little Bobby is a little under the recommended age
; range, and he needs help assembling the circuit.

; Each wire has an identifier (some lowercase letters) and can carry a 16-bit
; signal (a number from 0 to 65535). A signal is provided to each wire by a
; gate, another wire, or some specific value. Each wire can only get a signal
; from one source, but can provide its signal to multiple destinations. A gate
; provides no signal until all of its inputs have a signal.

; The included instructions booklet describes how to connect the parts
; together: x AND y -> z means to connect wires x and y to an AND gate, and
; then connect its output to wire z.

; For example:

;   - 123 -> x means that the signal 123 is provided to wire x.
;   - x AND y -> z means that the bitwise AND of wire x and wire y is provided
;     to wire z.
;   - p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and
;     then provided to wire q.
;   - NOT e -> f means that the bitwise complement of the value from wire e is
;     provided to wire f.

; Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If,
; for some reason, you'd like to emulate the circuit instead, almost all
; programming languages (for example, C, JavaScript, or Python) provide
; operators for these gates.

; For example, here is a simple circuit:

; 123 -> x
; 456 -> y
; x AND y -> d
; x OR y -> e
; x LSHIFT 2 -> f
; y RSHIFT 2 -> g
; NOT x -> h
; NOT y -> i

; After it is run, these are the signals on the wires:

; d: 72
; e: 507
; f: 492
; g: 114
; h: 65412
; i: 65079
; x: 123
; y: 456

; In little Bobby's kit's insructions booklet (provided as your puzzle input),
; what signal is ultimately provided to wire a?

(def bit-two-complement (partial - 65535))

(def operators {"AND" bit-and
                "OR" bit-or
                "LSHIFT" bit-shift-left
                "RSHIFT" bit-shift-right
                "NOT" bit-two-complement})

(defn coerce [x]
  "Coerce x to a number, a function, or a keyword."
  (cond
    (re-matches #"\d+" x) (Integer. x)  ; "123" => 123
    (operators x) (operators x)  ; "AND" => bit-and
    :else (keyword x)))  ; "af" => :af

(assert (= (coerce "123") 123))
(assert (= (coerce "AND") bit-and))
(assert (= (coerce "af") :af))

(defn parse-gates [input]
  "Convert from ['af' 'AND' 'ah'] to [AND :af :ah]."
  (let [coerced (map coerce input)]
    (condp = (count coerced)
      1 (first coerced)  ; A number or another wire.
      2 [(first coerced) (last coerced)]  ; [NOT :af].
      3 (let [[wire1 gate wire2] coerced]  ; [:af bit-and :ah]
          [gate wire1 wire2]))))

(assert (= (parse-gates ["af" "AND" "ah"]) [bit-and :af :ah]))
(assert (= (parse-gates ["NOT" "ah"]) [bit-two-complement :ah]))
(assert (= (parse-gates ["123"]) 123))

(defn parse-wires [input-line]
  "Convert from 'af AND ah -> ai' to {:ai [bit-and :af :ah]}."
  (let [splitted (clojure.string/split input-line #" -> ")
        k (keyword (last splitted))  ; :ai
        v (clojure.string/split (first splitted) #" ")]  ; ["af" "AND" "ah"]
    {k (parse-gates v)}))

(assert (= (parse-wires "af AND ah -> ai") {:ai [bit-and :af :ah]}))
(assert (= (parse-wires "NOT ah -> ai") {:ai [bit-two-complement :ah]}))
(assert (= (parse-wires "123 -> ai") {:ai 123}))

(defn connect-wires [input]
  (reduce (fn [circuit line]
            (conj circuit (parse-wires line)))
          {}
          input))

(assert (= (connect-wires ["af AND ah -> ai" "NOT ah -> aj" "123 -> ak"])
           {:ai [bit-and :af :ah]
            :aj [bit-two-complement :ah]
            :ak 123}))

(def input-circuit (connect-wires input))
(def circuit (atom input-circuit))

(defn resolve-wire [wire]
  (if (number? wire)
    wire
    (let [inputs (wire @circuit)
          result
          (cond
            (number? inputs) inputs
            (keyword? inputs) (resolve-wire inputs)
            (= 2 (count inputs)) (bit-two-complement
                                   (resolve-wire (last inputs)))
            :else (let [[gate wire1 wire2] inputs]
                    (gate (resolve-wire wire1) (resolve-wire wire2))))]
      (swap! circuit assoc wire result)  ; Update the circuit ("reduce" it).
      result)))


(def test-circuit (connect-wires ["123 -> x"
                                  "456 -> y"
                                  "x AND y -> d"
                                  "x OR y -> e"
                                  "x LSHIFT 2 -> f"
                                  "y RSHIFT 2 -> g"
                                  "NOT x -> h"
                                  "NOT y -> i"]))

(reset! circuit test-circuit)
(assert (= (resolve-wire :d) 72))
(assert (= (resolve-wire :e) 507))
(assert (= (resolve-wire :f) 492))
(assert (= (resolve-wire :g) 114))
(assert (= (resolve-wire :h) 65412))
(assert (= (resolve-wire :i) 65079))
(assert (= (resolve-wire :x) 123))
(assert (= (resolve-wire :y) 456))

(reset! circuit input-circuit)

(defn part1 []
  (println "The signal is"
           (resolve-wire :a)))


; --- Part Two ---

; Now, take the signal you got on wire a, override wire b to that signal, and
; reset the other wires (including wire a). What new signal is ultimately
; provided to wire a?

(reset! circuit input-circuit)
(swap! circuit assoc :b 956)

(defn part2 []
  (println "The new signal is"
           (resolve-wire :a)))
