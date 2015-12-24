; http://adventofcode.com/day/8
(ns aoc.day8)

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def input (clojure.string/split-lines (slurp "src/aoc/day8.input")))

; --- Day 8: Matchsticks ---

; Space on the sleigh is limited this year, and so Santa will be bringing his
; list as a digital copy. He needs to know how much space it will take up when
; stored.

; It is common in many programming languages to provide a way to escape special
; characters in strings. For example, C, JavaScript, Perl, Python, and even PHP
; handle special characters in very similar ways.

; However, it is important to realize the difference between the number of
; characters in the code representation of the string literal and the number of
; characters in the in-memory string itself.

; For example:

;   - "" is 2 characters of code (the two double quotes), but the string
;     contains zero characters.
;   - "abc" is 5 characters of code, but 3 characters in the string data.
;   - "aaa\"aaa" is 10 characters of code, but the string itself contains six
;     "a" characters and a single, escaped quote character, for a total of 7
;     characters in the string data.
;   - "\x27" is 6 characters of code, but the string itself contains just one -
;     an apostrophe ('), escaped using hexadecimal notation.

; Santa's list is a file that contains many double-quoted string literals, one
; on each line. The only escape sequences used are \\ (which represents a
; single backslash), \" (which represents a lone double-quote character), and
; \x plus two hexadecimal characters (which represents a single character with
; that ASCII code).

; Disregarding the whitespace in the file, what is the number of characters of
; code for string literals minus the number of characters in memory for the
; values of the strings in total for the entire file?

; For example, given the four strings above, the total number of characters of
; string code (2 + 5 + 10 + 6 = 23) minus the total number of characters in
; memory for string values (0 + 3 + 7 + 1 = 11) is 23 - 11 = 12.

(defn full-count [input]
  "Return the total count of code characters."
  (reduce + (map count input)))

(assert (= (full-count ["\"dwz\""]) 5))
(assert (= (full-count ["\"v\\xfb\\\"lgs\\\"kvjfywmut\\x9cr\""]) 28))

(defn replace-multi-chars [line]
  "Replace the 'multi-characters' by a single character.
  \\\\ => @
  \\\" => _
  \\x7e => *
  Also removes the first and last quotes."
  (let [stripped (apply str (drop-last (rest line)))]
    (-> stripped
        (clojure.string/replace #"\\\\" "@")
        (clojure.string/replace #"\\\"" "_")
        (clojure.string/replace #"\\x.." "*"))))

(assert (= (replace-multi-chars "\"dwz\"") "dwz"))
(assert (= (replace-multi-chars "\"v\\xfb\\\"lgs\\\"kvjfywmut\\x9cr\"")
           "v*_lgs_kvjfywmut*r"))

(defn char-count [input]
  "Return the number of in-memory characters."
  (reduce + (map count (map replace-multi-chars input))))

(assert (= (char-count ["\"dwz\""]) 3))
(assert (= (char-count ["\"v\\xfb\\\"lgs\\\"kvjfywmut\\x9cr\""]) 18))

(defn part1 []
  (println "The number of characters is"
           (- (full-count input) (char-count input))))


; --- Part Two ---

; Now, let's go the other way. In addition to finding the number of characters
; of code, you should now encode each code representation as a new string and
; find the number of characters of the new encoded representation, including
; the surrounding double quotes.

; For example:

;   - "" encodes to "\"\"", an increase from 2 characters to 6.
;   - "abc" encodes to "\"abc\"", an increase from 5 characters to 9.
;   - "aaa\"aaa" encodes to "\"aaa\\\"aaa\"", an increase from 10 characters to
;     16.
;   - "\x27" encodes to "\"\\x27\"", an increase from 6 characters to 11.

; Your task is to find the total number of characters to represent the newly
; encoded strings minus the number of characters of code in each original
; string literal. For example, for the strings above, the total encoded length
; (6 + 9 + 16 + 11 = 42) minus the characters in the original code
; representation (23, just like in the first part of this puzzle) is 42 - 23 =
; 19.

(defn encode-multi-chars [line]
  "Encode the 'multi-characters'.
  \\ => \\\\
  \" => \\\"
  \\x7e => \\\\x7e"
  (-> line
      (clojure.string/replace #"\\" "\\\\\\\\")
      (clojure.string/replace #"\"" "\\\\\"")
      ))

(assert (= (encode-multi-chars "\"dwz\"") "\\\"dwz\\\""))
(assert (= (encode-multi-chars "\"v\\xfb\\\"lgs\\\"kvjfywmut\\x9cr\"")
           "\\\"v\\\\xfb\\\\\\\"lgs\\\\\\\"kvjfywmut\\\\x9cr\\\"" ))

(defn encoded-char-count [input]
  "Return the number of encoded characters."
  (reduce + (map #(+ 2 (count %))  ;; Add 2 for beginning and end quotes.
                 (map encode-multi-chars input))))

(assert (= (encoded-char-count ["\"dwz\""]) 9))
(assert (= (encoded-char-count ["\"v\\xfb\\\"lgs\\\"kvjfywmut\\x9cr\""]) 38))
(assert (= (encoded-char-count ["\"\""
                                "\"abc\""
                                "\"aaa\\\"aaa\""
                                "\"\\x27\""]) 42))

(defn part2 []
  (println
    "The difference of characters between newly encoded and in the code is"
    (- (encoded-char-count input) (full-count input))))
