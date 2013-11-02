(ns jaderforth.core
  (:gen-class))

(use '[clojure.core.match :only (match)])

(defn parse [code]
  (reverse (into () (clojure.string/split code #"\s+"))))

(defn wordify [code]
  (defn sub [code words word-stack comment-depth]
    (defn append-token [word token]
      (assoc words word (conj (get words word []) token)))

    (match [comment-depth (first code)]
      [0 ":"]   (recur (rest (rest code)) (append-token (nth code 1) :word) (cons (nth code 1) word-stack) 0)
      [0 ":m"]  (recur (rest (rest code)) (append-token (nth code 1) :macro) (cons (nth code 1) word-stack) 0)
      [0 ";"]   (recur (rest code) words (rest word-stack) 0)
      [_ "("]   (recur (rest code) (append-token (first word-stack) "(") word-stack (+ comment-depth 1))
      [_ ")"]   (recur (rest code) (append-token (first word-stack) ")") word-stack (- comment-depth 1))
      [_ nil]   words
      :else (recur (rest code) (append-token (first word-stack) (first code)) word-stack 0)))
  (sub code {} '("main") 0))

(defn -main [& args]
  (wordify (parse ": someword ( foo bar ) baz :m test ( a b ) a b + ; hax ;\n5 6 test")))
