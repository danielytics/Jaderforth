(ns jaderforth.core
  (:use [clojure.core.match :only [match]])
  (:gen-class))

(def default-input ": someword ( foo bar ) baz :m test + ; :m add-five 5 + ; hax ;\n1 add-five 6 test")

(defn tokenize
  "Split code into tokens. A token is any whitespace-separated collection of characters"
  [code]
  (clojure.string/split code #"\s+"))

(defn update [map key func data] (update-in map [key] (fnil func []) data))

(defn wordify
  "Convert a vector of tokens into a map of words to vector of tokens"
  [tokens]
  (first
    (reduce
      (fn [[words macros cur-word cur-type stack] token]
        (match [token]
          [":" ]  [words macros nil :func  (conj stack [cur-word cur-type])]
          [":m"]  [words macros nil :macro (conj stack [cur-word cur-type])]
          [";" ]  (let [[word type] (last stack)]
                    [words macros word type (pop stack)])
          :else   (if cur-word
                    (match [cur-type]
                      [:func ]  (let [[updater data]  (if (contains? macros token)
                                                        [(comp vec concat) (macros token)]
                                                        [conj token])]
                                  [(update words cur-word updater data) macros cur-word cur-type stack])
                      [:macro]  [words (update macros cur-word conj token) cur-word cur-type stack])
                    [words macros token cur-type stack])))
      [{} {} "main" :func []]
      tokens)))

(defn parse
  "Convert code into map of words to macro-expanded vector of tokens"
  [code]
  (wordify (tokenize code)))
  
(defn -main [& args]
  (parse (or (first args) default-input)))

