(ns app.parser.core
  (:require [instaparse.core :as insta :refer-macros [defparser]]
            [clojure.string  :as str]))

;;; EBNF Rules for Simple Input
(def ebnf
  "EXPR   = SUM;
   SUM    = MUL (SPLIT ('+' | '-') SPLIT MUL)*;
   MUL    = POWER (SPLIT ('*' | '/') SPLIT POWER)*;
   POWER  = VALUE (SPLIT '^' SPLIT VALUE)*;
   VALUE  = SPLIT NUMBER SPLIT | SPLIT '(' SPLIT EXPR SPLIT ')' SPLIT;
   SPLIT  = #\"\\s*\";
   NUMBER = #\"[0-9]+(\\.[0-9]*)?|[0-9]*\\.[0-9]+\";
")

;;; Raw parse
(defparser parse-ast ebnf)

(defn eval-parsed [ast]
  ;; (println 'eval-parsed ast)
  (let [arg       (fn [ast-exp] (nth ast-exp 1))
        reduce-by (fn [ops ast-exp]
                    (loop [idx 5
                           val (eval-parsed (arg ast-exp))]
                      (if (< idx (count ast-exp))
                        (recur (+ idx 4) ((ops (nth ast-exp (- idx 2))) val (eval-parsed (nth ast-exp idx))))
                        val)))]
    (condp = (first ast)
      :EXPR   (eval-parsed (arg ast))
      :SUM    (reduce-by {"+" #'+ "-" #'-} ast)
      :MUL    (reduce-by {"*" #'+ "/" #'/} ast)
      :POWER  (reduce-by {"^" #'Math/pow}  ast)
      :VALUE  (if (= (count (rest ast)) 3)
                (eval-parsed (nth ast 2))
                (eval-parsed (nth ast 4)))
      :NUMBER (js/Number (arg ast))
      nil)))

(defn parse "Parse Input." [input]
  (-> input
      (parse-ast)
      (eval-parsed)))
