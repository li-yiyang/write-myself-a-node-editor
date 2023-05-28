(ns app.parser.core
  (:require [instaparse.core        :as insta :refer-macros [defparser]]
            [clojure.string         :as str]
            [sci.core               :as sci]
            [emmy.env               :as math]
            [emmy.expression.render :as exp-render]))

;;; EBNF Rules for Simple Input
;;; Calculator EBNF
(def ebnf
  "EXPR     = POWER;
   POWER    = MUL (SPLIT '^' SPLIT MUL)*;
   MUL      = SUM (SPLIT ('*' | '/') SPLIT SUM)*;
   SUM      = CMP (SPLIT ('+' | '-') SPLIT CMP)*;
   CMP      = VALUE (SPLIT ('<' | '<=' | '=' | '>=' | '>') SPLIT VALUE)*;
   VALUE    = SPLIT ELEMENT SPLIT | SPLIT '(' SPLIT EXPR SPLIT ')' SPLIT;
   ELEMENT  = LAMBDA | FUNC | FUNC-F | SYMBOL-V | NUMBER | EXPR;
   LAMBDA   = ('λ' | 'lambda' | 'fn') '(' SPLIT LIST-V SPLIT ').(' EXPR ')';
   FUNC     = SYMBOL-F '(' SPLIT ?[LIST-E] SPLIT ')'
   FUNC-F   = '(' SPLIT EXPR SPLIT ')(' SPLIT ?[LIST-E] SPLIT ')';
   LIST-V   = SYMBOL (SPLIT ',' SPLIT SYMBOL)*
   LIST-E   = ELEMENT (SPLIT ',' SPLIT ELEMENT)*
   SYMBOL-F = SYMBOL;
   SYMBOL-V = SYMBOL;
   SYMBOL   = #\"[a-zA-Z][a-zA-Z\\-]*\\??\";
   SPLIT    = #\"\\s*\";
   NUMBER   = #\"[0-9]+(\\.[0-9]*)?|[0-9]*\\.[0-9]+\";
")

;;; Raw parse
(defparser parse->ebnf-struct ebnf)

(defn ebnf-struct->ast [ast & {:keys [unquote] :or {unquote false}}]
  (let [arg (nth ast 1)
        args (rest ast)
        reduce-by (fn [ops content]
                    (loop [idx 4
                           val (ebnf-struct->ast arg :unquote unquote)]
                      (if (< idx (count content))
                        (recur
                         (+ idx 4)
                         (list (ops (nth content (- idx 2)))
                               val
                               (ebnf-struct->ast (nth content idx) :unquote unquote)))
                        val)))]
    (condp = (first ast)
      :EXPR     (ebnf-struct->ast arg :unquote unquote)
      :CMP      (reduce-by {"<"  '<
                            ">"  '>
                            ">=" '>=
                            "="  '=
                            "<=" '<=} args)
      :SUM      (reduce-by {"+" '+
                            "-" '-} args)
      :MUL      (reduce-by {"*" '*
                            "/" '/} args)
      :POWER    (reduce-by {"^" 'expt} args)
      :VALUE    (if (= (count args) 3)
                  (ebnf-struct->ast (nth args 1) :unquote unquote)
                  (ebnf-struct->ast (nth args 3) :unquote unquote))
      :ELEMENT  (ebnf-struct->ast arg :unquote unquote)
      :NUMBER   (js/Number arg)
      :SYMBOL   (symbol arg)
      :LAMBDA   (list 'fn (vec (ebnf-struct->ast (nth args 3) :unquote unquote))
                      (ebnf-struct->ast (nth args 6) :unquote true))
      :FUNC     (if (= (count args) 4)
                  (list (ebnf-struct->ast arg :unquote unquote))
                  (cons (ebnf-struct->ast arg :unquote unquote)
                        (ebnf-struct->ast (nth args 2) :unquote unquote)))
      :FUNC-F   (if (= (count args) 7)
                  (list (ebnf-struct->ast (nth args 2) :unquote unquote))
                  (cons (ebnf-struct->ast (nth args 2) :unquote unquote)
                        (ebnf-struct->ast (nth args 5) :unquote unquote)))
      :SYMBOL-F (ebnf-struct->ast arg)
      :SYMBOL-V (if unquote
                  (ebnf-struct->ast arg :unquote unquote)
                  (list 'quote (ebnf-struct->ast arg :unquote unquote)))
      :LIST-E   (map #(ebnf-struct->ast % :unquote unquote)
                     (map #'first (partition-all 4 args)))
      :LIST-V   (map #(ebnf-struct->ast % :unquote unquote)
                     (map #'first (partition-all 4 args)))
      nil)))

(def math-ctx (let [emmy-ns (sci/create-ns 'clojure.core)
                    namespace (sci/copy-ns emmy.env emmy-ns)
                    ]
                (sci/init {:namespaces {'clojure.core namespace}})))

(defn calculate [input]
  (let [ast (-> input (parse->ebnf-struct) (ebnf-struct->ast))]
    (if (= 'bad-input ast)
      'bad-input
      (try
        (sci/eval-form math-ctx ast)
        (catch js/Error e (println e) 'bad-calculation)))))

(def exp->raw
  (exp-render/make-infix-renderer   
   :precedence-map '{<  1
                     <= 1
                     =  1
                     >= 1
                     >  1
                     +  2
                     -  2
                     *  3
                     /  3
                     expt 4}
   :infix? '{+ - * / expt < <= = >= >}
   :special-handlers {'/ (fn [[num denom :as xs]]
                           (if (= 1 (count xs))
                             (str "1 / " num)
                             (str num " / " denom)))}))

(defn parse "Parse Input." [input]
  (try
    (exp->raw (calculate input))
    (catch js/Error e (println e) "")))
