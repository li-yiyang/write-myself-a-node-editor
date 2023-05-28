(ns app.nodes.core
  (:require [app.parser.core :as parser :refer [calculate]]
            [app.fun.helpers            :refer [map-map]]
            [emmy.env        :as math]))

;;; Core CLASS
(def node-class-core-atom (atom {}))

(defn make-node-class [class-name & {:keys [logic renderer rendered parameters in out]}]
  {
   :logic      logic                    ; logic should be a function
   :class-name class-name               ;
   :renderer   renderer
   :rendered   rendered
   :parameters parameters
   :in         in
   :out        out
   })

(defn make-logic-func [func]
  (cond (string? func) {'result (calculate func)} ; "1 + x"
        (fn?     func) {'result func}   ; user provide
        (map?    func) (map-map #'make-logic-func func) ; multi-inputs
        :else          (calculate "0")  ; fallback
        ))

(defn make-logic-next [next keys]
  (cond (keyword? next) { (symbol next) true } ; 
        (symbol?  next) { next true }
        (or (= next :all) (nil? next)) (->> keys
                                            (map (fn [sym] [sym true]))
                                            (into {}))
        (vector?  next) (->> next
                             (filter #(contains? keys %))
                             (map (fn [sym] [sym true]))
                             (into {}))
        (map? next)     (->> next
                             (filter (fn [[k -]] (contains? keys k)))
                             (map (fn [[k v]] [(symbol k)
                                               (cond (fn? v) v
                                                     :else true)]))
                             (into {})) ; {:out-port (fn [...] ...)}
        :else (throw "Bad next sepcification.")))

(defn make-class-logic [{:keys [func next]}]
  (let [logic-func (make-logic-func func)
        logic-next (make-logic-next next (keys logic-func))]
    {:func logic-func
     :next logic-next}))
(defn make-class-parameters [paras]
  (cond (symbol?  paras) {paras {:type "expr"}}
        (keyword? paras) {(symbol paras) {:type "expr"}}
        (vector?  paras) (into {}       ; [:x [:y {:type ...}]]
                               (map #(cond (symbol?  %) [% {:type "expr"}]
                                           (keyword? %) [(symbol paras) {:type "expr"}]
                                           (vector?  %) [(symbol (first %)) (last %)]
                                           :else (throw "Bad Parameters"))
                                    paras))
        :else {}))
(defn make-class-in [ins]
  (make-class-parameters ins))
(defn make-class-out [logic]
  (if (= (logic :next) :all)
    (into {} (map (fn [sym] [sym true]) (keys logic)))
    ;; to-do....
    (into {} (map (fn [sym] nil)))))
(defn make-class-renderer [opts]
  (fn [render-opts] [:rect {:width 1
                            :height 1}]))
(defn render-nodes [renderer opts]
  (renderer opts))

(defn ^private add-class [name & {:keys [logic renderer parameters in]}]
  (let [class-logic       (make-class-logic      logic)
        class-parameters  (make-class-parameters parameters)
        class-in          (make-class-in         in)
        class-out         (make-class-out        class-logic)
        class-renderer    (make-class-renderer   renderer)
        class-rendered    (render-nodes class-renderer {:in         class-in
                                                        :out        class-out
                                                        :parameters class-parameters
                                                        })]
    (swap! node-class-core-atom
           #'assoc (symbol name)
           (make-node-class name
                            :logic      class-logic
                            :renderer   class-renderer
                            :parameters class-parameters
                            :in         class-in
                            :out        class-out))))

(defn make-nodes [])
