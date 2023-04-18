(ns app.renderer.view
  (:require [reagent.core  :as reagent  :refer [atom cursor]]
            [re-frame.core :as re-frame :refer [dispatch subscribe]]))

;;; Load svg helper
(defn transform [& {:keys [x y s]}]
  {:transform (str (if (and x y) (str "translate(" x " " y ") ") "")
                   (if s         (str "scale(" s ")")            ""))})

;;; Function to draw board
;;; Info and Data
(defonce SELECTED-ID (atom nil))        ; 当前选中的节点 ID
(defonce SELECTED-PORT (atom nil))      ; 当前选中的端口 [node-id port-id]
(defonce SCALE (atom 30))               ; 当前画布的缩放大小
(defonce INFO-PAN (atom nil))           ; 是否绘制消息面板
(defonce TR-X (atom 0))
(defonce TR-Y (atom 0))

(defonce CLASS (atom {
                      :Number {
                               :class :Number
                               :param {:num 0}
                               :color :orange
                               :in-pos  {}
                               :out-pos {:val [1 0.5]}
                               :in    []
                               :out   {:val 0}
                               :func  (fn [{:keys [num]}] { :val num })
                               }
                      :Add    {
                               :class   :Add
                               :param   {}
                               :color   :black
                               :in-pos  {:a [0 0.25] :b [0 0.75]}
                               :out-pos {:val [1 0.5]}
                               :in      {:a 0 :b 0}
                               :out     {:val 0}
                               :func    (fn [{:keys [a b]}] { :val (+ a b) })
                               }
                      }))
(defonce NODES (atom {}))               ; 储存节点信息
(defonce ARCS  (atom #{}))              ; 储存边信息

(defonce DRAWED-NODES (atom '()))        ; 绘制的节点结果
(defonce DRAWED-ARCS  (atom '()))        ; 绘制的边结果

;;; Add/Delete Node
(defn del-node [id]
  (swap! NODES dissoc id))

(defn add-node [& {:keys [name type x y]}]
  (let [id (random-uuid)
        {:keys [param in out func
                in-pos out-pos color]} (@CLASS type)]
    (swap! NODES assoc id {:class type
                           :name  name
                           :param param
                           :pos-x x
                           :pos-y y
                           :in    in
                           :out   out
                           :func  func
                           :in-pos  in-pos
                           :out-pos out-pos
                           :color   color})))

(defn random-name []
  "name")

;;; Add/Delete Arcs
(defn add-arc [from-node from-port to-node to-port]
  (swap! ARCS conj [from-node from-port to-node to-port]))

(defn delete-arc [from-node from-port to-node to-port]
  (swap! ARCS disj [from-node from-port to-node to-port]))

(defn finde-arc [{:keys [from-node from-port to-node to-port]}]
  (some @ARCS (fn [[f-n f-p t-n t-p]]
                (and (or (nil? from-node) (= from-node f-n))
                     (or (nil? from-port) (= from-port f-p))
                     (or (nil? to-node)   (= to-node   t-n))
                     (or (nil? to-port)   (= to-port   t-p))))))

;;; Nodes
;;; Draw nodes
(defn draw-node-arc [{:keys [x1 y1 x2 y2]}]
        (fn [{:keys [x1 y1 x2 y2]}]
          (let [weight (min 5 (abs (* -0.1 (- y2 y1) (- x2 x1))))]
            [:svg/path {:d (str "M" x1 " " y1 " "
                            "C" (+ x1 weight) " " y1 ", "
                            (- x2 weight) " " y2 ", "
                            x2 " " y2)
                        :stroke :black
                        :stroke-width 0.1
                        :fill :none}])))
(defn draw-node-port [{:keys [id port x y]}]
  (let [select-port (fn [node-id port-id mouse]
                      (condp = mouse.button
                        0 (reset! SELECTED-PORT [node-id port-id])
                        '()))]
   (fn [{:keys [x y]}]
     [:circle {:cx x
               :cy y
               :r  0.1
               :stroke :black
               :stroke-width 0.05
               :fill (let [[node-id port-id] @SELECTED-PORT]
                       (if (and (= node-id id)
                                (= port-id port))
                         :orange
                         :white))
               :on-mouse-down #(select-port id port %)}])))
(defn draw-node-body [id {:keys [x y]}]
  (let [start-move (fn [node mouse]
                     (condp = mouse.button
                       0 (do
                           (reset! INFO-PAN nil)
                           (reset! SELECTED-ID node))
                       2 (do
                           (reset! INFO-PAN {:x mouse.clientX
                                             :y mouse.clientY
                                             :type :node
                                             :info node}))
                       '()))
        move-node  (fn [id mouse]
                     (when (= id @SELECTED-ID)
                       (reset!
                        NODES
                        (-> @NODES
                            (update-in [id :pos-x]
                                       #(+ % (/ mouse.movementX @SCALE)))
                            (update-in [id :pos-y]
                                       #(+ % (/ mouse.movementY @SCALE)))))))
        end-move   (fn []
                     (reset! SELECTED-ID nil))]
    (fn [id {:keys [x y]}]
      [:g (transform :x x :y y)
       [:rect {:width 1
              :height 1
              :fill @(cursor NODES [id :color])
              :on-mouse-down  #(start-move id %)
              :on-mouse-move  #(move-node id %)
              :on-mouse-leave end-move
              :on-mouse-up    end-move}]])))

(defn draw-node [id node]
  (fn []
    (println :draw-node id)
    (let [x (get-in @NODES [id :pos-x])
          y (get-in @NODES [id :pos-y])]
      [:g
       ;; draw body
       ^{:key (str id "body")} [draw-node-body id {:x x :y y}]

       ;; draw in port
       (for [[port [dx dy]] (node :in-pos)]
         ^{:key (str id "in" port)} [draw-node-port {:id   id
                                                     :port port
                                                     :x    (+ x dx)
                                                     :y    (+ y dy)}])

       ;; draw out port
       (for [[port [dx dy]] (node :out-pos)]
         ^{:key (str id "out" port)} [draw-node-port {:id   id
                                                      :port port
                                                      :x    (+ x dx)
                                                      :y    (+ y dy)}])])))

(add-watch NODES
           :redraw
           #(do
              (reset! DRAWED-NODES
                      (for [[id node] @NODES]
                        ^{:key (str "node" id)} [draw-node id node]))
              (println :redraw)))

;;; Art-board
(defn draw-artboard [& nodes]
  ;; local closure variable
  (let [width     (atom 600)      height    (atom 300)
        scale     SCALE           dragging? (atom false)
        selected-id SELECTED-ID]
    ;; predefine functions
    (let [resize-artboard  (fn [mouse]
                             (.stopPropagation mouse)
                             (reset!
                              scale
                              (max 10 (min 100 (+ (* 0.05 mouse.deltaY) @scale)))))
          start-artboard   (fn [mouse]
                             (.stopPropagation mouse)
                             (condp = mouse.button
                               0 (do
                                   (reset! INFO-PAN nil)
                                   (reset! dragging? true))
                               2 (do
                                   (reset! INFO-PAN  {:x mouse.clientX
                                                      :y mouse.clientY
                                                      :type :add}))
                               '()))
          moving-artboard  (fn [mouse]
                             (.stopPropagation mouse)
                             (when @dragging?
                               (reset! TR-X (+ @TR-X mouse.movementX))
                               (reset! TR-Y (+ @TR-Y mouse.movementY))))
          stop-artboard    (fn [mouse]
                             (reset! dragging? false))]
      (fn [node]
        [:g
         ;; Mask
         [:mask#art-board-background-mask
          [:rect {:width  @width
                  :height @height
                  :fill   :white
                  :stroke :black
                  :stroke-width 3}]]
         ;; Artboard
         [:g {:transform "translate(10 10)"}
          ;; background
          [:rect {:width  @width
                  :height @height
                  :fill   :white
                  :stroke :black
                  :stroke-width 3
                  :on-wheel       resize-artboard
                  :on-mouse-down  start-artboard
                  :on-mouse-move  moving-artboard
                  :on-mouse-leave stop-artboard
                  :on-mouse-up    stop-artboard}]
          ;; nodes
          [:g {:mask "url(#art-board-background-mask)"}
           [:g (transform :x @TR-X :y @TR-Y :s @scale)
            nodes]]]]))))

(defn draw-board []
  (let [nodes DRAWED-NODES]
    (fn []
      [draw-artboard
       (for [[from-node from-port to-node to-port] @ARCS]
         ^{:key (str from-node "-p-" from-port "-t-" to-node "-p-" to-port)}
         (let [[x1 y1] (get-in @NODES [from-port :out-pos from-port])
               [x2 y2] (get-in @NODES [to-port   :in-pos  to-port])]
           [draw-node-arc {:x1 x1   :y1 y1
                           :x2 x2   :y2 y2}]))
       @nodes
       ;; (for [[id node] @NODES]
       ;;   (do
       ;;     (println id)
       ;;     ^{:key (str "node" id)} [draw-node id node]))
       ])))

;;; Info pan
(defn draw-add-pan [info]
  (let [search (atom "")]
    (let [update-value #(reset! search (-> % .-target .-value))
          make-new-node (fn [type mouse]
                          (let [x (/ (- mouse.clientX @TR-X) @SCALE)
                                y (/ (- mouse.clientY @TR-Y) @SCALE)]
                            (reset! INFO-PAN nil)
                            (add-node {:name (random-name)
                                       :type type
                                       :x x
                                       :y y})))]
     (fn []
       [:div
        [:div.info-title {:style {:background "#CCC"
                                  :padding "3px"}}
         "Add Node"]
        [:div.input-field {:style {:padding "3px"}}
         [:input {:style {:width "60%"}
                 :value @search
                 :placeholder "Class"
                 :on-change update-value}]]
        (for [[type _] @CLASS]
          ^{:key (str "i-p-s-" type)}
          [:div.type-select {:style {:padding "3px"}
                             :on-click #(make-new-node type %)}
           (str type)])]))))
(defn draw-node-pan [info]
  (fn []
    (let [{:keys [param color name]} @(cursor NODES [info])]
      [:div
       [:div.info-title {:style {:background color
                                 :padding "3px"}}
        name
        ]])))

(defn draw-info-pan []
  (let [width  150
        height 200
        rect   [:rect {:width  width
                   :height height
                   :fill   :white
                   :fill-opacity 0.5
                   :stroke :black
                   :stroke-width 2}]]
    (fn []
      (when-not (nil? @INFO-PAN)
        (let [{:keys [type x y info]} @INFO-PAN]
          [:g (transform :x x :y y)
           [:mask#info-pan-mask rect]
           rect
           [:foreignObject {:mask "url(info-pan-mask)"
                            :width width
                            :height height}
            [:div {:style {:overflow-y :scroll
                           :width "100%"
                           :height "100%"
                           :margin 0
                           :padding 0}}
             (condp = type
                :add  [draw-add-pan info]
                :node [draw-node-pan info]
                '())]]])))))

(defonce WIDTH (atom js/window.innerWidth))
(defonce HEIGHT (atom js/window.innerHeight))

(defn main "Main View." []
  [:svg {:width  @WIDTH
         :height @HEIGHT
         :style {:background "#EEE"}}
   [draw-board]
   [draw-info-pan]])
