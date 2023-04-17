(ns app.renderer.view
  (:require [reagent.core  :as reagent  :refer [atom cursor]]
            [re-frame.core :as re-frame :refer [dispatch subscribe]]))

;;; Load svg helper
(defn transform [& {:keys [x y s]}]
  {:transform (str (if (and x y) (str "translate(" x " " y ") ") "")
                   (if s         (str "scale(" s ")")            ""))})

;;; Function to draw board
(defonce SELECTED-ID (atom nil))        ; 当前选中的节点 ID
(defonce SELECTED-PORT (atom nil))      ; 当前选中的端口 [node-id port-id]
(defonce SCALE (atom 30))               ; 当前画布的缩放大小
(defonce INFO-PAN (atom nil))          ; 是否绘制消息面板

(defonce NODES (atom {                  ; 绘制节点信息
               :A {
                   :id    :A
                   :color :orange
                   :type  :Number
                   :pos-x 0
                   :pos-y 1
                   :param { :num 1 }
                   :in    {}
                   :out   { :val [:C :a 0.5 0.25] }
                   }
               :B {
                   :id    :B
                   :color :orange
                   :type  :Number
                   :pos-x 1
                   :pos-y 1
                   :param { :num 2 }
                   :in    {}
                   :out   { :val [:C :a 0.5 0.75] }
                   }
               :C {
                   :id    :C
                   :color :black
                   :type  :Add
                   :pos-x 0
                   :pos-y 0
                   :param {}
                   :in    { :a [:A :val 0.25]
                            :b [:B :val 0.75] }
                   :out   { :val [nil nil 0.5 nil] }
                   }
               }))

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
                      (println :clicked @SELECTED-PORT)
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
                     (reset! SELECTED-ID node))
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
    (let [x (get-in @NODES [id :pos-x])
          y (get-in @NODES [id :pos-y])]
      [:g
       ;; draw node arc
       (doall
        (for [[port [to-node _ from-pos to-pos]] (node :out)
              :when (not (nil? to-node))]
          ^{:key (str id "arc" port)}
          [draw-node-arc {
                          :x1  (+ 1 x)
                          :y1  (+ from-pos y)
                          :x2  (get-in @NODES [to-node :pos-x])
                          :y2  (+ to-pos (get-in @NODES [to-node :pos-y]))}]))

       ;; draw body
       ^{:key (str id "body")} [draw-node-body id {:x x :y y}]

       ;; draw in port
       (for [[port [_ _ dy]] (node :in)]
          ^{:key (str id "in" port)} [draw-node-port {:id id
                                                      :port port
                                                      :x x
                                                      :y (+ y dy)}])
       ;; draw out port
       (for [[port [_ _ dy _]] (node :out)]
          ^{:key (str id "out" port)} [draw-node-port {:id id
                                                       :port port
                                                       :x (+ x 1)
                                                       :y (+ y dy)}])])))
(defn draw-artboard [& nodes]
  ;; local closure variable
  (let [width     (atom 600)      height    (atom 300)
        tr-x      (atom 0)        tr-y      (atom 0)
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
                               0 (reset! dragging? true)
                               2 (reset! INFO-PAN  [:add-node])
                               '()))
          moving-artboard  (fn [mouse]
                             (.stopPropagation mouse)
                             (when @dragging?
                               (reset! tr-x (+ @tr-x mouse.movementX))
                               (reset! tr-y (+ @tr-y mouse.movementY))))
          stop-artboard    (fn [mouse]
                             (reset! dragging? false))]
      (fn [node]
        [:g
         ;; Mask
         [:mask#art-board-background-mask
          [:rect {:width  @width
                  :height @height
                  :fill   :white}]]
         ;; Artboard
         [:g {:mask :art-board-background-mask}
          ;; background
          [:rect {:width  @width
                  :height @height
                  :fill   :white
                  :on-wheel       resize-artboard
                  :on-mouse-down  start-artboard
                  :on-mouse-move  moving-artboard
                  :on-mouse-leave stop-artboard
                  :on-mouse-up    stop-artboard}]
          ;; nodes
          [:g (transform :x @tr-x :y @tr-y :s @scale)
           nodes]]]))))

(defn draw-board []
  (fn []
      [:svg {:width 600
             :height 300
             :style {:border "2px solid black"}}
       [draw-artboard
        (for [[id node] @NODES]
          ^{:key (str "node" id)}
          [draw-node id node])]]))

(defn main "Main View." []
  [draw-board])
