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
(defonce TR-X (atom 0))                 ; 画板 X 方向的位移量
(defonce TR-Y (atom 0))                 ; 画板 Y 方向的位移量

(defonce CLASS (atom {                  ; 节点类的信息
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
                      :Sub    {
                               :class   :Sub
                               :param   {}
                               :color   :grey
                               :in-pos  {:a [0 0.25] :b [0 0.75]}
                               :out-pos {:val [1 0.5]}
                               :in      {:a 0 :b 0}
                               :out     {:val 0}
                               :func    (fn [{:keys [a b]}] {:val (- a b)})
                               }
                      :Out    {
                               :class   :Sub
                               :param   {}
                               :color   :black
                               :in-pos  {:a [0 0.5]}
                               :out-pos {}
                               :in      {:a nil}
                               :out     {}
                               :func    (fn [{:keys [a]}]
                                          (println a)
                                          {})}
                      }))

(defonce NODES (atom {}))               ; 储存节点信息
(defonce ARCS  (atom #{}))              ; 储存边信息

(defonce DRAWED-NODES (atom '()))       ; 绘制的节点结果
(defonce DRAWED-ARCS  (atom '()))       ; 绘制的边结果

;;; Add/Delete Arcs
;;; Delete arc by [from-node from-port to-node to-port]
(defn delete-arc [from-node from-port to-node to-port]
  (swap! ARCS disj [from-node from-port to-node to-port]))

;;; Find arc(s) by {:from-node ... :from-port ... ...}
(defn find-arc [{:keys [from-node from-port to-node to-port]}]
  (filter (fn [[f-n f-p t-n t-p]]
            (and (or (nil? from-node) (= from-node f-n))
                 (or (nil? from-port) (= from-port f-p))
                 (or (nil? to-node)   (= to-node   t-n))
                 (or (nil? to-port)   (= to-port   t-p))))
          @ARCS ))

;;; Add arc by [from-node from-port to-node to-port]
(defn add-arc [from-node from-port to-node to-port]
  (cond
    (and (get-in @NODES [from-node :out-pos from-port])
         (get-in @NODES [to-node   :in-pos  to-port]))
    (do
      (doall (for [[f-n f-p t-n t-p] (find-arc {:to-node to-node
                                                :to-port to-port})]
               (delete-arc f-n f-p t-n t-p)))
      (swap! ARCS conj [from-node from-port to-node to-port]))

    (and (get-in @NODES [from-node :in-pos  from-port])
         (get-in @NODES [to-node   :out-pos to-port]))
    (do
      (doall (for [[f-n f-p t-n t-p] (find-arc {:to-node from-node
                                                :to-port from-port})]
               (delete-arc f-n f-p t-n t-p)))
      (swap! ARCS conj [to-node to-port from-node from-port]))))

;;; Add/Delete Node
;;; Delete Node
(defn del-node [id]
  (doall
   (for [[from-node from-port to-node to-port] (find-arc {:from-node id})]
     (do
       (println :delete-arc from-node from-port to-node to-port)
       (delete-arc from-node from-port to-node to-port))))
  (doall
   (for [[from-node from-port to-node to-port] (find-arc {:to-node id})]
     (do
       (println :delete-arc from-node from-port to-node to-port)
       (delete-arc from-node from-port to-node to-port))))
  (swap! NODES dissoc id))

;;; Add Node
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
                           :in-pos  in-pos
                           :out-pos out-pos
                           :color   color})))

(defn random-name []
  "name")

;;; Nodes
;;; Draw nodes
(defn draw-node-arc [{:keys [x1 y1 x2 y2 info]}]
  (let [select-arc (fn [info mouse]
                     (reset! INFO-PAN {:x mouse.clientX
                                       :y mouse.clientY
                                       :type :arc
                                       :info info}))]
   (fn [{:keys [x1 y1 x2 y2]}]
     (let [weight (min 6 (* 0.1 (abs (- y2 y1)) (max 8 (abs (- x2 x1)))))]
       [:path {:d (str "M" x1 " " y1 " "
                       "C" (+ x1 weight) " " y1 ", "
                       (- x2 weight) " " y2 ", "
                       x2 " " y2)
               :stroke :grey
               :stroke-width 0.12
               :fill :none
               :on-click #(select-arc info %)}]))))

(defn draw-arcs []
  (fn []
    [:g
     (doall
      (for [[from-node from-port to-node to-port] @ARCS]
        (let [x1 @(cursor NODES [from-node :pos-x])
              y1 @(cursor NODES [from-node :pos-y])
              x2 @(cursor NODES [to-node :pos-x])
              y2 @(cursor NODES [to-node :pos-y])
              [dx1 dy1] @(cursor NODES [from-node :out-pos from-port])
              [dx2 dy2] @(cursor NODES [to-node   :in-pos  to-port])]
          ^{:key (str "arc-" from-node from-port to-node to-port)}
          [draw-node-arc {:x1 (+ x1 dx1) :y1 (+ y1 dy1)
                          :x2 (+ x2 dx2) :y2 (+ y2 dy2)
                          :info [from-node from-port to-node to-port]}])))]))
(defn draw-node-port [{:keys [id port x y]}]
  (let [select-port (fn [node-id port-id mouse]
                      (condp = mouse.button
                        0 (if (nil? @SELECTED-PORT)
                            (reset! SELECTED-PORT [node-id port-id])
                            (let [[id2 port2] @SELECTED-PORT]
                              (reset! SELECTED-PORT nil)
                              (add-arc id2 port2 node-id port-id)))
                        nil))]

   (fn [{:keys [x y]}]
     [:circle {:cx x
               :cy y
               :r  0.12
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
      [:g (conj (transform :x x :y y)
                {
                 :on-mouse-down  #(start-move id %)
                 :on-mouse-move  #(move-node id %)
                 :on-mouse-leave end-move
                 :on-mouse-up    end-move})
       [:rect {:width 1
               :height 1
               :fill @(cursor NODES [id :color])}]])))

(defn draw-node [id node]
  (fn []
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

(defn draw-nodes []
  (fn []
    [:g
     (for [[id node] @NODES]
      ^{:key (str "node" id)} [draw-node id node])]))

;;; Art-board
(defn draw-artboard [& nodes]
  ;; local closure variable
  (let [width     (atom 600)      height    (atom 300)
        dragging? (atom false)]
    ;; predefine functions
    (let [resize-artboard  (fn [mouse]
                             (.stopPropagation mouse)
                             (reset!
                              SCALE
                              (max 10 (min 100 (+ (* 0.05 mouse.deltaY) @SCALE)))))
          start-artboard   (fn [mouse]
                             (.stopPropagation mouse)
                             (reset! SELECTED-PORT nil)
                             (condp = mouse.button
                               0 (do
                                   (reset! INFO-PAN nil)
                                   (reset! dragging? true))
                               2 (do
                                   (reset! INFO-PAN  {:x mouse.clientX
                                                      :y mouse.clientY
                                                      :type :add}))
                               nil))
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
           [:g (transform :x @TR-X :y @TR-Y :s @SCALE)
            nodes]]]]))))

(defn draw-board []
  (let [nodes DRAWED-NODES]
    (fn []
      [draw-artboard
       ^{:key :draw-arcs} [draw-arcs]
       ^{:key :draw-nodes} [draw-nodes]])))

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
  (let [params (atom {})
        name-f (atom "")
        editname? (atom false)]
    (let [update-param (fn [arg input]
                         #(reset!
                           params
                           (assoc @params arg (-> input .-target .-value))))
          update-name (fn [input]
                        #(reset! name-f (-> input .-target .-value)))
          delete-node (fn [node]
                        (reset! INFO-PAN nil)
                        (del-node node))]
     (fn []
       (let [{:keys [param color name]} @(cursor NODES [info])]
         (reset! name-f name)
         (reset! editname? false)
         (reset! params param)
         [:div
          [:div.info-title {:style {:background color
                                    :padding "3px"}}
           (if @editname?
             [:input.info-input {:value @name-f
                                 :style {:width "30%"
                                         :padding "2px"
                                         :margin "0"
                                         :margin-right "3px"}
                                 :on-change update-name}]
             [:span {:on-click #(println :clicked)}
              @name-f])]
          (for [[arg val] param]
            ^{:key (str "info-p-" arg)}
            [:div.info-item {:style {:margin-top "3px"
                                     :margin-left "2px"
                                     :margin-right "2px"}}
             [:span.info-label  {:style {:width "30%"
                                         :padding "2px"
                                         :margin "0"
                                         :margin-right "3px"}}
              arg]
             [:input.info-input {
                                 :style {:width "50%"
                                         :padding "2px"
                                         :margin "0"}
                                 :on-change update-param}]])
          [:button.info-button {:width "80%"
                                :on-click #(delete-node info)}
           "delete node"]])))))
(defn draw-arc-pan [info]
  (let [delete (fn [[from-node from-port to-node to-port]]
                 (reset! INFO-PAN nil)
                 (delete-arc from-node from-port to-node to-port))]
    (fn [info]
      [:div
       [:div.info-title {:style {:background :grey
                                 :padding "3px"}}]
       [:button.info-button {:width "80%"
                             :on-click #(delete info)}
        "delete arc"]])))

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
      (when-not (nil? @INFO-PAN)        ; draw when INFO-PAN is triggered
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
               :arc  [draw-arc-pan info]
               nil)]]])))))

(defonce WIDTH (atom js/window.innerWidth))
(defonce HEIGHT (atom js/window.innerHeight))

(defn main "Main View." []
  (fn []
     [:svg {:width  @WIDTH
            :height @HEIGHT
            :style {:background "#CCC"}}
      [draw-board]
      [draw-info-pan]]))
