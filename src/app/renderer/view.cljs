(ns app.renderer.view
  (:require [reagent.core        :as reagent     :refer [atom cursor]]
            [app.fun.randomname                  :refer [random-name]]
            [app.parser.core     :as parser      :refer [parse]]))

;;; JS Window Info
(defonce WIDTH (atom js/window.innerWidth))
(defonce HEIGHT (atom js/window.innerHeight))
(defonce SIDE-BAR-WIDTH (atom 200))

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
                      :Input {
                               :class :Input
                               :param {:exp ""}
                               :color :orange
                               :in-pos  {}
                               :out-pos {:val [1 0.5]}
                               :in    {}
                               :out   {:val 0}
                               :func  (fn [{:keys [exp]}]
                                        { :val exp })
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
                               :class   :Out
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
  ;; delete in arcs
  (doseq [[from-node from-port to-node to-port] (find-arc {:from-node id})]
    (delete-arc from-node from-port to-node to-port))

  ;; delete out arcs
  (doseq [[from-node from-port to-node to-port] (find-arc {:to-node id})]
    (delete-arc from-node from-port to-node to-port))

  ;; delete the node
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

;;; Node Evaluate
(defn eval-node [id]
  (let [class (cursor NODES [id :class])
        in    (cursor NODES [id :in])
        param (cursor NODES [id :param])
        out   (cursor NODES [id :out])]
    (let [func (cursor CLASS [@class :func])]
      ;; update-node-input
      (doseq [[port _] @in]
        (doseq [[from-node from-port _ _] (find-arc {:to-node id :to-port port})]
          (swap! in assoc port @(cursor NODES [from-node :out from-port]))))

      ;; eval out
      (reset! out (@func (conj @in @param))))))

(defonce EVAL-NODE-QUEUE (atom []))

(defn eval-graph-initial []
  (reset! EVAL-NODE-QUEUE
          (->> @NODES
               (filter (fn [[_ {class :class}]] (= class :Input)))
               (map (fn [[id _]] id)))))
(defn eval-graph-stepper []
  (when-not (empty? @EVAL-NODE-QUEUE)
    (let [[node & rest] @EVAL-NODE-QUEUE]
      (eval-node node)
      (reset! EVAL-NODE-QUEUE
              (concat rest
                      (map (fn [[_ _ from _]] from)
                           (find-arc {:from-node node})))))))

(defn eval-graph []
  (eval-graph-initial)
  (while (not (empty? @EVAL-NODE-QUEUE))
    (eval-graph-stepper)))

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
  (let [node-moved (atom false)]
    (let [start-move (fn [node mouse]
                       (condp = mouse.button
                         0 (do          ; left click: MOVE NODE
                             (reset! node-moved false)
                             (reset! INFO-PAN nil)
                             (reset! SELECTED-ID node))
                         2 (do          ; right click: OPEN INFO-PAN
                             (reset! INFO-PAN {:x mouse.clientX
                                               :y mouse.clientY
                                               :type :node
                                               :info node}))
                         '()))
          move-node  (fn [id mouse]
                       (when (= id @SELECTED-ID)
                         (reset! node-moved true)
                         (reset!
                          NODES
                          (-> @NODES
                              (update-in [id :pos-x]
                                         #(+ % (/ mouse.movementX @SCALE)))
                              (update-in [id :pos-y]
                                         #(+ % (/ mouse.movementY @SCALE)))))))
          end-move   (fn [node mouse]
                       (reset! SELECTED-ID nil)
                       (when (and (= mouse.movementX 0)
                                  (= mouse.movementY 0)
                                  node
                                  (not @node-moved))
                         (reset! INFO-PAN {:x mouse.clientX
                                           :y mouse.clientY
                                           :type :node
                                           :info node})
                         (reset! node-moved false)))]
      (fn [id {:keys [x y]}]
        [:g (conj (transform :x x :y y)
                  {
                   :on-mouse-down  #(start-move id %)
                   :on-mouse-move  #(move-node id %)
                   :on-mouse-leave #(end-move false %)
                   :on-mouse-up    #(end-move id %)})
         [:rect {:width 1
                 :height 1
                 :fill @(cursor NODES [id :color])}]
         [:text {:x 0
                 :y -0.05
                 :font-size 0.3}
          @(cursor NODES [id :name])]]))))

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
  (let [dragging? (atom false)]
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
          [:rect {:width  (- @WIDTH @SIDE-BAR-WIDTH (* 3 10))
                  :height (- @HEIGHT 20)
                  :fill   :white
                  :stroke :black
                  :stroke-width 3}]]
         ;; Artboard
         [:g {:transform "translate(10 10)"}
          ;; background
          [:rect {:width  (- @WIDTH @SIDE-BAR-WIDTH (* 3 10))
                  :height (- @HEIGHT 20)
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
(defn draw-pan-button [attrs]
  (fn [attrs]
    [:center.info-button-wrapper {:style {:padding "5px"}}
     [:button.info-button {:style {:width "100%"}
                           :on-click (attrs :on-click)}
      (attrs :label)]]))
(defn draw-pan-split [attrs]
  (fn [attrs]
    [:div.info-split {:style {:padding-top "5px"
                              :padding-bottom "2px"
                              :padding-left "5px"
                              :padding-right "5px"}}
     [:span.info-split-label (attrs :label)]
     [:hr.info-split-hr {:style {:margin-top "1px"
                                 :margin-bottom "0px"}}]]))
(defn draw-pan-form [attrs]
  (let [nop #()]
    (fn [attrs]
      [:div.info-item {:style {:margin-top "3px"
                               :margin-left "2px"
                               :margin-right "2px"}}
       [:span.info-label {:style {:width "30%"
                                  :padding "2px"
                                  :margin "0"
                                  :margin-right "3px"}}
        (attrs :label)]
       [:input.info-input {:style {:width "50%"
                                   :padding "2px"
                                   :margin "0"}
                           :placeholder (or (attrs :placeholder) "")
                           :value (or (attrs :value) "")
                           :disabled (boolean (attrs :disabled))
                           :on-blur (or (attrs :on-blur) nop)
                           :on-key-press (or (attrs :on-key-press) nop)
                           :on-change (or (attrs :on-change) nop)}]])))
(defn draw-pan-title [attrs]
  (fn [attrs]
    [:div.info-title {:style {:background (or (attrs :color) :grey)
                              :padding "5px"}}
     (attrs :label)]))
(defn draw-add-pan [info]
  (let [search (atom (random-name))]
    (let [rand-name-fun #(reset! search (random-name))
          update-value #(reset! search (-> % .-target .-value))
          make-new-node (fn [type mouse]
                          (let [x (/ (- mouse.clientX @TR-X) @SCALE)
                                y (/ (- mouse.clientY @TR-Y) @SCALE)]
                            (reset! INFO-PAN nil)
                            (add-node {:name @search
                                       :type type
                                       :x x
                                       :y y})))]
     (fn []
       [:div
        [draw-pan-title {:color "#CCC"
                         :label "Add Node"}]
        ^{:key :info-pan-form-split} [draw-pan-split {:label "Node Name"}]
        [draw-pan-form {:label "name"
                        :value @search
                        :on-change update-value}]

        ^{:key :info-pan-class-split} [draw-pan-split {:label "Classes"}]
        (for [[type _] @CLASS]
          ^{:key (str "i-p-s-" type)}
          [:div.type-select {:style {:padding "3px"}
                             :on-click #(make-new-node type %)}
           (str type)])]))))
(defn draw-node-pan [info]
  (let [editname? (atom false)
        buffer (atom false)]
    (let [update      (fn [node type arg input]
                        (swap!
                         (cursor NODES [node type])
                         #'assoc arg
                         (-> input .-target .-value)))
          delete-node (fn [node]
                        (reset! INFO-PAN nil)
                        (del-node node))
          enter-press (fn [node select arg key]
                        (condp = key.charCode
                          13  (do
                                (let [val (cursor NODES [node select arg])]
                                  (reset! val (parse @val)))
                                (eval-node node))
                          nil))]
     (fn [info]
       (let [{:keys [param in out color name]} @(cursor NODES [info])]
         (reset! editname? false)
         [:div
          [draw-pan-title {:color color
                           :label name}]

          ^{:key :info-pan-param-split} [draw-pan-split {:label "Parameters"}]
          (doall
           (for [[arg val] param]
             ^{:key (str "info-p-" arg)}
             [draw-pan-form {:label arg
                             :placeholder val
                             :value val
                             ; :on-blur #(eval-node info)
                             :on-key-press #(enter-press info :param arg %)
                             :on-change #(update info :param arg %)}]))
          ^{:key :info-pan-in-split} [draw-pan-split {:label "Inputs"}]
          (doall
           (for [[arg val] in]
             ^{:key (str "info-i-" arg)}
             [draw-pan-form {:label arg
                             :placeholder val
                             :value val
                             ; :on-blur #(eval-node info)
                             :on-key-press #(enter-press info :in arg %)
                             :on-change #(update info :in arg %)}]))

          ^{:key :info-pan-val-split} [draw-pan-split {:label "Results"}]
          (doall
           (for [[arg val] out]
             ^{:key (str "info-v-" arg)}
             [draw-pan-form {:label arg
                             :disabled true
                             :value val}]))

          [draw-pan-button {:on-click #(delete-node info)
                            :label "Delete Node"}]])))))
(defn draw-arc-pan [info]
  (let [delete (fn [from-node from-port to-node to-port]
                 (reset! INFO-PAN nil)
                 (delete-arc from-node from-port to-node to-port))]
    (fn [info]
      (let [[from-node from-port to-node to-port] info]
        [:div
         [draw-pan-title {:label (str from-port "->" to-port)}]

         [draw-pan-button {:label "Delete Arc"
                           :on-click #(delete from-node from-port to-node to-port)
                           }]]))))

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
               :add  ^{:key info} [draw-add-pan info]
               :node ^{:key info} [draw-node-pan info]
               :arc  ^{:key info} [draw-arc-pan info]
               nil)]]])))))

;;; Sidebar
(defn draw-sidebox-dbg []
  (fn []
    [:g
     [:foreignObject {:width @SIDE-BAR-WIDTH
                      :height (- @HEIGHT 20)}
      [:div
       [:h1 "Bad DBG"]

       (doall
        (for [[idx id] (map-indexed vector @EVAL-NODE-QUEUE)]
          ^{:key (str idx "sidebar-" id)} [:li @(cursor NODES [id :name])]))]]
     [:g (transform :x 10 :y (- @HEIGHT 60))
      ^{:key :sidebox-dbg-eval-graph} [:rect {:width 30
                                              :height 30
                                              :fill :green
                                              :stroke :black
                                              :stroke-width "2px"
                                              :on-click eval-graph}]
      ^{:key :sidebox-dbg-eval-graph-init} [:rect {:width 30
                                                      :height 30
                                                      :x 40
                                                      :fill :yellow
                                                      :stroke :black
                                                      :stroke-width "2px"
                                                      :on-click eval-graph-initial}]
      ^{:key :sidebox-dbg-eval-graph-step} [:rect {:width 30
                                                   :height 30
                                                   :x 80
                                                   :fill :red
                                                   :stroke :black
                                                   :stroke-width "2px"
                                                   :on-click eval-graph-stepper}]]]))

(defn draw-side-bar []
  (let [drag? (atom false)]
    (let [mouse-down #(reset! drag? true)
          mouse-leave #(reset! drag? false)
          mouse-move (fn [mouse]
                       (when @drag?
                         (if (< @SIDE-BAR-WIDTH 100)
                           (reset! SIDE-BAR-WIDTH 100)
                           (swap! SIDE-BAR-WIDTH - mouse.movementX))))]
      (fn []
        (let [sidebox [:rect {:width @SIDE-BAR-WIDTH
                              :height (- @HEIGHT 20)
                              :fill :white
                              :stroke :black
                              :stroke-width 3}]]
          [:g (transform :x (- @WIDTH @SIDE-BAR-WIDTH 20)
                         :y 10)
           [:rect {:width 10
                   :height (- @HEIGHT 20)
                   :fill :transparent
                   :stroke :none
                   :on-mouse-down mouse-down
                   :on-mouse-move mouse-move
                   :on-mouse-leave mouse-leave
                   :on-mouse-up mouse-leave}]
           [:g (transform :x 10 :y 0)
            [:mask.sidebox-mask sidebox]
            [:g {:mask "url(#sidebox-mask)"}
             sidebox
             [draw-sidebox-dbg]]]])))))

(defn main "Main View." []
  (let [handler (clj->js (fn []
                           (reset! WIDTH  (.-innerWidth js/window))
                           (reset! HEIGHT (.-innerHeight js/window))))]
    (let [_ (.addEventListener js/window "resize" handler)]
      (fn []
        [:svg {:width  @WIDTH
               :height @HEIGHT
               :style {:background "#CCC"}}
         [draw-board]
         [draw-info-pan]
         [draw-side-bar]]))))
