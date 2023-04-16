(ns app.renderer.view
  (:require [reagent.core  :as reagent  :refer [atom cursor]]
            [re-frame.core :as re-frame :refer [dispatch subscribe]]))

;;; Load svg helper
(defn transform [& {:keys [x y s]}]
  {:transform (str (if (and x y) (str "translate(" x " " y ") ") "")
                   (if s         (str "scale(" s ")")            ""))})

;;; Function to draw board
(defn board []
  (let [nodes        (subscribe [:nodes])
        node-classes (subscribe [:node-class])
        width        (atom 600)
        height       (atom 300)
        tr-x         (atom 0)
        tr-y         (atom 0)
        scale        (atom 30)
        dragging?    (atom false)
        seleced-id   (atom nil)]
    (let [resize-artboard (fn [event]
                            (.stopPropagation event)
                            (#(reset! scale (max 10 (min % 100)))
                             (+ (* 0.005 event.deltaY) @scale)))
          move-artboard   (fn [event]
                            (.stopPropagation event)
                            (when @dragging?
                              (reset! tr-x (+ @tr-x event.movementX))
                              (reset! tr-y (+ @tr-y event.movementY))))
          nomove-artboard #(reset! dragging? false)
          start-move-artb (fn [event]
                            (.stopPropagation event)
                            (condp = event.button
                              0 (reset! dragging? true)
                              '()))
          move-node        (fn [id event]
                             (.stopPropagation event)
                             (when (= id @seleced-id)
                               (dispatch [:move-node id
                                          (/ event.movementX @scale)
                                          (/ event.movementY @scale)])))
          end-move-n       #(reset! seleced-id nil)
          mouse-dow-n      (fn [id event]
                             (.stopPropagation event)
                             (reset! seleced-id id)
                             (condp = event.button
                               2 (println :left-key)
                               '()))]
      (fn []
        (let [node-width  1
              node-height 1
              rx          5]
          [:svg {:width  @width
                 :height @height
                 :style  {:border "3px solid black"}}
           ;; Artboard mask
           [:mask#board-background-mask
            [:rect {:width  @width
                    :height @height
                    :fill   :white}]]
           ;; Artboard
           [:g {:mask :board-background-mask}

            [:rect
             {
              :width  @width
              :height @height
              :fill   :white
              :on-wheel       resize-artboard
              :on-mouse-move  move-artboard
              :on-mouse-up    nomove-artboard
              :on-mouse-leave nomove-artboard
              :on-mouse-down  start-move-artb}]

            [:g (transform :x @tr-x :y @tr-y :s @scale)
             (for [[id node] @nodes]
               [:g (conj {:key id} (transform :x (node :pos-x) :y (node :pos-y)))
                [:rect
                 {
                  :width  node-width
                  :height node-height
                  :fill   (node :color)
                  :on-mouse-down  #(mouse-dow-n id %)
                  :on-mouse-move  #(move-node id %)
                  :on-mouse-leave end-move-n
                  :on-mouse-up    end-move-n
                 }]]
               )
             ]]])))))

(defn main "Main View." []
  [board])
