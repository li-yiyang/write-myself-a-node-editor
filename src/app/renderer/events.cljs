(ns app.renderer.events
  (:require [re-frame.core   :as re-frame :refer [reg-event-db reg-event-fx]]
            [app.renderer.db :as database :refer [default-db]]))

(reg-event-db
 :initialize-db
 (fn [_ _] default-db))

(reg-event-db
 :move-node
 (fn [db [_ id dx dy]]
   (let [x (get-in db [:nodes id :pos-x])
         y (get-in db [:nodes id :pos-y])]
    (-> db
        (assoc-in [:nodes id :pos-x] (+ dx x))
        (assoc-in [:nodes id :pos-y] (+ dy y))))))
