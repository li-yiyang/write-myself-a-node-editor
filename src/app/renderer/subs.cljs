(ns app.renderer.subs
  (:require [re-frame.core :as re-frame :refer [reg-sub]]))

(reg-sub :node-class (fn [db] (db :node-class)))

(reg-sub :classes-names (fn [db] (keys (db :node-class))))

(reg-sub :class (fn [db [_ class-name]] (get-in db [:node-class class-name])))
