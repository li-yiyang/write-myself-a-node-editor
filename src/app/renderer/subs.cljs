(ns app.renderer.subs
  (:require [re-frame.core :as re-frame :refer [reg-sub]]))

(reg-sub :nodes (fn [db] (db :nodes)))

(reg-sub :node-class (fn [db] (db :node-class)))
