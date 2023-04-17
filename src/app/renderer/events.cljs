(ns app.renderer.events
  (:require [re-frame.core   :as re-frame :refer [reg-event-db reg-event-fx]]
            [app.renderer.db :as database :refer [default-db]]))

(reg-event-db
 :initialize-db
 (fn [_ _] default-db))
