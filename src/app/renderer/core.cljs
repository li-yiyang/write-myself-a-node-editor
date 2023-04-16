(ns app.renderer.core
  (:require [reagent.dom         :as reagentdom  :refer [render]]
            [re-frame.core       :as re-frame    :refer [dispatch-sync]]
            [app.renderer.view   :as view        :refer [main]]
            [app.renderer.db     :as db]
            [app.renderer.subs   :as subs]
            [app.renderer.events :as events]
            [devtools.core       :as devtools]
            ))

(defn ^:dev/after-load start! "Start Renderer" []
  (enable-console-print!)               ; for dev usage
  (devtools/install!)
  (dispatch-sync [:initialize-db])
  (render [main] (.getElementById js/document "app-container")))
