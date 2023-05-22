(ns app.renderer.core
  (:require [reagent.dom         :as reagentdom  :refer [render]]
            [app.renderer.view   :as view        :refer [main]]
            [devtools.core       :as devtools]))

(defn ^:dev/after-load start! "Start Renderer" []
  (enable-console-print!)               ; for dev usage
  (devtools/install!)
  (render [main] (.getElementById js/document "app-container")))
