(ns app.main.core
  (:require ["electron" :refer [app BrowserWindow]]))

(def main-window (atom nil))            ; main window

(defn init-browser []
  (reset! main-window                   ; set the window with size 800x600
          (BrowserWindow.
           (clj->js {:width 800
                     :height 600})))

  ;; it should be resource/public/index.html,
  ;; __dirname in javascript tells Electron where to find file
  (.loadURL ^js/electron.BrowserWindow @main-window
            (str "file://" js/__dirname "/public/index.html"))

  ;; Deal with situation when closing window
  (.on ^js/electron.BrowserWindow @main-window "closed"
       #(reset! main-window nil)))

(defn main
  "Main function for Electron app."
  []
  (.on app "window-all-closed" #(.quit app)) ; quit app
  (.on app "ready" init-browser))
