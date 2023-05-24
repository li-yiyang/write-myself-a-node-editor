(ns app.main.core
  (:require ["electron"        :as electron :refer [app BrowserWindow]]
            [cognitect.transit :as tr]))

(def main-window (atom nil))            ; main window
(def dialog (.-dialog electron))        ; dialog

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

(defn open-dialog []
  (.showOpenDialog dialog (clj->js {:properties ["openFile" "multiSelections"]})))

(defn save-dialog []
  (.showOpenDialog dialog (clj->js {:properties []})))

;; ;;; Notifications: Just for Fun...
;; (def notification (.-Notification electron))

;; (defn show-notification
;;   "Show Notifications. "
;;   [title body]
;;   (-> (js/Object. notification #js{:title title
;;                                    :body body})
;;       (.show)))

;; ;;; File read and write
;; (defn select-file-dialog []
;;   (let [options #js {:properties ["openFile"]
;;                      :filters    [{:name       "All Files"
;;                                    :extensions "*"}]}]
;;     (as-> (.showOpenDialog dialog options) result
;;       (if (not (.canceled result))
;;         (.filePaths result)
;;         nil))))

;; (defn read-file [path]
;;   (.readFileSync fs path "utf8"))

;; (defn write-file [path content]
;;   (.writeFileSync fs path content))

;; ;;; Regist IPC event
;; (.on ipc-main "read-file" (fn [event path]
;;                             (let [data (read-file path)]
;;                               (.send event "file-data" data))))

;; (.on ipc-main "write-file" (fn [event path content]
;;                              (write-file path content)
;;                              (.send event "file-saved")))

;; (.on ipc-main "show-notification" (fn [event title body]
;;                                     (show-notification title body)
;;                                     (.send event "notification showed")))

;; (.on ipc-main "select-file-dialog" (fn [event]
;;                                      (.send event
;;                                             "selected-file"
;;                                             (select-file-dialog))))
