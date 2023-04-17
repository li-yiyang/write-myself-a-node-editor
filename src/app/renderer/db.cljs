(ns app.renderer.db)

(def default-db
  {     
   :node-class {
                :Number {
                         :class :Number
                         :param [ :num ]
                         :in    []
                         :out   [ :val ]
                         :func  (fn [{:keys [num]}] { :val num })
                         }
                :Add    {
                         :class :Add
                         :param []
                         :in    [ :a :b ]
                         :out   [ :val ]
                         :func  (fn [{:keys [a b]}] { :val (+ a b) })
                         }
                }})
