(ns app.renderer.db)

(def default-db
  {     
   :node-class {
                :Number {
                         :class :Number
                         :param {:num 0}
                         :color :orange
                         :in-pos  {}
                         :out-pos {:val [1 0.5]}
                         :in    []
                         :out   {:val 0}
                         :func  (fn [{:keys [num]}] { :val num })
                         }
                :Add    {
                         :class   :Add
                         :param   {}
                         :color   :black
                         :in-pos  {:a [0 0.25] :b [0 0.75]}
                         :out-pos {:val [1 0.5]}
                         :in      {:a 0 :b 0}
                         :out     {:val 0}
                         :func    (fn [{:keys [a b]}] { :val (+ a b) })
                         }
                }})
