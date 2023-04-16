(ns app.renderer.db)

(def default-db
  {:nodes     {
               :A {
                   :id    :A
                   :color :orange
                   :type  :Number
                   :pos-x 0
                   :pos-y 1
                   :param { :num 1 }
                   :in    {}
                   :out   { :val [:C :a] }
                   }
               :B {
                   :id    :B
                   :color :orange
                   :type  :Number
                   :pos-x 1
                   :pos-y 1
                   :param { :num 2 }
                   :in    {}
                   :out   { :val [:C :a] }
                   }
               :C {
                   :id    :C
                   :color :black
                   :type  :Add
                   :pos-x 0
                   :pos-y 0
                   :param {}
                   :in    { :a [:A :val]
                            :b [:B :val] }
                   :out   { :val [] }
                   }
               }

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
