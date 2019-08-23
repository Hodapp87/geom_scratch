(ns geom-scratch.core
  (:require
   [thi.ng.morphogen.core :as mg]
   [thi.ng.geom.core :as g]
   ;;[thi.ng.geom.vector :as v :refer [vec3]]
   [thi.ng.geom.core.vector :as v :refer [vec3]]
   [thi.ng.geom.aabb :as a])
  (:import
   [thi.ng.morphogen.core BoxNode]))

;; Trying to follow:
;; https://github.com/thi-ng/morphogen/blob/develop/src/examples.org

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def tree
  (let [branch (fn [[dir lpos]]
                 (mg/subdiv-inset
                  :dir :y :inset 0.05
                  :out {lpos (mg/subdiv dir 3 :out {1 nil}) 4 nil}))
        module (mg/subdiv-inset
                :dir :y :inset 0.4
                :out (mapv branch [[:cols 0] [:cols 1] [:slices 2] [:slices 3]]))]
    (mg/subdiv
     :rows 3
     :out [module
           (mg/subdiv
            :rows 3 :out {1 (mg/subdiv :cols 3 :out [nil {} nil])})
           module])))

(mg/save-mesh (mg/seed-box (a/aabb 1 0.5 1)) tree "foo.ply")
