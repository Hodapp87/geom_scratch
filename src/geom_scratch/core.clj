(ns geom-scratch.core
  (:require
   ;;[thi.ng.morphogen.core :as mg]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec3 vec2 V3]]
   ;;[thi.ng.geom.core.vector :as v :refer [vec3]]
   [thi.ng.geom.aabb :as a]
   [thi.ng.geom.matrix :refer [matrix44 M44]]
   ;; what is up with your fucking fetish for moving things around?
   ;;[thi.ng.geom.aabb :as a]
   [thi.ng.geom.circle :as c]
   ;;[thi.ng.geom.types.utils.ptf :as ptf]
   ;; whatever.
   ;; [thi.ng.geom.ptf :as ptf]
   ;;[thi.ng.math :as m2]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.math.core :as m :refer [THIRD_PI TWO_PI]]
   [thi.ng.dstruct.core :as d]
   [clojure.java.io :as io]
   [thi.ng.math.macros :as mm]
   )
  ;;(:import [thi.ng.morphogen.core BoxNode])
  )

;; Trying to follow:
;; https://github.com/thi-ng/morphogen/blob/develop/src/examples.org

(comment
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
             module]))))

;;(mg/save-mesh (mg/seed-box (a/aabb 1 0.5 1)) tree "foo.ply")

;; https://github.com/thi-ng/geom/blob/master/geom-types/src/ptf.org
(defn compute-tangents
  [points]
  (let [t (mapv (fn [[p q]] (m/normalize (m/- q p))) (d/successive-nth 2 points))]
    (conj t (peek t))))

(defn compute-frame
  [tangents norms bnorms i]
  (let [ii (dec i)
        p  (tangents ii)
        q  (tangents i)
        a  (m/cross p q)
        n  (if-not (m/delta= 0.0 (m/mag-squared a))
             (let [theta (Math/acos (m/clamp-normalized (m/dot p q)))]
               (g/transform-vector (g/rotate-around-axis M44 (m/normalize a) theta) (norms ii)))
             (norms ii))]
    [n (m/cross q n)]))

(defn compute-first-frame
  [t]
  (let [t' (m/abs t)
        i  (if (< (t' 0) (t' 1)) 0 1)
        i  (if (< (t' 2) (t' i)) 2 i)
        n  (m/cross t (m/normalize (m/cross t (assoc V3 i 1.0))))]
    [n (m/cross t n)]))

(defn compute-frames
  [points]
  (let [tangents (compute-tangents points)
        [n b]    (compute-first-frame (first tangents))
        num      (count tangents)]
    (loop [norms [n], bnorms [b], i 1]
      (if (< i num)
        (let [[n b] (compute-frame tangents norms bnorms i)]
          (recur (conj norms n) (conj bnorms b) (inc i)))
        [points tangents norms bnorms]))))

(defn align-frames
  [[points tangents norms bnorms]]
  (let [num   (count tangents)
        a     (first norms)
        b     (peek norms)
        theta (-> (m/dot a b) (m/clamp-normalized) (Math/acos) (/ (dec num)))
        theta (if (> (m/dot (first tangents) (m/cross a b)) 0.0) (- theta) theta)]
    (loop [norms norms, bnorms bnorms, i 1]
      (if (< i num)
        (let [t (tangents i)
              n (-> M44
                    (g/rotate-around-axis t (* theta i))
                    (g/transform-vector (norms i)))
              b (m/cross t n)]
          (recur (assoc norms i n) (assoc bnorms i b) (inc i)))
        [points tangents norms bnorms]))))

(defn sweep-point
  "Takes a path point, a PTF normal & binormal and a profile point.
  Returns profile point projected on path (point)."
  [p n b [qx qy]]
  (vec3
   (mm/madd qx (n 0) qy (b 0) (p 0))
   (mm/madd qx (n 1) qy (b 1) (p 1))
   (mm/madd qx (n 2) qy (b 2) (p 2))))

(defn sweep-profile
  [profile [points _ norms bnorms]]
  (let [frames (map vector points norms bnorms)
        tx     (fn [[p n b]] (mapv #(sweep-point p n b %) profile))
        frame0 (tx (first frames))]
    (->> (next frames) ;; TODO transducer
         (reduce
          (fn [[faces prev] frame]
            (let [curr  (tx frame)
                  curr  (conj curr (first curr))
                  faces (->> (mapcat
                              (fn [a b] [(vector (a 0) (a 1) (b 1) (b 0))])
                              (d/successive-nth 2 prev)
                              (d/successive-nth 2 curr))
                             (concat faces))]
              [faces curr]))
          [nil (conj frame0 (first frame0))])
         (first))))

(defn sweep-mesh
  [points profile & [{:keys [mesh align?]}]]
  (let [frames (compute-frames points)
        frames (if align? (align-frames frames) frames)]
    (->> frames
         (sweep-profile profile)
         (g/into (or mesh (bm/basic-mesh))))))

(defn sweep-strand
  [[p _ n b] r theta delta profile]
  (-> (mapv
       #(->> (vec2 r (mm/madd % delta theta))
             (g/as-cartesian)
             (sweep-point (p %) (n %) (b %)))
       (range (count p)))
      (sweep-mesh profile {:align? true})))

(defn sweep-strands
  [base r strands twists profile]
  (let [delta (/ (* twists TWO_PI) (dec (count (first base))))]
    (->> (m/norm-range strands)
         (butlast)
         (pmap #(sweep-strand base r (* % TWO_PI) delta profile)))))

(defn sweep-strand-mesh
  [base r strands twists profile & [{:as opts}]]
  (->> (sweep-strands base r strands twists profile)
       (reduce g/into (or (:mesh opts) (bm/basic-mesh)))))

(defn cinquefoil
  [t]
  (let [t  (* t (* m/TWO_PI))
        pt (* 2.0 t)
        qt (* 5.0 t)
        qc (+ 3.0 (Math/cos qt))]
    (v/vec3 (* qc (Math/cos pt)) (* qc (Math/sin pt)) (Math/sin qt))))

(with-open [o (io/output-stream "cinquefoil.stl")]
  (mio/write-stl
   (mio/wrapped-output-stream o)
   (sweep-mesh
    (mapv cinquefoil (m/norm-range 400))
    (g/vertices (c/circle 0.5) 20)
    {:align? true})))
;; The above won't run because:
;; "java.lang.IllegalArgumentException
;;   No implementation of method: :- of protocol:
;;   #'thi.ng.math.core/IMathOps found for class: java.lang.Double"

;; I can switch sweep-mesh with ptf/sweep-mesh (and uncomment ptf line
;; at the top of the file), and then it runs, but I end up with a mesh
;; where it appears that every other triangle is missing - as if it
;; tried to render the mesh with quads, and then only handled half of
;; every quad.
