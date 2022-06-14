(ns quantus.geometry
  (:require [quantus.math :as qm]
            [quantus.angles :as qa]
            [quantus.coordinates :as qc]
            [quantus.core :as q]))

;; TODO: tests!

(defn circle-intersection-points
  "Calculates the intersection of two circles defined by their center
  point (x,y) and radius r.  There are three possible outcomes:
  no-intersection because the centers of the circles are further apart
  than their radii.
  full-overlap because one circle is inside the other.
  Two (x,y) points of intersection, though they may be the same point
  in some cases."
  [x1 y1 r1 x2 y2 r2]
  ;; https://gist.github.com/jupdike/bfe5eb23d1c395d8a0a1a4ddd94882ac
  (let [center-dx (- x1 x2)
        center-dy (- y1 y2)
        R (qm/sqrt (+ (* center-dx center-dx)
                      (* center-dy center-dy)))]
    #_#_if-not (<= (Math/abs (- r1 r2))
                   R
                   (+ r1 r2))
    (cond
      (> R (+ r1 r2))
      :no-intersection

      (< R (qm/abs (- r1 r2)))
      :full-overlap

      :else
      (let [R2 (* R R)
            R4 (* R2 R2)
            a (/ (- (* r1 r1) (* r2 r2))
                 (* 2 R2))
            r2r2 (- (* r1 r1)
                    (* r2 r2))
            c (qm/sqrt (- (/ (* 2 (+ (* r1 r1) (* r2 r2)))
                             R2)
                          (/ (* r2r2 r2r2)
                             R4)
                          1))

            fx (+ (/ (+ x1 x2) 2)
                  (* a (- x2 x1)))
            gx (/ (* c (- y2 y1))
                  2)
            ix1 (+ fx gx)
            ix2 (- fx gx)

            fy (+ (/ (+ y1 y2) 2)
                  (* a (- y2 y1)))
            gy (/ (* c (- x1 x2))
                  2)
            iy1 (+ fy gy)
            iy2 (- fy gy)]
        [[ix1 iy1] [ix2 iy2]]))))

(defn distance-to-line
  "Calculates the distance from a point (x,y) to a line defined by:
  0 = a*x + b*y + c"
  ([x y a b] (distance-to-line x y a b 0))
  ([x y a b c]
   (/ (qm/abs ^double (+ (* a x)
                         (* b y)
                         c))
      (qm/sqrt (+ (* a a)
                  (* b b))))))

(defn distance-to-angle-through-origin
  "Calculates the distance from a point (x,y) to a line through the
  origin at angle theta."
  [x y theta]
  (distance-to-line x y 1.0 (-> theta qa/tan -)))

(defn distance-to-line-2points
  "Calculates the distance from a point to a line passing through two
  other points."
  ([[x y] [x1 y1] [x2 y2]]
   (distance-to-line-2points x y x1 y1 x2 y2))
  ([x y x1 y1 x2 y2]
   (qm// (qm/- (qm/* (qm/- x2 x1)
                     (qm/- y1 y))
               (qm/* (qm/- x1 x)
                     (qm/- y2 y1)))
         (qc/magnitude (qm/- (qc/xy x1 y1) (qc/xy x2 y2)))
         #_(qm/sqrt (qm/+ (qm/pow (qm/- x2 x1) 2)
                          (qm/pow (qm/- y2 y1) 2))))))

(defn line-intersection
  "Given 4 points in 2D, calculates the point where a line through
  points 1 and 2 intersects with a line through points 3 and 4.
  Points are an x-y tuple.  If the points don't intersect, then
  `:no-intersection` is returned."
  ([[x1 y1] [x2 y2] [x3 y3] [x4 y4]]
   (line-intersection x1 y1 x2 y2 x3 y3 x4 y4))
  ([x1 y1 x2 y2 x3 y3 x4 y4]
   (let [d (- (* (- x1 x2) (- y3 y4))
              (* (- y1 y2) (- x3 x4)))
         tn (- (* (- x1 x3) (- y3 y4))
               (* (- y1 y3) (- x3 x4)))
         un (- (* (- x1 x3) (- y1 y2))
               (* (- y1 y3) (- x1 x2)))]
     (if (zero? d) :no-intersection
         (let [t (/ tn d)
               u (/ un d)]
           [(+ x1 (* t (- x2 x1)))
            (+ y1 (* t (- y2 y1)))])))))

(defn line-segment-intersection
  "Given 4 points in 2D, calculates the point where a line between
  points 1 and 2 intersects with a line between points 3 and 4.
  Points are an x-y tuple.  If the lines don't intersect between the
  points (lines are not projected), then `:no-intersection` is
  returned."
  ([[x1 y1] [x2 y2] [x3 y3] [x4 y4]]
   (line-segment-intersection x1 y1 x2 y2 x3 y3 x4 y4))
  ([x1 y1 x2 y2 x3 y3 x4 y4]
   (let [d (- (* (- x1 x2) (- y3 y4))
              (* (- y1 y2) (- x3 x4)))
         tn (- (* (- x1 x3) (- y3 y4))
               (* (- y1 y3) (- x3 x4)))
         un (- (* (- x1 x3) (- y1 y2))
               (* (- y1 y3) (- x1 x2)))]
     (cond
       (zero? d) :no-intersection

       (or (and (<= 0 tn d)
                (<= 0 un d))
           (and (>= 0 tn d)
                (>= 0 un d)))
       (let [t (/ tn d)
             u (/ un d)]
         [(+ x1 (* t (- x2 x1)))
          (+ y1 (* t (- y2 y1)))])

       :else
       :no-intersection))))

(defn enclosing-obround
  "Given a seqeunce of points, returns an enclosing obround.  The
  obround will be at least `radius` from all the points.  By default,
  the obround will be aligned with the line between the first and last
  points being enclosed.
  Obround: A shape consisting of two semicircles connected by parallel
  lines tangent to their endpoints."
  ([points radius]
   (let [{x1 :x y1 :y} (first points)
         {x2 :x y2 :y} (last points)
         direction (qc/bearing [(qm/- x1 x2) (qm/- y1 y2)])]
     (enclosing-obround points radius direction)))
  ([points radius direction]
   (let [p1 (first points)
         p2 (last points)
         {x1 :x y1 :y} p1
         {x2 :x y2 :y} p2
         deviation (if (= p1 p2)
                     (qm/zero-value p1)
                     (->> points
                          (map (fn [{x :x y :y}]
                                 (qm/abs (distance-to-line-2points [x y] [x1 y1] [x2 y2]))))
                          (reduce qm/max)))
         thetas (mapv qa/degrees (qm/linspace -90 90 17))]
     (concat (mapv (fn [dir]
                     (->> dir
                          (qm/+ direction)
                          (qc/polar->xy (qm/+ radius deviation))
                          (qm/+ (qc/->xy p1))))
                   thetas)
             (mapv (fn [dir]
                     (->> dir
                          (qm/+ direction)
                          (qm/+ (qa/degrees 180))
                          (qc/polar->xy (qm/+ radius deviation))
                          (qm/+ (qc/->xy p2))))
                   thetas)))))
