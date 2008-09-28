(import math)

(poly cons
   (null)
   (cons head:a (cons tail:a)))
   
(poly Shape
   (Rectangle s1:float s2:float)
   (Ellipse  r1:float r2:float)
   (RtTriangle s1:float s2:float)
   (Polygon points:[(tuple float float)]))
   
(meth area (Rectangle s1 s2)  (* s1 s2))
(meth area (RtTriangle s1 s2) (* s1 (/ s2 2))
(meth area (Ellipse r1 r2)    (* math.pi r1 r2 ))

