module PointInPoly where

type Point = (Double, Double)

-- Guaranteed to be simple polygons, and the point is not on the edge.
pointInPoly :: [Point] -> Point -> Bool
pointInPoly poly (x, y) = odd . sum . zipWith intersectWithRay poly $ (tail . cycle) poly
  where
    -- Line representation: ax + by + c = 0.
    getABC (x1, y1) (x2, y2) = (y1 - y2, x2 - x1, x1 * y2 - x2 * y1)
    -- Casting a ray starting from `point` towards left.
    intersectWithRay p1@(_, y1) p2@(_, y2) =
        let (a, b, c) = getABC p1 p2 in
        -- If parallel, regard as no intersection.
        if a == 0 then 0::Int
        else let x' = (-c - b * y) / a in
          -- Note '< max' and '>= min' would differentiate cases where
          -- intersecting point is either p1 or p2.
          if x' <= x && y < max y1 y2 && y >= min y1 y2 then 1 else 0
