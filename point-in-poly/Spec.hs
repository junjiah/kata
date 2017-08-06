module PointInPoly.Test where

import           PointInPoly (Point, pointInPoly)
import           Test.Hspec

main :: IO ()
main = hspec $
  it "Should handle a simple square" $
    let poly1 = [(-5, -5), (5, -5) , (5, 5), (-5, 5)] in
    let poly2 = [(0, 5), (5, 0), (0, -5), (-5, 0)] in do
      showAndTest poly1 (-6, 0) False
      showAndTest poly1 (1, 1) True
      showAndTest poly1 (2, 1) True
      showAndTest poly1 (6, 6) False
      showAndTest poly1 (5, 6) False
      showAndTest poly1 (3, 3) True
      showAndTest poly1 (4, 3) True
      showAndTest poly1 (3, 4) True
      showAndTest poly2 (6, 0) False
      showAndTest poly2 (4, 0) True

showAndTest :: [Point] -> Point -> Bool -> IO ()
showAndTest poly point expect = do
  drawTest poly point expect
  pointInPoly poly point `shouldBe` expect

drawTest :: [Point] -> Point -> Bool -> IO ()
drawTest poly point inside = putStrLn htmlDiv
  where
    htmlDiv = concat [ "<div style='background:white; width:140px;'>"
                 , "<svg width='140' height='140'>"
                 , "<polygon points='" ++ unwords points ++ "' stroke='blue' fill='white'></polygon>"
                 , "<circle cx='" ++ show cx ++ "' cy='" ++ show cy ++ "' r='2' fill='" ++ color ++ "'></circle>"
                 , "</svg>"
                 , "</div>"
                 ]
    points = map (showPt . transform) poly
    showPt (x,y) = show x ++ "," ++ show y
    (cx,cy) = transform point
    transform (x,y) = (t x, t y) where t i = (i + 7) * 10 + 0.5
    color = if inside then "green" else "red"
