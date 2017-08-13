module TextAlignJustify.Test where

import           Test.Hspec
import           TextAlignJustify (justify)

longInput :: String
longInput = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum sagittis dolor mauris, at elementum ligula tempor eget. In quis rhoncus nunc, at aliquet orci. Fusce at dolor sit amet felis suscipit tristique. Nam a imperdiet tellus. Nulla eu vestibulum urna. Vivamus tincidunt suscipit enim, nec ultrices nisi volutpat ac. Maecenas sit amet lacinia arcu, non dictum justo. Donec sed quam vel risus faucibus euismod. Suspendisse rhoncus rhoncus felis at fermentum. Donec lorem magna, ultricies a nunc sit amet, blandit fringilla nunc. In vestibulum velit ac felis rhoncus pellentesque. Mauris at tellus enim. Aliquam eleifend tempus dapibus. Pellentesque commodo, nisi sit amet hendrerit fringilla, ante odio porta lacus, ut elementum justo nulla et dolor."

longOutput :: String
longOutput = "\
\Lorem  ipsum  dolor  sit amet,\n\
\consectetur  adipiscing  elit.\n\
\Vestibulum    sagittis   dolor\n\
\mauris,  at  elementum  ligula\n\
\tempor  eget.  In quis rhoncus\n\
\nunc,  at  aliquet orci. Fusce\n\
\at   dolor   sit   amet  felis\n\
\suscipit   tristique.   Nam  a\n\
\imperdiet   tellus.  Nulla  eu\n\
\vestibulum    urna.    Vivamus\n\
\tincidunt  suscipit  enim, nec\n\
\ultrices   nisi  volutpat  ac.\n\
\Maecenas   sit   amet  lacinia\n\
\arcu,  non dictum justo. Donec\n\
\sed  quam  vel  risus faucibus\n\
\euismod.  Suspendisse  rhoncus\n\
\rhoncus  felis  at  fermentum.\n\
\Donec lorem magna, ultricies a\n\
\nunc    sit    amet,   blandit\n\
\fringilla  nunc. In vestibulum\n\
\velit    ac    felis   rhoncus\n\
\pellentesque. Mauris at tellus\n\
\enim.  Aliquam eleifend tempus\n\
\dapibus. Pellentesque commodo,\n\
\nisi    sit   amet   hendrerit\n\
\fringilla,   ante  odio  porta\n\
\lacus,   ut   elementum  justo\n\
\nulla et dolor."

main :: IO ()
main = hspec $ it "Example" $ do
  justify "123 45 6" 7 `shouldBe` "123  45\n6"
  justify longInput 30 `shouldBe` longOutput
