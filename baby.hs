doubleMe x = x + x

doubleUs x y = doubleMe x * 2 + doubleMe y * 2

doubleSamllNumber x = if x > 100
                      then x
                      else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
