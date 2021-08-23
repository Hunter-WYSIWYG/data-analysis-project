module Tree exposing (..)

import Svg exposing (Svg, line)
import Svg.Attributes exposing (..)

toString prop value =
    prop (String.fromFloat value)

drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    line
        [ toString x1 0, toString y1 0, (toString x2 targetX), toString y2 targetY, stroke "black" ]
        []