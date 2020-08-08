module Point exposing (Point, add, average, distance)


type alias Point =
    { x : Int, y : Int }


add : Point -> Point -> Point
add a b =
    { x = a.x + b.x, y = a.y + b.y }


distance : Point -> Point -> Float
distance a b =
    sqrt (toFloat (b.x - a.x) ^ 2 + toFloat (b.y - a.y) ^ 2)


average : Point -> Point -> Point
average a b =
    { x = (a.x + b.x) // 2, y = (a.y + b.y) // 2 }
