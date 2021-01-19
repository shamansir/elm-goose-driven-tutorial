module CombinedTransform exposing (..)


type alias Matrix =
    { m11 : Float
    , m12 : Float
    , m21 : Float
    , m22 : Float
    , dx : Float
    , dy : Float
    }


init : Matrix
init =
    { m11 = 1
    , m12 = 0
    , m21 = 0
    , m22 = 1
    , dx = 0
    , dy = 0
    }


reset : Matrix -> Matrix
reset = always init


rotate : Float -> Matrix -> Matrix
rotate angle =
    dot
        { m11 = cos angle
        , m12 = -1 * sin angle
        , m21 = sin angle
        , m22 = cos angle
        , dx = 0
        , dy = 0
        }


rotateDeg : Float -> Matrix -> Matrix
rotateDeg = rotate << degrees


translate : Float -> Float -> Matrix -> Matrix
translate x y =
    dot
        { m11 = 1
        , m12 = 0
        , m21 = 0
        , m22 = 1
        , dx = x
        , dy = y
        }


translateX : Float -> Matrix -> Matrix
translateX x =
    translate x 0


translateY : Float -> Matrix -> Matrix
translateY y =
    translate 0 y


scale : Float -> Float -> Matrix -> Matrix
scale sx sy =
    dot
        { m11 = sx
        , m12 = 0
        , m21 = 0
        , m22 = sy
        , dx = 0
        , dy = 0
        }

scaleX : Float -> Matrix -> Matrix
scaleX sx = scale sx 0


scaleY : Float -> Matrix -> Matrix
scaleY sy = scale 0 sy


shear : Float -> Float -> Matrix -> Matrix
shear phi psi =
    dot
        { m11 = 1
        , m12 = tan psi
        , m21 = tan phi
        , m22 = 1
        , dx = 0
        , dy = 0
        }


shearX : Float -> Matrix -> Matrix
shearX phi = shear phi 0


shearY : Float -> Matrix -> Matrix
shearY psi = shear 0 psi


reflectX : Matrix -> Matrix
reflectX =
    dot
        { m11 = 1
        , m12 = 0
        , m21 = 0
        , m22 = -1
        , dx = 0
        , dy = 0
        }


reflectY : Matrix -> Matrix
reflectY =
    dot
        { m11 = -1
        , m12 = 0
        , m21 = 0
        , m22 = 1
        , dx = 0
        , dy = 0
        }


reflectBoth : Matrix -> Matrix
reflectBoth =
    dot
        { m11 = -1
        , m12 = 0
        , m21 = 0
        , m22 = -1
        , dx = 0
        , dy = 0
        }


dot : Matrix -> Matrix -> Matrix
dot a b =
    { m11 = a.m11 * b.m11 + a.m12 * b.m21 + a.dx * 0
    , m12 = a.m11 * b.m12 + a.m12 * b.m22 + a.dx * 0
    , dx = a.m11 * b.dx + a.m12 * b.dy + a.dx * 1
    , m21 = a.m21 * b.m11 + a.m22 * b.m21 + a.dy * 0
    , m22 = a.m21 * b.m12 + a.m22 * b.m22 + a.dy * 0
    , dy = a.m21 * b.dx + a.m22 * b.dy + a.dy * 1
    }
