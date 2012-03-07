module SVG where

import Data.Word
import Data.List

data SVGAngle = SVGDeg Float 
              | SVGRad Float
              | SVGGrad Float

data SVGColor = SVGColorPercent Int Int Int 
              | SVGColorHex String

data SVGLength = SVGLengthEm Double 
               | SVGLengthEx Double
               | SVGLengthPx Double
               | SVGLengthIn Double
               | SVGLengthCm Double
               | SVGLengthMm Double
               | SVGLengthPt Double
               | SVGLengthPc Double
               | SVGLengthPercent Int
               | SVGLengthNull Double

type SVGCoordinate= SVGLength
type SVGInteger = Integer

instance Show SVGAngle where
    show (SVGDeg a) = show a ++ "deg"
    show (SVGRad a) = show a ++ "rad"
    show (SVGGrad a) = show a ++ "grad"

instance Show SVGColor where
    show (SVGColorPercent r g b) = "rgb(" ++ show r ++ "%, " ++ show g ++ "%, " ++ show b ++ "%)"
    show (SVGColorHex triple) = "#" ++ triple

instance Show SVGLength where
    show (SVGLengthEm l) = show l ++ "em"
    show (SVGLengthEx l) = show l ++ "ex"
    show (SVGLengthPx l) = show l ++ "px"
    show (SVGLengthIn l) = show l ++ "in" 
    show (SVGLengthCm l) = show l ++ "cm"
    show (SVGLengthMm l) = show l ++ "mm"
    show (SVGLengthPt l) = show l ++ "pt"
    show (SVGLengthPc l) = show l ++ "pc"
    show (SVGLengthPercent l) = show l ++ "%"
    show (SVGLengthNull l) = show l

data SVGTransform = SVGMatrix Double Double Double Double Double Double 
                  | SVGTranslateX Double
                  | SVGTranslateXY Double Double
                  | SVGScaleEq Double
                  | SVGScale Double Double
                  | SVGRotate Double
                  | SVGRotateAround Double Double Double
                  | SVGSkewX Double
                  | SVGSkewY Double

instance Show SVGTransform where
    show (SVGMatrix a b c d e f) = "matrix(" ++ intercalate "," (map show [a,b,c,d,e,f]) ++ ")"
    show (SVGTranslateX tx) = "translate(" ++ show tx ++ ")"
    show (SVGTranslateXY tx ty) = "translate(" ++ show tx ++ "," ++ show ty ++ ")"
    show (SVGScaleEq sxy) = "scale(" ++ show sxy ++ ")"
    show (SVGScale sx sy) = "scale(" ++ show sx ++ "," ++ show sy ++ ")"
    show (SVGRotate r) = "rotate(" ++ show r ++ ")"
    show (SVGRotateAround r sx sy) = "rotate(" ++ intercalate "," (map show [r,sx,sy]) ++ ")" 
    show (SVGSkewX sx) = "skewX(" ++ show sx ++ ")"
    show (SVGSkewY sy) = "skewY(" ++ show sy ++ ")"

data SVGPaint = SVGPColor SVGColor
              | SVGPIRI String

instance Show SVGPaint where
    show (SVGPColor col) = show col
    show (SVGPIRI id) = "url(#" ++ id ++ ")"

data SVGRule = SVGRNonzero 
             | SVGREvenOdd

instance Show SVGRule where
    show SVGRNonzero = "nonzero"
    show SVGREvenOdd = "evenodd"

data SVGLinecap = SVGLCButt
                | SVGLCRound
                | SVGLCSquare

instance Show SVGLinecap where
    show SVGLCButt = "butt"
    show SVGLCRound = "round"
    show SVGLCSquare = "square"

data SVGStyleElement = SVGFill SVGPaint 
                     | SVGFillRule SVGRule
                     | SVGFillOpacity Double
                     | SVGStroke SVGPaint
                     | SVGStrokeWidth SVGLength
                     | SVGStrokeLinecap SVGLinecap
                     | SVGStrokeOpacity Double

instance Show SVGStyleElement where
    show (SVGFill paint) = "fill=\"" ++ show paint ++ "\""
    show (SVGFillRule rule) = "fill-rule=\"" ++ show rule ++ "\""
    show (SVGFillOpacity op) = "fill-opacity=\"" ++ show op ++ "\""
    show (SVGStroke paint) = "stroke=\"" ++ show paint ++ "\""
    show (SVGStrokeWidth width) = "stroke-width=\"" ++ show width ++ "\""
    show (SVGStrokeLinecap cap) = "stroke-linecap=\"" ++ show cap ++ "\""
    show (SVGStrokeOpacity op) = "stroke-opacity=\"" ++ show op ++ "\""


data SVGCommonAttrib = SVGID String
                     | SVGClass String
                     | SVGTransforms [SVGTransform]
                     | SVGStyle [SVGStyleElement]

instance Show SVGCommonAttrib where
    show (SVGID id) = "id=\"" ++ id ++ "\""
    show (SVGClass sClass) = "class=\"" ++ sClass ++ "\""
    show (SVGTransforms transform) = "transform=\"" ++ intercalate " " (map show transform) ++ "\""
    show (SVGStyle styles) = intercalate " " (map show styles)

data SVGPathSpec = SVGAbsMoveTo Double Double
                 | SVGRelMoveTo Double Double
                 | SVGAbsLineTo Double Double
                 | SVGRelLineTo Double Double
                 | SVGAbsHLineTo Double
                 | SVGRelHLineTo Double
                 | SVGAbsVLineTo Double
                 | SVGRelVLineTo Double
                 | SVGAbsCubicTo Double Double Double Double Double Double
                 | SVGRelCubicTo Double Double Double Double Double Double
                 | SVGAbsSmoothCubicTo Double Double Double Double
                 | SVGRelSmoothCubicTo Double Double Double Double
                 | SVGAbsQuadraticTo Double Double Double Double
                 | SVGRelQuadraticTo Double Double Double Double
                 | SVGAbsSmoothQuadraticTo Double Double
                 | SVGRelSmoothQuadraticTo Double Double
                 | SVGAbsEllipticTo Double Double Double Bool Bool Double Double
                 | SVGRelEllipticTo Double Double Double Bool Bool Double Double
                 | SVGClosePath

instance Show SVGPathSpec where
    show (SVGAbsMoveTo x y) = "M " ++ show x ++ " " ++ show y
    show (SVGRelMoveTo x y) = "m " ++ show x ++ " " ++ show y
    show (SVGAbsLineTo x y) = "L " ++ show x ++ " " ++ show y
    show (SVGRelLineTo x y) = "l " ++ show x ++ " " ++ show y
    show (SVGAbsHLineTo x) = "H " ++ show x
    show (SVGRelHLineTo x) = "h " ++ show x
    show (SVGAbsVLineTo y) = "V " ++ show y
    show (SVGRelVLineTo y) = "v " ++ show y
    show (SVGAbsCubicTo csx csy ctx cty tx ty) = "C " ++ intercalate " " (map show [csx,csy,ctx,cty,tx,ty])
    show (SVGRelCubicTo csx csy ctx cty tx ty) = "c " ++ intercalate " " (map show [csx,csy,ctx,cty,tx,ty])
    show (SVGAbsSmoothCubicTo ctx cty tx ty) = "S " ++ intercalate " " (map show [ctx,cty,tx,ty])
    show (SVGRelSmoothCubicTo ctx cty tx ty) = "s " ++ intercalate " " (map show [ctx,cty,tx,ty])
    show (SVGAbsQuadraticTo cx cy x y) = "Q " ++ intercalate " " (map show [cx,cy,x,y])
    show (SVGRelQuadraticTo cx cy x y) = "q " ++ intercalate " " (map show [cx,cy,x,y])
    show (SVGAbsSmoothQuadraticTo x y) = "T " ++ intercalate " " (map show [x,y])
    show (SVGRelSmoothQuadraticTo x y) = "t " ++ intercalate " " (map show [x,y])
    show (SVGAbsEllipticTo rx ry xrot largeArc sweepDir x y) = 
        "A " ++ intercalate " " (map show [rx, ry, xrot]) ++ " " ++ 
        show (fromEnum largeArc) ++ " " ++ show (fromEnum sweepDir) ++ " " ++ 
        intercalate " " (map show [x,y])
    show (SVGRelEllipticTo rx ry xrot largeArc sweepDir x y) = 
        "a " ++ intercalate " " (map show [rx, ry, xrot]) ++ " " ++ 
        show (fromEnum largeArc) ++ " " ++ show (fromEnum sweepDir) ++ " " ++ 
        intercalate " " (map show [x,y])
    show SVGClosePath = "Z"

data SVGColorStops = SVGColorStop Double SVGColor
instance Show SVGColorStops where
    show (SVGColorStop offset color) = "<stop offset=\"" ++ show offset ++ "\" color=\"" ++ show color ++ "\"/>"

data SVGElement = SVGGroup [SVGCommonAttrib] [SVGElement] 
                | SVGDesc String
                | SVGTitle String
                | SVGImage SVGCoordinate SVGCoordinate SVGLength SVGLength String
                | SVGPath [SVGCommonAttrib] [SVGPathSpec]
                | SVGRect [SVGCommonAttrib] SVGCoordinate SVGCoordinate SVGLength SVGLength
                | SVGRoundedRect [SVGCommonAttrib] SVGCoordinate SVGCoordinate SVGLength SVGLength SVGLength SVGLength
                | SVGCircle [SVGCommonAttrib] SVGCoordinate SVGCoordinate SVGLength
                | SVGEllipse [SVGCommonAttrib] SVGCoordinate SVGCoordinate SVGLength SVGLength
                | SVGLine [SVGCommonAttrib] SVGCoordinate SVGCoordinate SVGCoordinate SVGCoordinate
                | SVGPolyLine [SVGCommonAttrib] [SVGCoordinate]
                | SVGPolygon [SVGCommonAttrib] [SVGCoordinate]
                | SVGLinearGradient String [SVGColorStops] 
                | SVGRadialGradient String [SVGColorStops]

instance Show SVGElement where
    show (SVGGroup attribs elements) = "<g " ++ 
                                       intercalate " " (map show attribs) ++ 
                                       ">" ++ intercalate "\n" (map show elements) ++ "</g>"
    show (SVGDesc desc) = "<desc>" ++ desc ++ "</desc>"
    show (SVGTitle title) = "<title>" ++ title ++ "</title>"
    show (SVGImage x y width height iri) = "<image x=\"" ++ show x ++
                                           "\" y=\"" ++ show y ++
                                           "\" width=\"" ++ show width ++
                                           "\" height=\"" ++ show height ++
                                           "\" xlink:href=\"" ++ iri ++ "\" />"
    show (SVGPath attribs path) = "<path d=\"" ++ 
                                  intercalate " " (map show path) ++ "\" " ++ 
                                  intercalate " " (map show attribs) ++ "/>"
    show (SVGRect attribs x y w h) = "<rect " ++ 
                                     intercalate " " (map show attribs) ++ 
                                     " x=\"" ++ show x ++ "\"" ++ 
                                     " y=\"" ++ show y ++ "\"" ++ 
                                     " width=\"" ++ show w ++ "\"" ++ 
                                     " height=\"" ++ show h ++ "\"" ++ "/>"
    show (SVGRoundedRect attribs x y w h rx ry) = "<rect " ++ 
                                                  intercalate " " (map show attribs) ++ 
                                                  " x=\"" ++ show x ++ "\"" ++ 
                                                  " y=\"" ++ show y ++ "\"" ++ 
                                                  " width=\"" ++ show w ++ "\"" ++ 
                                                  " height=\"" ++ show h ++ "\"" ++ 
                                                  " rx=\"" ++ show rx ++ "\"" ++ 
                                                  " ry=\"" ++ show ry ++ "\"" ++ "/>"
    show (SVGCircle attribs cx cy r) = "<circle " ++ 
                                       intercalate " " (map show attribs) ++ 
                                       " cx=\"" ++ show cx ++ "\"" ++ 
                                       " cy=\"" ++ show cy ++ "\"" ++
                                       " r=\"" ++ show r ++ "\" />"
    show (SVGEllipse attribs cx cy rx ry) = "<circle " ++ 
                                            intercalate " " (map show attribs) ++ 
                                            " cx=\"" ++ show cx ++ "\"" ++ 
                                            " cy=\"" ++ show cy ++ "\"" ++
                                            " rx=\"" ++ show rx ++ "\"" ++ 
                                            " ry=\"" ++ show ry ++ "\" />"
    show (SVGLine attribs sx sy tx ty) = "<line " ++
                                         intercalate " " (map show attribs) ++ 
                                         " x1=\"" ++ show sx ++ "\"" ++ 
                                         " y1=\"" ++ show sy ++ "\"" ++
                                         " x2=\"" ++ show tx ++ "\"" ++ 
                                         " y2=\"" ++ show ty ++ "\" />"
                                                        
    show (SVGPolyLine attribs points) = "<polyline " ++ 
                                         intercalate " " (map show attribs) ++ 
                                         " points=\"" ++ 
                                         intercalate " " (map show points) ++ 
                                        "\" />"
    show (SVGPolygon attribs points) = "<polygon " ++ 
                                       intercalate " " (map show attribs) ++ 
                                       " points=\"" ++ 
                                       intercalate " " (map show points) ++ 
                                       "\" />"
    show (SVGLinearGradient id stops) = "<linearGradient id=\"" ++ id ++ "\">\n" ++ 
                                        intercalate "\n" (map show stops) ++ 
                                        "</linearGradient>"
    show (SVGRadialGradient id stops) = "<radialGradient id=\"" ++ id ++ "\">\n" ++ 
                                        intercalate "\n" (map show stops) ++ 
                                        "</radialGradient>"

data SVGDocAttribute = SVGDocWidth SVGLength
                     | SVGDocHeight SVGLength

instance Show SVGDocAttribute where
    show (SVGDocWidth l) = "width=\"" ++ show l ++ "\""
    show (SVGDocHeight l) = "height=\"" ++ show l ++ "\""

data SVGDocument = SVGDocument [SVGDocAttribute] [SVGElement]

instance Show SVGDocument where
    show (SVGDocument attribs elements) = 
        "<?xml version=\"1.0\" standalone=\"no\"?>\n" ++
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \n" ++
        "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
        "<svg " ++ intercalate " " (map show attribs) ++ " version=\"1.1\" \n" ++ 
        "xmlns=\"http://www.w3.org/2000/svg\">\n" ++ 
        intercalate "\n" (map show elements) ++ 
        "\n</svg>\n"

svgCenteredDocument :: [SVGDocAttribute] -> [SVGElement] -> SVGDocument
svgCenteredDocument attribs elements = 
    SVGDocument attribs [SVGGroup [SVGTransforms [SVGTranslateXY 200 200]] elements]







blueStyle = SVGStyle [ SVGFill (SVGPColor (SVGColorHex "ccc")), 
                       SVGStroke (SVGPColor (SVGColorPercent 0 0 100)),
                       SVGStrokeWidth (SVGLengthNull 2) ]

emptyStyle = SVGStyle [SVGFillOpacity 0.0,
                       SVGStroke (SVGPColor (SVGColorPercent 0 0 100)),
                       SVGStrokeWidth (SVGLengthNull 2) ]


tdoc0 = SVGDocument [] [SVGPolygon [blueStyle]
                        (map SVGLengthNull [100,100,200,100,200,200,100,200]),
                        SVGPolygon [blueStyle]
                        (map SVGLengthNull [200,200,300,200,300,300,200,300])]
t0 = writeFile "tut0.svg" $ show tdoc0

hexagon c r = concatPairs $ translateTo c basicHexagon where
  basicHexagon =  top ++ (negate r, 0):bottom 
  top          =  [(r,0),(r * cos 1,(r * sin 1)),(negate (r * cos 1), r * (sin 1))]
  bottom       =  map (\(x,y)->(x,negate y)) (reverse top)

translateTo (x,y) poly = map f poly where f (a,b)= ((a+x),(b+y))

concatPairs :: [(a,a)] -> [a]
concatPairs [] = []
concatPairs ((x,y):xys) = x:y:(concatPairs xys)

tdoc1 = SVGDocument [] [SVGPolygon [blueStyle] (map SVGLengthNull (hexagon (100,100) 50))]

t1 = writeFile "t1.svg" $ show tdoc1

{-
hexField r n m = let 
     mkHex n = hexagon (1.5*n*(r*2),(r*2)) r
     row n = map mkHex [1..n]
     aRow = row n
  in concat [map (offset (r*x)) aRow |x<-[1..m]]

offset r polys = map (oh r) polys where
  oh r pt@(x,y) = (x+(1.5*r),y+(r*sin 1))

t2 = writeFile "t2.svg" $ writePolygons (blue $ hexField 50 4 5 )
-}



