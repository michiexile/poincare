{-
Poincare disk tilings.
(c) 2012 Mikael Vejdemo-Johansson
BSD License.
-}

module Poincare where

import SVG
import Complex
import Data.List (nub, (\\))
import Data.Map ((!), Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

type Point = (Double, Double)
type Line = (Point, Point)
type Polygon = [Point]
type Circle = (Point, Double)
type Disk = Circle

type KPoint = Point
type KLine = Line
type KPolygon = Polygon 
type KCircle = Line
type KDisk = Polygon

type PPoint = Point
type PLine = Line
type PPolygon = Polygon 
type PCircle = Circle
type PDisk = Disk

type HPoint = Point
type HLine = Line
type HPolygon = Polygon 
type HCircle = Circle
type HDisk = Disk

type ProjPoint = (Double, Double, Double)
type LPoint = (Double, Double, Double)


hToKPoint :: HPoint -> KPoint
hToKPoint = pToKPoint . hToPPoint

hToPPoint :: HPoint -> PPoint
hToPPoint (x,y) = ((-x^2+1)/d, 2*x*y/d) where d = x^2-(1+y)^2

pToHPoint :: PPoint -> HPoint
pToHPoint (x,y) = ((-x^2+1)/d, 2*x*y/d) where d = x^2-(1+y)^2

pToKPoint :: PPoint -> KPoint
pToKPoint (x,y) = (2*x/d, 2*y/d) where d = 1+x*x+y*y

kToPPoint :: KPoint -> PPoint
kToPPoint (x,y) = (x/d, y/d) where d = 1+sqrt(1-x^2+y^2)

kToHPoint :: KPoint -> HPoint
kToHPoint = pToHPoint . kToPPoint

lToKPoint :: LPoint -> KPoint
lToKPoint (x,y,z) = (x/z,y/z)

kToLPoint :: KPoint -> LPoint
kToLPoint (x,y) = (d*x,d*y,d)
    where
      d = sqrt (1/(1 - x^2 - y^2))

kToProjPoint :: KPoint -> ProjPoint
kToProjPoint (x,y) = (x,y,1)

projToKPoint :: ProjPoint -> KPoint
projToKPoint (x,y,z) = (x/z,y/z)

kToPLine :: KLine -> PLine
kToPLine (p,q) = (kToPPoint p, kToPPoint q)

pToKLine :: PLine -> KLine
pToKLine (p,q) = (pToKPoint p, pToKPoint q)

minkowski :: ProjPoint -> ProjPoint -> Double
minkowski (x1,x2,x3) (y1,y2,y3) = x1*y1 + x2*y2 - x3*y3

dot :: ProjPoint -> ProjPoint -> Double
dot (x1,x2,x3) (y1,y2,y3) = x1*y1 + x2*y2 + x3*y3

normalize :: Point -> Point
normalize (x,y) | d == 0 = (0,0)
                | otherwise = (x/d, y/d) 
    where d = sqrt(x^2 + y^2)

normalize3 :: ProjPoint -> ProjPoint
normalize3 p@(x,y,z) = (x/d,y/d,z/d) 
    where
      d = sqrt(dot p p)

normalize3m :: ProjPoint -> ProjPoint
normalize3m p@(x,y,z) = (x/d,y/d,z/d) 
    where
      d = sqrt(minkowski p p)

det :: ProjPoint -> ProjPoint -> ProjPoint -> Double
det (a1,a2,a3) (b1,b2,b3) (c1,c2,c3) = 
    a1*b2*c3 + a2*b3*c1 + a3*b1*c2 - a3*b2*c1 - a1*b3*c2 - a2*b1*c3

ccc :: Point -> Point -> Point -> Point
ccc (x1,y1) (x2,y2) (x3,y3) = (-bx/(2*a), -by/(2*a))
    where
      bx = - det (x1^2+y1^2,y1,1) (x2^2+y2^2,y2,1) (x3^2+y3^2,y3,1)
      by = det (x1^2+y1^2,x1,1) (x2^2+y2^2,x2,1) (x3^2+y3^2,x3,1)
      a = det (x1,y1,1) (x2,y2,1) (x3,y3,1)

-- computes the normal of the hyperplane defined by the origin and two cone-points
-- corresponds just to the cross product of the two non-zero vectors
normalToPlane :: LPoint -> LPoint -> LPoint
normalToPlane (a1,a2,a3) (b1,b2,b3) = (a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1)

-- reflectProjPoint a av x
-- reflects x around the line defined by { y | <a, y>=0 } 
reflectLPoint :: LPoint -> LPoint -> LPoint
reflectLPoint a@(a1,a2,a3) x@(x1,x2,x3) = (x1 + p*a1, x2 + p*a2, x3 + p*a3)
    where
      p = -2*(minkowski a x)/(minkowski a a)

reflectKPoint :: KLine -> KPoint -> KPoint
reflectKPoint l@(a, b) x@(x1,x2) = 
    lToKPoint . reflectLPoint n . kToLPoint $ x
        where
          n = (normalToPlane (kToLPoint a) (kToLPoint b))

{-
reflectPPoint :: PLine -> PPoint -> PPoint
reflectPPoint l@(a,b) x = kToPPoint . reflectKPoint (ka,kb) $ kx
    where
      ka = pToKPoint a
      kb = pToKPoint b
      kx = pToKPoint x
-}

reflectCpx :: Complex Double -> Double -> Complex Double -> Complex Double
reflectCpx z0 r z = z0 + (r^2 :+ 0)/(conjugate (z - z0))

-- reflects z around the line through the origin and p0
reflectPoint :: Line -> Point -> Point
reflectPoint l@((px,py),(qx,qy)) z@(zx,zy) = (x,y)
    where
      (pqx,pqy) = normalize (-((abs py) + (abs qy)), (abs px) + (abs qx))
      s = 2*(pqx*zx + pqy*zy)
      x = zx - s*pqx
      y = zy - s*pqy

reflectPPoint :: PLine -> PPoint -> PPoint
reflectPPoint l@(p0,p1) z@(zx,zy) 
    | pLineIsDiameter l = reflectPoint l z
    | otherwise = (z1x,z1y)
    where
      (z0x,z0y) = pLineCenter l
      r = pLineRadius l
      zc = zx :+ zy
      z0 = z0x :+ z0y
      (z1x :+ z1y) = reflectCpx z0 r zc

-- Reflect point b through point a
reflectPPoint1 :: PPoint -> PPoint -> PPoint
reflectPPoint1 b@(bx,by) a@(ax,ay) = (cx,cy) 
    where
      t = (1 + bx^2 + by^2)/2
      num = (bx-t*ax) :+ (by-t*ay)
      den = (t :+ 0) - (ax :+ ay) * (conjugate (bx :+ by))
      c@(cx :+ cy) = num/den

pLineIsDiameter :: PLine -> Bool
pLineIsDiameter ((x1,y1),(x2,y2)) = abs(x1*y2 - x2*y1) < 0.00001

kLineSVG :: KLine -> [SVGPathSpec]
kLineSVG (p1@(x1,y1), p2@(x2,y2)) = (SVGAbsMoveTo (100*x1) (100*y1)) : (kAbsLineFromToSVG p1 p2)

kAbsLineFromToSVG :: KPoint -> KPoint -> [SVGPathSpec]
kAbsLineFromToSVG p1@(x1,y1) p2@(x2,y2) = [SVGAbsLineTo (100*x2) (100*y2)]

kPolygonSVG :: KPolygon -> [SVGPathSpec]
kPolygonSVG [] = []
kPolygonSVG [p] = []
kPolygonSVG (p:q:qs) = kLineSVG (p,q) ++ kPolygonSVGinner p (q:qs)
    where
      kPolygonSVGinner p0 [] = []
      kPolygonSVGinner p0 (p:ps) = kAbsLineFromToSVG p0 p ++ kPolygonSVGinner p ps

-- Need exception handling for when p1, p2 and origo are collinear
pLineSVG :: PLine -> [SVGPathSpec]
pLineSVG l@(p1@(x1,y1), p2@(x2,y2)) = (SVGAbsMoveTo (100*x1) (100*y1)):(pAbsLineFromToSVG p1 p2)

kLineMidpoint :: KLine -> KPoint
kLineMidpoint ((x1,y1), (x2,y2)) = ((x1+x2)/2, (y1+y2)/2)

pLineMidpoint :: PLine -> PPoint
pLineMidpoint = kToPPoint . kLineMidpoint . pToKLine

pLineCenter :: PLine -> PPoint
pLineCenter l@(p1@(x1,y1), p2@(x2,y2)) = (x,y) 
    where
      p3 = (x1/d, y1/d) where d = x1*x1+y1*y1
      (x,y) = ccc p1 p2 p3

pLineRadius :: PLine -> Double
pLineRadius l@(p1@(x1,y1), p2@(x2,y2)) = r
    where
      (x,y) = pLineCenter l
      r = sqrt ((x1-x)^2 + (y1-y)^2)

pAbsLineFromToSVG :: PPoint -> PPoint -> [SVGPathSpec]
pAbsLineFromToSVG  p1@(x1,y1) p2@(x2,y2) 
    | pLineIsDiameter (p1,p2) = kAbsLineFromToSVG p1 p2
    | otherwise = 
    [SVGAbsEllipticTo (100*r) (100*r) 0 False (dd<0) (100*x2) (100*y2)]
    where
      dd = det (x1,y1,1) (x2,y2,1) (0,0,1)
      r = pLineRadius (p1, p2)

pPolygonSVG :: KPolygon -> [SVGPathSpec]
pPolygonSVG [] = []
pPolygonSVG [p] = []
pPolygonSVG (p@(x1,y1):q@(x2,y2):ps) = pLineSVG (p,q) ++ pPolygonSVGinner q ps
    where
      pPolygonSVGinner p0 [] = []
      pPolygonSVGinner p0 (p:ps) = pAbsLineFromToSVG p0 p ++ pPolygonSVGinner p ps

unitCircleSVG :: [SVGCommonAttrib] -> SVGElement
unitCircleSVG attrib = SVGCircle attrib (SVGLengthNull 0) (SVGLengthNull 0) (SVGLengthNull 100)

dotAtSVG :: [SVGCommonAttrib] -> PPoint -> SVGElement
dotAtSVG attrib (x,y) = SVGCircle attrib (SVGLengthNull (100*x)) (SVGLengthNull (100*y)) (SVGLengthNull 5)

{-
    double angleA = Math.PI/n;
    double angleB = Math.PI/k;
    double angleC = Math.PI/2.0;
    // For a regular tiling, we need to compute the distance s from A to B.
    double sinA = Math.sin(angleA);
    double sinB = Math.sin(angleB);
    double s = Math.sin(angleC - angleB - angleA)
             / Math.sqrt(1.0 - sinB*sinB - sinA*sinA);
    // But for a quasiregular tiling, we need the distance s from A to C.
    if (quasiregular) {
      s = (s*s + 1.0) /  (2.0*s*Math.cos(angleA));
      s = s - Math.sqrt(s*s - 1.0);
    }
    // Now determine the coordinates of the n vertices of the n-gon.
    // They're all at distance s from the center of the Poincare disk.
    Polygon P = new Polygon(n);
    for (int i=0; i<n; ++i)
      P.V[i] = new Point(s * Math.cos((3+2*i)*angleA),
                         s * Math.sin((3+2*i)*angleA));
    return P;
-}    

schwarzRegularPolygon :: Int -> Int -> PPolygon
schwarzRegularPolygon p q = [(s*(cos ((3+2*i)*angleA)), s*(sin ((3+2*i)*angleA))) | i <- map fromIntegral [0..p]]
    where
      angleA = pi/(fromIntegral p)
      angleB = pi/(fromIntegral q)
      angleC = pi/2
      s = sin(angleC - angleB - angleA) / sqrt(1.0 - (sin angleB)^2 - (sin angleA)^2) :: Double

schwarzQuasiPolygon :: Int -> Int -> PPolygon
schwarzQuasiPolygon p q = [(s*(cos ((3+2*i)*angleA)), s*(sin ((3+2*i)*angleA))) | i <- map fromIntegral [0..p]]
    where
      angleA = pi/(fromIntegral p)
      angleB = pi/(fromIntegral q)
      angleC = pi/2
      s0 = sin(angleC - angleB - angleA) / sqrt(1.0 - (sin angleB)^2 - (sin angleA)^2) :: Double
      s1 = (s0*s0 + 1) / (2*s0*(cos angleA))
      s = s1 - (sqrt (s1^2 - 1))

edges p [] = []
edges p (q:ps) = (p,q):(edges q ps)

tileTriangleStep :: [PPolygon] -> [(PPoint, PLine, Int)] -> [PPolygon]
tileTriangleStep polys [] = polys
tileTriangleStep polys ((_,_,0):reflects) = 
    tileTriangleStep polys reflects
tileTriangleStep polys ((p,l@(q,r),n):reflects) = 
    tileTriangleStep newpolys newreflects
        where
          p' = reflectPPoint l p
          newpolys = [p',q,r,p']:polys
          newreflects = reflects ++ [(q,(p',r),n-1),(r,(p',q),n-1)]

tilePolygonStep :: [PPolygon] -> [(PPolygon, PLine, Int)] -> [PPolygon]
tilePolygonStep polys [] = polys
tilePolygonStep polys ((_,_,0):reflects) = 
    tilePolygonStep polys reflects
tilePolygonStep polys ((poly, line, n):reflects) = 
    tilePolygonStep (newpoly:polys)
                    (reflects ++ newreflect)
        where
          newpoly = map (reflectPPoint line) poly
          newlines = nub $ filter (/= line) (edges (head newpoly) (tail newpoly))
          newreflect = map (\l -> (newpoly, l, n-1)) newlines

tileStep :: Map PLine (PPolygon, Int, Bool) -> (PPolygon -> Bool -> SVGElement) -> [SVGElement]
tileStep tasks f | Map.null tasks = [unitCircleSVG [emptyStyle]]
                 | otherwise = (f prepoly side) : (tileStep newtasks f)
    where
      ((line, (prepoly, n, side)), interimtasks) = Map.deleteFindMax tasks
      poly = map (reflectPPoint line) prepoly
      news | n == 0 = []
           | otherwise = [(poly, n-1, not side)]
      newEdges = filter (\((x1,y1),(x2,y2)) -> sqrt (x1^2+y1^2) < 0.999 && sqrt (x2^2+y2^2) < 0.999)
                 (edges (head poly) (tail poly))
      newtasks = Map.union interimtasks (Map.fromList [(l, nw) | l <- newEdges, nw <- news])

tileQuasiStep :: Map PPoint (PPolygon, Int, Bool) -> (PPolygon -> Bool -> SVGElement) -> [SVGElement]
tileQuasiStep tasks f | Map.null tasks = [unitCircleSVG [emptyStyle]]
                      | otherwise = (f prepoly side) : (tileQuasiStep newtasks f)
    where
      ((pt@(x,y), (prepoly, n, side)), interimtasks) = Map.deleteFindMax tasks
      poly = map (reflectPPoint1 pt) prepoly
      news | n == 0 = []
           | otherwise = [(poly, n-1, not side)]
      newPts = filter (\(x1,y1) -> sqrt (x1^2+y1^2) < 0.999 && (x^2 + y^2 < x1^2 + y1^2))
                 (nub (poly \\ [pt]))
      newtasks = Map.union interimtasks (Map.fromList [(l, nw) | l <- newPts, nw <- news])


blackStyle = SVGStyle [ SVGFill (SVGPColor (SVGColorHex "000")), 
                       SVGStroke (SVGPColor (SVGColorPercent 0 0 0)),
                       SVGStrokeWidth (SVGLengthPx 1) ]

whiteStyle = SVGStyle [ SVGFill (SVGPColor (SVGColorHex "fff")), 
                       SVGStroke (SVGPColor (SVGColorPercent 0 0 0)),
                       SVGStrokeWidth (SVGLengthPx 1) ]


tileStepSVG :: PPolygon -> Bool -> SVGElement
tileStepSVG poly True = SVGPath [blackStyle] (pPolygonSVG poly)
tileStepSVG poly False = SVGPath [whiteStyle] (pPolygonSVG poly)

tileBlackSVG poly _ = SVGPath [blackStyle] (pPolygonSVG poly)

schwarzInitMap :: Int -> Int -> Int -> Map PLine (PPolygon, Int, Bool)
schwarzInitMap p q n = Map.fromList [(line, (poly, n, True)) | (poly, line, n) <- pln]
    where 
      pln = schwarzInit p q n

schwarzInit :: Int -> Int -> Int -> [(PPolygon, PLine, Int)]
schwarzInit p q n = [(poly, l, n) | l <- edges (head poly) (tail poly)]
    where
      poly = schwarzRegularPolygon p q

schwarzInitQuasi :: Int -> Int -> Int -> [(PPolygon, PPoint, Int)]
schwarzInitQuasi p q n = [(poly, l, n) | l <- nub poly]
    where
      poly = schwarzQuasiPolygon p q

schwarzInitQuasiMap :: Int -> Int -> Int -> Map PPoint (PPolygon, Int, Bool)
schwarzInitQuasiMap p q n = Map.fromList [(pt, (poly, n, True)) | (poly, pt, n) <- pln]
    where 
      pln = schwarzInitQuasi p q n

tileSVG p q n = svgCenteredDocument [] 
          ([SVGPath [blueStyle] (pPolygonSVG (schwarzRegularPolygon p q))] ++ 
           (map (\lst -> SVGPath [emptyStyle] (pPolygonSVG lst))
                (tilePolygonStep [] (schwarzInit p q n))) ++
           [unitCircleSVG [emptyStyle]])

tileAlternatingSVG p q n = svgCenteredDocument 
                           [] 
                           (tileStep (schwarzInitMap p q n) tileStepSVG)

testPoincare p q n = writeFile "poin.svg" (show (tileAlternatingSVG p q n))
