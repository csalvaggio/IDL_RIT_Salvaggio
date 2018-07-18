;+
; :NAME:
;    POINT_INSIDE_TRIANGULAR_FACET
;
; :PURPOSE:
;    This function determines whether a provided point (or set of points) falls
;    within the boundaries of a triangular facet.  This function uses the 
;    technique based upon the barycentric coordinate test described at
;    http://www.devmaster.net/wiki/Ray-triangle_intersection.
;
; :CATEGORY:
;    Graphics.
;
; :CALLING SEQUENCE:
;    Result = POINT_INSIDE_TRIANGULAR_FACET( point, facet )
;
; :INPUTS:
;    point
;       A 3-element vector containing the x,y,z coordinate -OR- an
;       array of 3-element vectors (3xN) containing the x,y,z coordinates
;       of the point(s) to be evaluated to see if they fall within
;       the provided triangular facet 
;    facet
;       A structure containing the vertices of a triangular facet - the tags
;       are POINT1, POINT2, and POINT3 and these represent the 3-dimensional
;       coordinates, defined as a 3-element vector, for the vertices in 
;       counter-clockwise order so that the right-hand rule holds for defining
;       the surface normal
;
; :KEYWORD PARAMETERS:
;    None
;    
; :RESULT:
;    A boolean status flag (or vector of flags) indicating whether the provided
;    point (or set of points) are contained within the triangular boundary
;    specified; 1 indicates the point is contained in the boundary, 0 indicates
;    it does not.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    REPLICATE_VECTOR
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    September, 2009   Original code
; 
; :DISCLAIMER:
;    This source code is provided "as is" and without warranties as to performance 
;    or merchantability. The author and/or distributors of this source code may 
;    have made statements about this source code. Any such statements do not 
;    constitute warranties and shall not be relied on by the user in deciding 
;    whether to use this source code.
;
;    This source code is provided without any express or implied warranties 
;    whatsoever. Because of the diversity of conditions and hardware under which 
;    this source code may be used, no warranty of fitness for a particular purpose 
;    is offered. The user is advised to test the source code thoroughly before 
;    relying on it. The user must assume the entire risk of using the source code.
;-

FUNCTION POINT_INSIDE_TRIANGULAR_FACET, point, facet

;+
; Determine the number of points (x,y,z triplets) provided by the calling routine
;-
   numberPoints = N_ELEMENTS( point ) / 3

;+
; Determine the facet normal and the dominant component of that vector
;-
   facetNormal = CROSSP( facet.point2 - facet.point1, facet.point3 - facet.point1 )
   maximumComponent = MAX( ABS( facetNormal ), dominantComponent )

;+
; Use the knowledge of the dominant component to eliminate the dominant axis in the
; point and facet data
;-
   CASE dominantComponent OF
      0: BEGIN
            pPrimed = [ point[1,*], point[2,*] ]
            aPrimed = REPLICATE_VECTOR( [ facet.point1[1], facet.point1[2] ], numberPoints )
            bPrimed = REPLICATE_VECTOR( [ facet.point2[1], facet.point2[2] ], numberPoints )
            cPrimed = REPLICATE_VECTOR( [ facet.point3[1], facet.point3[2] ], numberPoints )
         END
      1: BEGIN
            pPrimed = [ point[0,*], point[2,*] ]
            aPrimed = REPLICATE_VECTOR( [ facet.point1[0], facet.point1[2] ], numberPoints )
            bPrimed = REPLICATE_VECTOR( [ facet.point2[0], facet.point2[2] ], numberPoints )
            cPrimed = REPLICATE_VECTOR( [ facet.point3[0], facet.point3[2] ], numberPoints )
         END
      2: BEGIN
            pPrimed = [ point[0,*], point[1,*] ]
            aPrimed = REPLICATE_VECTOR( [ facet.point1[0], facet.point1[1] ], numberPoints )
            bPrimed = REPLICATE_VECTOR( [ facet.point2[0], facet.point2[1] ], numberPoints )
            cPrimed = REPLICATE_VECTOR( [ facet.point3[0], facet.point3[1] ], numberPoints )
         END
   ENDCASE

;+
; Compute the vectors representing each side of the triangular facet as it is projected
; onto the plane orthogonal to the dominant axis of the facet normal vector
;-
   b = bPrimed - aPrimed
   c = cPrimed - aPrimed
   p = pPrimed - aPrimed

;+
; Compute the barycentric coordinates of the ray-plane intersection point on the projected
; triangle computed above
;-
   u = ( p[1,*]*c[0,*] - p[0,*]*c[1,*] ) / ( b[1,*]*c[0,*] - b[0,*]*c[1,*] )
   v = ( p[1,*]*b[0,*] - p[0,*]*b[1,*] ) / ( c[1,*]*b[0,*] - c[0,*]*b[1,*] )

;+
; Determine if the barycentric coordinates meet the criteria for the point falling
; inside the triangular facet boundaries, namely:
;    u >= 0
;    v >= 0
;    (u+v) <= 1
;-
   insideIndex = WHERE( ( u GE 0 ) AND ( v GE 0 ) AND ( (u + v) LE 1 ), numberInside )

;+
; If the provided point data represents a single point, convert the boolean status 
; flag to a scalar, otherwise, form a vector of status flags
;-
   insideStatus = LONARR( numberPoints )
   IF ( numberInside GT 0 ) THEN insideStatus[insideIndex] = 1
   IF ( numberPoints EQ 1 ) THEN insideStatus = insideStatus[0]

;+
; Return the flag(s) representing the status of the provided point(s) relative
; to the triangular facet boundaries to the calling routine
;-
   RETURN, insideStatus

END