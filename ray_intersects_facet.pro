;+
; :NAME:
;    RAY_INTERSECTS_FACET
;
; :PURPOSE:
;    This function determines whether a given ray (or set of rays) 
;    intersect a provided triangular facet in 3-space.
;
; :CATEGORY:
;    Graphics.
;
; :CALLING SEQUENCE:
;    Result = RAY_INTERSECTS_FACET( rayOrigin,
;                                   rayDirection,
;                                   facet,
;                                   INTERSECTION_POINT=intersectionPoint,
;                                   INTERSECTION_DISTANCE=intersectionDistance,
;                                   INTERSECTION_ANGLE=intersectionAngle )
;
; :INPUTS:
;    rayOrigin
;       A 3-element vector -OR- a 3xN element array containing the origin (or
;       origins) of the cast ray(s)
;    rayDirection
;       A complementary 3-element vector -OR- 3xN element array containing the
;       direction vector (or direction vectors) for each of the ray origins
;       described by rayOrigin
;    facet
;       A structure containing the vertices of a triangular facet - the tags
;       are POINT1, POINT2, and POINT3 and these represent the 3-dimensional
;       coordinates, defined as a 3-element vector, for the vertices in 
;       counter-clockwise order so that the right-hand rule holds for defining
;       the surface normal
;
; :KEYWORD PARAMETERS:
;    INTERSECTION_POINT
;       A named variable that upon completion of the routine will contain a 
;       3-element vector -OR- 3xN element array of ray-plane intersection
;       point(s)
;    INTERSECTION_DISTANCE
;       A named variable that upon completion of the routine will contain a 
;       scalar -OR- N-element vector of intersection distance(s) along each ray-
;       facet path
;    INTERSECTION_ANGLE
;       A named variable that upon completion of the routine will contain a 
;       scalar -OR- N-element vector of intersection angle(s) that the ray(s)
;       make with the interesected plane
;
;    NOTE: These keyword parameters will contain the values noted above even if
;          the ray did not intersect the plane within the triangular boundaries
;    
; :RESULT:
;    A boolean status flag (or vector of flags) indicating whether the provided
;    ray (or set of rays) intersect the triangular facet specified; 1 indicates 
;    the intersection (or hit) occurred, 0 indicates that it did not.  The criteria
;    for the hit are
;       1) the ray intersects the plane inside the triangular boundary
;       2) the ray intersects the plane on the same side as the surface normal
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    ANGLE_BETWEEN_TWO_VECTORS
;    DOT_PRODUCT
;    FACET_VERTICES_TO_PLANE_COEFFICIENTS
;    NORMALIZE
;    POINT_INSIDE_TRIANGULAR_FACET
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

FUNCTION RAY_INTERSECTS_FACET, rayOrigin, $
                               rayDirection, $
                               facet, $
                               INTERSECTION_POINT=intersectionPoint, $
                               INTERSECTION_DISTANCE=intersectionDistance, $
                               INTERSECTION_ANGLE=intersectionAngle

;+
; Determine the equation of the plane through the three vertices of the provided
; facet and determine the unnormalized facet normal
;-
   planeCoefficients = FACET_VERTICES_TO_PLANE_COEFFICIENTS( facet, FACET_NORMAL=facetNormal )
   A = planeCoefficients[0]
   B = planeCoefficients[1]
   C = planeCoefficients[2]
   D = planeCoefficients[3]

;+
; Determine the number of rays provided
;-
   dimensions = SIZE( rayDirection, /DIMENSIONS )
   numberRays = ( N_ELEMENTS( dimensions ) EQ 1 ) ? 1 : dimensions[1]

;+
; Created replicated data structures to allow multiple ray computations to occur and
; eliminate recurrent computations where possible
;-
   replicatedFacetNormal = REPLICATE_VECTOR( facetNormal, numberRays )
   normalizedRayDirection = NORMALIZE( rayDirection )

;+
; Determine the distance between the ray origin and the intersection point of the 
; ray (line) with the plane through the three vertices of the provided facet (the 
; intersection point may be behind the ray origin resulting in a negative distance,
; however, for this demonstration this will be ignored as the point in facet routine
; should remove these intersections)
;-
   intersectionDistance = -( D + DOT_PRODUCT( rayOrigin, replicatedFacetNormal ) ) / $
                          DOT_PRODUCT( normalizedRayDirection, replicatedFacetNormal )

;+
; Determine the intersection point of the ray (line) with the plane through the 
; three vertices of the provided facet by multiplying the intersection distance by the
; normalized ray direction vector and adding this to the ray origin point
;-
   IF ( numberRays EQ 1 ) THEN BEGIN
      intersectionPoint = rayOrigin + intersectionDistance[0] * normalizedRayDirection
   ENDIF ELSE BEGIN
      transposeDistance = TRANSPOSE( intersectionDistance )
      intersectionPoint = rayOrigin + [ transposeDistance, transposeDistance, transposeDistance ] * normalizedRayDirection
   ENDELSE

;+
; Determine if the intersection point of the ray and the plane falls within the 
; provided facet boundary
;-
   hit = POINT_INSIDE_TRIANGULAR_FACET( intersectionPoint, facet )

;+
; It is now necessary to check the angle of intersection between the ray and the 
; facet normal to see if the intersection is occurring from the front or the back
; side of the facet - only front side intersections can result in a visible facet
; (a front side intersection is represented by an intersection angle in the half-
; open interval (90,180] degrees)
;-
   intersectionAngle = ANGLE_BETWEEN_TWO_VECTORS( rayDirection, replicatedFacetNormal )

   backsideIntersection = WHERE( intersectionAngle LE (90 * !DTOR), numberBackside )

;+
; A final decision can now be made concerning the ray-facet intersection by using
; the information gathered as we know if the point is inside the provided facet
; boundaries whether the ray is intersecting the facet from the back side
;-
   IF ( numberBackside GT 0 ) THEN hit[backsideIntersection] = 0

;+
; Return the hit status scalar/vector to the calling routine
;-
   IF ( numberRays EQ 1 ) THEN BEGIN
      hit = hit[0]
      intersectionAngle = intersectionAngle[0]
      intersectionDistance = intersectionDistance[0]
      intersectionPoint = REFORM( intersectionPoint )
   ENDIF

   RETURN, hit

END