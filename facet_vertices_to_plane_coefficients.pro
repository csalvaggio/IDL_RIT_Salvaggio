;+
; :NAME:
;    FACET_VERTICES_TO_PLANE_COEFFICIENTS
;
; :PURPOSE:
;    This function will compute the coefficients of a plane equation of the form
;       Ax + By + Cz + D = 0
;    from a set of triangilar facet vertices.  The surface normal can also be
;    computed if desired.
;
; :CATEGORY:
;    Graphics.
;
; :CALLING SEQUENCE:
;    Result = FACET_VERTICES_TO_PLANE_COEFFICIENTS( vertices, 
;                                                   FACET_NORMAL=facetNormal, 
;                                                   NORMALIZE=normalize )
;
; :INPUTS:
;    vertices
;       A structure containing the vertices of a triangular facet - the tags
;       are POINT1, POINT2, and POINT3 and these represent the 3-dimensional
;       coordinates, defined as a 3-element vector, for the vertices in 
;       counter-clockwise order so that the right-hand rule holds for defining
;       the surface normal
;
; :KEYWORD PARAMETERS:
;    FACET_NORMAL
;       This keyword will define a named variable that will contain the surface
;       normal upon completion of this routine
;    NORMALIZE
;       This keyword, used in combination with FACET_NORMAL, will cause the
;       surface normal vector to be a unit vector
;    
; :RESULT:
;    Result will be a 4-element vector containing the coefficients of the plane
;    equation of the form [A, B, C, D].
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    NORMALIZE
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

FUNCTION FACET_VERTICES_TO_PLANE_COEFFICIENTS, vertices, FACET_NORMAL=facetNormal, NORMALIZE=normalize

   coefficients = [  DETERM( [ TRANSPOSE( [1,1,1] ), $
                               TRANSPOSE( [vertices.point1[1],vertices.point2[1],vertices.point3[1]] ), $
                               TRANSPOSE( [vertices.point1[2],vertices.point2[2],vertices.point3[2]] ) ], /CHECK ), $
                     DETERM( [ TRANSPOSE( [vertices.point1[0],vertices.point2[0],vertices.point3[0]] ), $
                               TRANSPOSE( [1,1,1] ), $
                               TRANSPOSE( [vertices.point1[2],vertices.point2[2],vertices.point3[2]] ) ], /CHECK ), $
                     DETERM( [ TRANSPOSE( [vertices.point1[0],vertices.point2[0],vertices.point3[0]] ), $
                               TRANSPOSE( [vertices.point1[1],vertices.point2[1],vertices.point3[1]] ), $
                               TRANSPOSE( [1,1,1] ) ], /CHECK ), $
                    -DETERM( [ TRANSPOSE( [vertices.point1[0],vertices.point2[0],vertices.point3[0]] ), $
                               TRANSPOSE( [vertices.point1[1],vertices.point2[1],vertices.point3[1]] ), $
                               TRANSPOSE( [vertices.point1[2],vertices.point2[2],vertices.point3[2]] ) ], /CHECK ) ]

   facetNormal = [ coefficients[0], coefficients[1], coefficients[2] ]

   IF KEYWORD_SET( normalize ) THEN facetNormal = NORMALIZE( facetNormal )

   RETURN, coefficients

END