;+
; :NAME:
;    PROJECTIVE_MAPPING_MATRIX
;
; :PURPOSE:
;    This function will compute the projective transformation matrix
;    to allow the conversion between two sets of quadrilateral
;    vertices, one set being considered the image and the other being 
;    considered the map.  The determined transformation, mappingMatrix,
;    can be subsequently used to convert a homogenous map coordinate [u,v] 
;    to a homogenous image coordinate [x,y] according to the following
;    expression
;
;    [x, y, 1] = TRANSPOSE( NORMH( mappingMatrix ## TRANSPOSE( [u, v, 1] ) ) )
;
; :CATEGORY:
;    GENERAL.
;
; :CALLING SEQUENCE:
;    result = PROJECTIVE_MAPPING_MATRIX( imageX, imageY, mapX, mapY )
;
; :INPUTS:
;    imageX
;       A 4-element vector containing the x coordinates of the vertices
;       defining the quadritalteral on the image to be transformed.
;    imageY
;       A 4-element vector containing the y coordinates of the vertices
;       defining the quadritalteral on the image to be transformed.
;    mapX
;       A 4-element vector containing the x coordinates of the vertices
;       defining the quadritalteral on the map to serve as a target for
;       the transform.
;    mapY
;       A 4-element vector containing the y coordinates of the vertices
;       defining the quadritalteral on the map to serve as a target for
;       the transform.
;
; :KEYWORD PARAMETRS:
;    None.
;
; :ERROR CHECKING:
;    None.
;
; :SIDE EFFECTS:
;    None.
;
; :REQUIRES:
;    None.
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    January, 2012     Original code
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

FUNCTION PROJECTIVE_MAPPING_MATRIX, imageX, imageY, mapX, mapY

   x = DOUBLE( imageX )
   y = DOUBLE( imageY )
   u = DOUBLE( mapX )
   v = DOUBLE( mapY )

   mappingVector = INVERT( [ [ u[0], v[0], 1, 0,    0,    0, -u[0]*x[0], -v[0]*x[0] ], $
                             [ u[1], v[1], 1, 0,    0,    0, -u[1]*x[1], -v[1]*x[1] ], $
                             [ u[2], v[2], 1, 0,    0,    0, -u[2]*x[2], -v[2]*x[2] ], $
                             [ u[3], v[3], 1, 0,    0,    0, -u[3]*x[3], -v[3]*x[3] ], $
                             [ 0,    0,    0, u[0], v[0], 1, -u[0]*y[0], -v[0]*y[0] ], $
                             [ 0,    0,    0, u[1], v[1], 1, -u[1]*y[1], -v[1]*y[1] ], $
                             [ 0,    0,    0, u[2], v[2], 1, -u[2]*y[2], -v[2]*y[2] ], $
                             [ 0,    0,    0, u[3], v[3], 1, -u[3]*y[3], -v[3]*y[3] ] ] ) ## $
                   TRANSPOSE( [ x[0], x[1], x[2], x[3], y[0], y[1], y[2], y[3] ] )
                   

   mappingMatrix = [ [ mappingVector[0], mappingVector[1], mappingVector[2] ], $ 
                     [ mappingVector[3], mappingVector[4], mappingVector[5] ], $
                     [ mappingVector[6], mappingVector[7], 1 ] ]

   RETURN, mappingMatrix

END