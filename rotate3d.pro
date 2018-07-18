;+
; :NAME:
;    ROTATE3D
;
; :PURPOSE:
;    This function will perform a rotation of the provided homogenous point 
;    in 3-space.  The result represents the orthogonal matrix corresponding 
;    to a clockwise/left-handed rotation with Euler angles angleX, angleY, 
;    and angleZ with x-y-z convention.
;
; :CATEGORY:
;    GENERAL.
;
; :CALLING SEQUENCE:
;    result = ROTATE3D( point, angleX, angleY, angleZ, /DEGREES )
;
; :INPUTS:
;    point
;       A vector containing the three-dimensional point to be rotated in
;       homogenous coordinates [x,y,z,1].
;    angleX
;       The Euler angle through which to rotate the point around the x axis.
;    angleY
;       The Euler angle through which to rotate the point around the y axis.
;    angleZ
;       The Euler angle through which to rotate the point around the z axis.
;
; :KEYWORD PARAMETRS:
;      DEGREES
;         Set this keyword to interpret the provided Euler angles of rotation
;         in degrees.  If this keyword is omitted, the angles are assumed to be 
;         specified in radians.
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

FUNCTION ROTATE3D, point, angleX, angleY, angleZ, DEGREES=inDegrees

   IF KEYWORD_SET( inDegrees ) THEN BEGIN
      phi = angleX * !DTOR
      theta = angleY * !DTOR
      psi = angleZ * !DTOR
   ENDIF ELSE BEGIN
      phi = DOUBLE( angleX )
      theta = DOUBLE( angleY )
      psi = DOUBLE( angleZ )
   ENDELSE

   r = [ [ COS( theta )*COS( psi ), $
           -COS( phi )*SIN( psi ) + SIN( phi )*SIN( theta )*COS( psi ), $
           SIN( phi )*SIN( psi ) + COS( phi )*SIN( theta )*COS( psi ), $
           0 ], $
         [ COS( theta )*SIN( psi ), $
           COS( phi )*COS( psi ) + SIN( phi )*SIN( theta )*SIN( psi ), $
           -SIN( phi )*COS( psi ) + COS( phi )*SIN( theta )*SIN( psi ), $
           0 ], $
         [ -SIN( theta ), $
           SIN( phi )*COS( theta ), $
           COS( phi )*COS( theta ), $
           0 ], $
         [ 0D, 0D, 0D, 1D] ]

   RETURN, point ## r

END