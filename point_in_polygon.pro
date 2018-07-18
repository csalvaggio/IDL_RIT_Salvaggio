;+
; :NAME:
;    POINT_IN_POLYGON
;
; :PURPOSE:
;    This function will determine whether the supplied point is contained
;    within the supplied general polygon
;
; :CATEGORY:
;    Graphics.
;
; :CALLING SEQUENCE:
;    Result = POINT_IN_POLYGON( point, polygon )
;
; :INPUTS:
;    point
;       A vector containing the (x,y) coordinates for the point being checked
;    polygon
;       A structure describing the polygon that contains the following tags
;          NV  number of vertices in the polygon
;          V   2xNV array containing the indivudual vertices of the polygon
;
; :KEYWORD PARAMETERS:
;    None
;    
; :RESULT:
;    A boolean status flag indicating whether the provided point is contained
;    within the polygonal boundary specified; 1 indicates the point is contained
;    in the boundary, 0 indicates it does not.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    October, 2003     Original code
;    May, 2004         Fixed bug when polygon data was not a floating
;                      point data type
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

FUNCTION POINT_IN_POLYGON, point, polygon

   pointInside = 0B

   j = polygon.NV-1
   
   FOR i = 0, polygon.NV-1 DO BEGIN
      
      u0 = [ DOUBLE( polygon.V[0,i] ), DOUBLE( polygon.V[1,i] ) ]
      u1 = [ DOUBLE( polygon.V[0,j] ), DOUBLE( polygon.V[1,j] ) ]

      IF ( point[1] LT u1[1] ) THEN BEGIN
         IF ( u0[1] LE point[1] ) THEN BEGIN
            IF ( ( point[1] - u0[1] ) * ( u1[0] - u0[0] ) GT $
                 ( point[0] - u0[0] ) * ( u1[1] - u0[1] ) ) THEN BEGIN
               IF ( pointInside EQ 0B ) THEN BEGIN
                  pointInside = 1B 
               ENDIF ELSE BEGIN
                  pointInside = 0B
               ENDELSE
            ENDIF
         ENDIF
      ENDIF ELSE BEGIN
         IF ( point[1] LT u0[1] ) THEN BEGIN
            IF ( ( point[1] - u0[1] ) * ( u1[0] - u0[0] ) LT $
                 ( point[0] - u0[0] ) * ( u1[1] - u0[1] ) ) THEN BEGIN
               IF ( pointInside EQ 0B ) THEN BEGIN
                  pointInside = 1B 
               ENDIF ELSE BEGIN
                  pointInside = 0B
               ENDELSE
            ENDIF
         ENDIF
      ENDELSE

      j = i

   ENDFOR

   RETURN, pointInside

END
