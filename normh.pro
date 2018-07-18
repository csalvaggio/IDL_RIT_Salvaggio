;+
; :NAME:
;    NORMH
;
; :PURPOSE:
;    This function normalizes the provided array by the homogeneous
;    coordinate.
;
; :CATEGORY:
;    GENERAL.
;
; :CALLING SEQUENCE:
;    result = NORMH( array, /COLUMN )
;
; :INPUTS:
;    array
;       A one-dimensional or two-dimensional homogenous array. If the 
;       array is two-dimensional, the last row is assumed to contain the
;       homogenous coordinates (points in columns).
;
; :KEYWORD PARAMETRS:
;      COLUMN
;         Set this keyword if the last column of the provided array
;         contains the homogeneous coordinates (points in rows).
;
; :ERROR CHECKING:
;    This routine will only work for one- or two-dimensional arrays.
;
; :SIDE EFFECTS:
;    The normalized return value will always be of data type DOUBLE.
;
; :REQUIRES:
;    REPLICATE_VECTOR
;
; :MODIFICATION HISTORY:
;    Written by:       Philip S. Salvaggio
;    February, 2011    Original code
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

FUNCTION NORMH, array, COLUMN=column

   arrayDimensions = SIZE( array, /DIMENSIONS )

   CASE SIZE( array, /N_DIMENSIONS ) OF
      1: $
         BEGIN
            RETURN, ( array[-1] NE 0 ) ? array / DOUBLE( array[-1] ) : DOUBLE( array )
         END
      2: $
         BEGIN
            IF ( KEYWORD_SET( column ) && column ) THEN BEGIN
               hCoords = array[-1,*]
               hCoords[ WHERE( hCoords EQ 0, /NULL ) ] = 1
               divisors = REPLICATE_VECTOR( hCoords, arrayDimensions[0], /COLUMN )
            ENDIF ELSE BEGIN
               hCoords = REFORM( array[*,-1] )
               hCoords[ WHERE( hCoords EQ 0, /NULL ) ] = 1
               divisors = REPLICATE_VECTOR( hCoords, arrayDimensions[1] )
            ENDELSE
            RETURN, array / DOUBLE( divisors )
         END
      ELSE: $
         BEGIN
            MESSAGE, 'Provided array must be 1 or 2 dimensions'
         END
   ENDCASE

END