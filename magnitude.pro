;+
; :NAME:
;    MAGNITUDE
;
; :PURPOSE:
;    This function will compute the magnitude (length) of the provided n-dimensional
;    vector.
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    Result = MAGNITUDE( vector )
;
; :INPUTS:
;    vector
;       A n-dimensional vector -OR- an array of N n-dimensional vectors (nxN) 
;
; :KEYWORD PARAMETERS:
;    None
;    
; :RESULT:
;    A scalar or a vector containg the magnitude(s) of the provided vectors.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    None
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

FUNCTION MAGNITUDE, vector

;+
; Create a double-precision local copy of the provided vector(s) for use in the
; computation of the magnitude
;-
   v = DOUBLE( vector )
   
;+
; Return the magnitude(s) of the provided vector(s)
;-
   RETURN, SQRT( TOTAL( v * v, 1 ) )

END