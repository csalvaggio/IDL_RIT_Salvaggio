;+
; :NAME:
;    NORMALIZE
;
; :PURPOSE:
;    This function will normalize or compute the unit vector equivalent for
;    the provided vector.
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    Result = NORMALIZE( vector )
;
; :INPUTS:
;    vector
;       A n-dimensional vector -OR- an array of N n-dimensional vectors (nxN) 
;
; :KEYWORD PARAMETERS:
;    None
;    
; :RESULT:
;    A scalar or a vector containg the normalized version of the provided vector(s).
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    NORMALIZE
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

FUNCTION NORMALIZE, vector

;+
; Determine the dimensionality of the vector(s) provided
;-
   dimensions = SIZE( vector, /DIMENSIONS )
   numberElements = dimensions[0]

;+
; Create a double-precision local copy of the provided vector(s) for use in the
; normalization computation
;-
   v = DOUBLE( vector )

;+
; Replicate the magnitude in a vector the same size as the provided vector dimensions
;-
   m = REPLICATE_VECTOR( MAGNITUDE( v ), numberElements, /COLUMNS )

;+
; Return the normalized vector(s)
;-
   RETURN, v / m

END