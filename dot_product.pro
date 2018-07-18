;+
; :NAME:
;    DOT_PRODUCT
;
; :PURPOSE:
;    This function will compute the dot product between two provided vectors 
;    (or arrays or vectors) in n-space.
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    Result = DOT_PRODUCT( vector1, vector2 )
;
; :INPUTS:
;    vector1
;       A n-dimensional vector -OR- an array of N n-dimensional vectors (nxN) 
;    vector2
;       A n-dimensional vector -OR- an array of N n-dimensional vectors (nxN) 
;       (NOTE: The provided vector dimensions and number of vectors must be equal)
;
; :KEYWORD PARAMETERS:
;    None
;    
; :RESULT:
;    A scalar or a vector containg the dot product(s) between the provided
;    vectors.
;
; :SIDE EFFECTS:
;    If the size of the provided vectors do not match, execution will be halted
;    and a session reset will occur.
;
; :REQUIRES:
;    ARRAYS_ARE_SAME_SIZE
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

FUNCTION DOT_PRODUCT, vector1, vector2

;+
; Check if the dimension and the number of vectors are the same for both provided
; arguments
;-
   IF NOT ARRAYS_ARE_SAME_SIZE( vector1, vector2 ) THEN BEGIN
      MESSAGE, "Incompatible vector/array dimensions.", /CONTINUE
      HELP, vector1
      HELP, vector2
      RETALL
   ENDIF

;+
; Create double-precision local copies of the provided vector(s) for use in the
; computation of the dot product
;-
   v1 = DOUBLE( vector1 )
   v2 = DOUBLE( vector2 )

;+
; Return the dot product(s) between the provided vector(s)
;-
   RETURN, TOTAL( v1 * v2, 1 )

END