;+
; :NAME:
;    ARRAYS_ARE_SAME_SIZE
;
; :PURPOSE:
;    This function will check if two provide arrays are the same size.  This 
;    routine only checks the dimensions (no other characteristics are 
;    considered).
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    Result = ARRAYS_ARE_SAME_SIZE( array1, array2 )
;
; :INPUTS:
;    array1
;       A valid IDL array or vector data element 
;    array2
;       A valid IDL array or vector data element 
;
; :KEYWORD PARAMETERS:
;    None
;    
; :RESULT:
;    This function will return a boolean (1) if the provided arrays are the same
;    dimensions or a boolean (0) if they are not.
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

FUNCTION ARRAYS_ARE_SAME_SIZE, array1, array2

;+
; Determine the dimensions of the provided arrays
;-
   array1Dimensions = SIZE( array1, /DIMENSIONS )
   array2Dimensions = SIZE( array2, /DIMENSIONS )

;+
; Determine the number of dimensions in each array
;-
   array1NumberDimensions = N_ELEMENTS( array1Dimensions )
   array2NumberDimensions = N_ELEMENTS( array2Dimensions )

;+
; If the number of dimensions do not match, return a FALSE
;-
   IF ( array1NumberDimensions NE array2NumberDimensions ) THEN RETURN, 0

;+
; If any of the dimensions are different in size, return a FALSE
;-
   FOR dimension = 0, array1NumberDimensions-1 DO BEGIN
      IF ( array1Dimensions[dimension] NE array2Dimensions[dimension] ) THEN RETURN, 0
   ENDFOR

;+
; Return a TRUE indicating that the dimensions of the provided arrays are the same
;-
   RETURN, 1

END