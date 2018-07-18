;+
; :NAME:
;    REPLICATE_VECTOR
;
; :PURPOSE:
;    This function will replicate a provided vector as either the rows or
;    columns of a 2-dimensional array.  This provides a utility similar to
;    the REPLICATE functions provided in IDL for scalar values.
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    Result = REPLICATE_VECTOR( vector, numberReplicates, COLUMNS=columns )
;
; :INPUTS:
;    vector
;       An n-dimensional valid IDL vector data element 
;    numberReplicates
;       The number of times the vector should be replicated in the rows
;       of a 2-dimensional array
;
; :KEYWORD PARAMETERS:
;    COLUMNS
;       Set this keyword (/COLUMNS) if you'd like to replicate the vector
;       in the columns of the array rather than the default rows
;    
; :RESULT:
;    Result will be an nxN array with the provided n-dimensional vector
;    replicated in the N rows of this array.  Result will be Nxn if the
;    /COLUMNS keyword was specified.
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

FUNCTION REPLICATE_VECTOR, vector, numberReplicates, COLUMNS=columns

   IF KEYWORD_SET( columns ) THEN BEGIN
      RETURN, vector ## REPLICATE( 1, numberReplicates, 1 )
   ENDIF ELSE BEGIN
      RETURN, REPLICATE( 1, numberReplicates ) ## vector
   ENDELSE

END