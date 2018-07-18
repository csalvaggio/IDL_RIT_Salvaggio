;+
; :NAME:
;    TAG_EXISTS
;
; :PURPOSE:
;    This function will search a structure for the existence of a specified
;    tag.
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    Result = TAG_EXISTS( tag, structure )
;
; :INPUTS:
;       tag
;          A scalar string specifying the tag to search for
;       structure
;          A structure variable to search
;
; :KEYWORD PARAMETERS:
;    None
;
; :RESULT:
;    Result is a boolean status flag indicating whether the specified
;    tag exists in the provided structure
;    
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    November, 2009    Original code
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

FUNCTION TAG_EXISTS, tag, structure

   IF ( N_ELEMENTS( structure ) EQ 0 ) THEN RETURN, 0
   tagNames = STRUPCASE( TAG_NAMES( structure ) )
   index = WHERE( tagNames EQ STRUPCASE( tag ), count )
   RETURN, count GT 0

END