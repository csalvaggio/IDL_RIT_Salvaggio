;+
; :NAME:
;    IS_EQUAL
;
; :PURPOSE:
;    This function will determine if two scalars are equal.  This function is 
;    intended to be used when comparing two floating point values to determine 
;    in they are "close enough" to each other to be considered equal.
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    Result = IS_EQUAL( value1, value2, ACCEPTABLE_DIFFERENCE=acceptableDifference )
;
; :INPUTS:
;    value1
;       A scalar value
;    value2
;       A scalar value
;
; :KEYWORD PARAMETERS:
;    ACCEPTABLE_DIFFERENCE
;       An optional keyword that will set the value of the maximum acceptable 
;       difference between value1 and value2 for equality to be assumed.
;
; :RESULT:
;    A scalar value of 1 (TRUE) if the provided values can be considered equal
;    or 0 (FALSE) otherwise.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    February, 2013    Original code
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

FUNCTION IS_EQUAL, value1, value2, ACCEPTABLE_DIFFERENCE=acceptableDifference

   IF KEYWORD_SET( acceptableDifference ) THEN BEGIN
      epsilon = acceptableDifference
   ENDIF ELSE BEGIN
      epsilon = value1 / 1E6
   ENDELSE

   IF ( ABS( value1 - value2 ) LT epsilon ) THEN BEGIN
      RETURN, 1
   ENDIF ELSE BEGIN
      RETURN, 0
   ENDELSE

END