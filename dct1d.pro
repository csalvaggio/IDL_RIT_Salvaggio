;+
; :NAME:
;    DCT1D
;
; :PURPOSE:
;    This function computes the one-dimensional discrete cosine transform
;    (or its inverse) of the provide vector.
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    Result = DCT1D( functionValues, INVERSE=inverse )
;
; :INPUTS:
;    functionValues
;       A vector containing the data for which the one-dimensional DCT is
;       to be computed.
;
; :KEYWORD PARAMETRS:
;    INVERSE
;       A keyword parameter that if set will cause the provided  data
;       to be treated as DCT coefficients and cause the function to 
;       perform an inverse transformation to provide the spatial domain 
;       representation of the data.
;
; :RETURN VALUE:
;    Result is a double precision, floating point vector containing either the
;    DCT coefficients or the inverse for the provided data.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    February, 2009    Original code
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

FUNCTION DCT1D, functionValues, INVERSE=inverse

   M = N_ELEMENTS( functionValues )

   transformedValues = DBLARR( M )

   IF ( KEYWORD_SET( INVERSE ) ) THEN BEGIN

      originalValues = functionValues

      functionValues[0] = functionValues[0] * SQRT( 1.0 / M )
      functionValues[1:M-1] = functionValues[1:M-1] * SQRT( 2.0 / M )

      FOR u = 0, M-1 DO BEGIN
         FOR x = 0, M-1 DO BEGIN
            angle = ( 2.0d * u + 1) * x * !DPI / ( 2.0d * M )
            transformedValues[u] = transformedValues[u] + functionValues[x] * COS( angle )
         ENDFOR
      ENDFOR

      functionValues = originalValues

   ENDIF ELSE BEGIN

      FOR u = 0, M-1 DO BEGIN
         FOR x = 0, M-1 DO BEGIN
            angle = ( 2.0d * x + 1) * u * !DPI / ( 2.0d * M )
            transformedValues[u] = transformedValues[u] + functionValues[x] * COS( angle )
         ENDFOR
      ENDFOR

      transformedValues[0] = transformedValues[0] * SQRT( 1.0 / M )
      transformedValues[1:M-1] = transformedValues[1:M-1] * SQRT( 2.0 / M )

   ENDELSE

   RETURN, transformedValues

END