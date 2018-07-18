;+
; :NAME:
;    DCT2D
;
; :PURPOSE:
;    This function computes the two-dimensional discrete cosine transform
;    (or its inverse) of the provide two-dimensional data array.
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    Result = DCT2D( image, INVERSE=inverse )
;
; :INPUTS:
;    image
;       A two-dimensional array containing the data for which the two-
;       dimensional DCT is to be computed.
;
; :KEYWORD PARAMETRS:
;    INVERSE
;       A keyword parameter that if set will cause the provided image data
;       to be treated as DCT coefficients and cause the function to 
;       perform an inverse transformation to provide the spatial domain 
;       representation of the data.
;
; :RETURN VALUE:
;    Result is a double precision, floating point array containing either the
;    DCT coefficients or the inverse for the provided data.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    DCT1D
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

FUNCTION DCT2D, image, INVERSE=inverse

   imageSize = SIZE( image, /DIMENSIONS )
   M = imageSize[0]
   N = imageSize[1]

   transformedImage = DBLARR( M, N )

   WIDGET_CONTROL, /HOURGLASS

   FOR x = 0, M-1 DO BEGIN
      IF KEYWORD_SET( INVERSE ) THEN BEGIN
         transformedImage[x,*] = DCT1D( image[x,*], /INVERSE )
      ENDIF ELSE BEGIN
         transformedImage[x,*] = DCT1D( image[x,*] )
      ENDELSE
   ENDFOR

   FOR y = 0, N-1 DO BEGIN
      IF KEYWORD_SET( INVERSE ) THEN BEGIN
         transformedImage[*,y] = DCT1D( transformedImage[*,y], /INVERSE )
      ENDIF ELSE BEGIN
         transformedImage[*,y] = DCT1D( transformedImage[*,y] )
      ENDELSE
   ENDFOR
 
   RETURN, transformedImage

END