;+
; :NAME:
;    DCT_BLOCK
;
; :PURPOSE:
;    This function produces a block DCT image (or its inverse) from the
;    provided input data.
;
; :CATEGORY:
;    Image Processing.
;
; :CALLING SEQUENCE:
;    Result = DCT_BLOCK( image, blockDimension, INVERSE=inverse )
;
; :INPUTS:
;    image
;       A two-dimensional array containing the data to be block DCT
;       transformed.
;    blockDimension
;       A scalar describing the dimension of the square DCT blocks that are
;       to be formed in the image.
;
; :KEYWORD PARAMETRS:
;    INVERSE
;       A keyword parameter that if set will cause the provided image data
;       to be treated as block DCT coefficients and cause the function to 
;       perform an inverse transformation to provide the spatial domain 
;       representation of the data.
;
; :RETURN VALUE:
;    Result is a double precision, floating point array containing either the
;    block DCT or its inverse for the provided data.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    DCT2D
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

FUNCTION DCT_BLOCK, image, blockDimension, INVERSE=inverse

   dimensions = SIZE( image, /DIMENSIONS )

   dctImage = DBLARR( dimensions[0], dimensions[1] )

   blocksHorizontal = dimensions[0] / blockDimension
   blocksVertical = dimensions[1] / blockDimension

   FOR h = 0, blocksHorizontal-1 DO BEGIN
      FOR v = 0, blocksVertical-1 DO BEGIN
         startColumn = h * blockDimension
         endColumn = startColumn+blockDimension-1
         startRow = v * blockDimension
         endRow = startRow+blockDimension-1
         imageBlock = image[startColumn:endColumn,startRow:endRow]
         dctImage[startColumn:endColumn,startRow:endRow] = DCT2D( imageBlock, INVERSE=inverse )
      ENDFOR
   ENDFOR

   RETURN, dctImage

END