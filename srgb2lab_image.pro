;+
; :NAME:
;    SRGB2LAB_IMAGE
;
; :PURPOSE:
;    This function converts a provided sRGB color image to a CIE L*a*b*
;    color space representation image assuming Illuminant D65.  A
;    non-linearity adjustment for the monitor response is applied to the
;    provided color data.
;
; :CATEGORY:
;    Color Science.
;
; :CALLING SEQUENCE:
;    Result = SRGB2LAB_IMAGE( image )
;
; :INPUTS:
;    image
;       A 3xMxN array containing the 8-bit/color/pixel color image to be
;       transformed.
;
; :KEYWORD PARAMETRS:
;    None
;
; :RETURN VALUE:
;    Result is 3xMxN array containing the CIE L*a*b* color space representation
;    of the provided RGB color image (assuming Illuminant D65).
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRED:
;    SRGB2XYZ
;    XYZ2LAB
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    February 2010     Original code
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

FUNCTION SRGB2LAB_IMAGE, image

   numberDimensions = SIZE( image, /N_DIMENSIONS )
   imageDimension = SIZE( image, /DIMENSIONS )
   IF ( (numberDimensions NE 3) OR (imageDimension[0] NE 3 ) ) THEN BEGIN
      MESSAGE, "Supplied image must be 3-band color ([3,*,*])"
   ENDIF

   imageType = SIZE( image, /TYPE )
   IF ( imageType NE 1 ) THEN BEGIN
      MESSAGE, "Supplied image must be of BYTE data type"
   ENDIF

   scaledImage = image / 255D

   labImage = XYZ2LAB( SRGB2XYZ( REFORM( scaledImage, 3, imageDimension[1]*imageDimension[2] ) ) )

   RETURN, REFORM( labImage, 3, imageDimension[1], imageDimension[2] )

END