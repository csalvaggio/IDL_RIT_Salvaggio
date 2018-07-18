;+
; :NAME:
;    DELTAE
;
; :PURPOSE:
;    This function returns the computed color difference (deltaE)
;    between two (2) provided RGB color triplets.  The triplets are
;    converted to sRGB (0-1), converted to tristimulus values (XYZ),
;    converted to CIE L*a*b* where the color difference between them
;    is computed using a simple Euclidean distance measure.
;
; :CATEGORY:
;    Color Science.
;
; :CALLING SEQUENCE:
;    Result = DELTAE( color1, color2, ILLUMINANT=illuminant,
;                                     DL=dL,
;                                     DA=dA,
;                                     DB=dB,
;                                     AVERAGE=average )
;
; :INPUTS:
;    color1
;       A 3-element vector or 3xn array of color triplet values
;       with each color value in the range [0,255]
;    color2
;       A 3-element vector or 3xn array of color triplet values
;       with each color value in the range [0,255]
;
; :KEYWORD PARAMETERS:
;    ILLUMINANT
;       An optional keyword to describe the illuminant that should
;       be used for the XYZ to CIE L*a*b* conversion
;          0 - D65 (Daylight) [DEFAULT]
;          1 - Illuminant A (Tungsten)
;          2 - Illuminant F2 (Cool White Fluorescent)
;          3 - Illuminant F11 (Narrow Band Fluorescent)
;          4 - Illuminant F7 (Daylight Fluorescent)
;          5 - Illuminant F8 (Daylight Fluorescent)
;    DL
;       An optional keyword to return a scalar or vector of deltaL value(s)
;    DA
;       An optional keyword to return a scalar or vector of deltaA value(s)
;    DB
;       An optional keyword to return a scalar or vector of deltaB value(s)
;    AVERAGE
;       An optional keyword that will, upon return, contain
;       a 4-element vector composed of deltaE, deltaL, deltaA, and
;       deltaB
;
; :RETURN VALUE:
;    Result is either a scalar (if the provided color values
;    were single triplets) or an n-element vector (if a series of
;    triplets were provided for each color) that contains the color
;    difference values.
;
; :ERROR CHECKING:
;    RETURN, -1
;       if the provided color data is not of compatible dimensions
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    SRGB2XYZ
;    XYZ2LAB
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    January, 2008     Original code
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

FUNCTION DELTAE, color1, color2, ILLUMINANT=illuminant, $
                                 DL=dL, $
                                 DA=dA, $
                                 DB=dB, $
                                 AVERAGE=average

   IF NOT KEYWORD_SET( illuminant ) THEN illuminant = 0

   IF ( N_ELEMENTS( color1 ) NE N_ELEMENTS( color2 ) ) THEN BEGIN
      PRINT, " "
      PRINT, "%DELTAE: provided data elements are not the same size"
      PRINT, " "
      RETURN, -1
   ENDIF

   numberDimensions = SIZE( color1, /N_DIMENSIONS )

   IF ( numberDimensions EQ 1 ) THEN BEGIN
      sRGB1 = REFORM( color1, 3, 1 ) / 255D
      sRGB2 = REFORM( color2, 3, 1 ) / 255D
   ENDIF ELSE BEGIN
      sRGB1 = REFORM( color1, 3, N_ELEMENTS( color1 )/3 ) / 255D
      sRGB2 = REFORM( color2, 3, N_ELEMENTS( color1 )/3 ) / 255D
   ENDELSE

   XYZ1 = SRGB2XYZ( sRGB1 )
   XYZ2 = SRGB2XYZ( sRGB2 )

   Lab1 = XYZ2LAB( XYZ1, ILLUMINANT=illuminant )
   Lab2 = XYZ2LAB( XYZ2, ILLUMINANT=illuminant )

   dE = SQRT( TOTAL( (Lab1 - Lab2)^2, 1 ) )

   difference = Lab1 - Lab2
   IF ( numberDimensions EQ 1 ) THEN BEGIN
      dL = difference[0]
      dA = difference[1]
      dB = difference[2]
   ENDIF ELSE BEGIN
      dL = REFORM( difference[0,*] )
      dA = REFORM( difference[1,*] )
      dB = REFORM( difference[2,*] )
   ENDELSE

   average = MEAN( dE )
   average = [ average, MEAN( dL ) ]
   average = [ average, MEAN( dA ) ]
   average = [ average, MEAN( dB ) ]

   RETURN, dE

END