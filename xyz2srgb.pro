;+
; :NAME:
;    XYZ2SRGB
;
; :PURPOSE:
;    This function converts a provided set of tristimulus values to
;    sRGB.  The tristimulus values are assumed to be in the range
;    [0,1] and computed with illuminant D65.  A non-linearity adjustment
;    for the monitor response is applied (gamma = 2.2).
;       (Source: http://www.brucelindbloom.com)
;
;       | R-linear |   |  3.2404542 -1.5371385 -0.4985314 |   | X |
;       | G-linear | = | -0.9692660  1.8760108  0.0415560 | * | Y |
;       | B-linear |   |  0.0556434 -0.2040259  1.0572252 |   | Z |
;
;    For [R-linear,G-linear,B-linear] <= 0.0031308
;       sRGB = 12.92 * sRGB
;
;    For [R-linear,G-linear,B-linear] > 0.0031308
;       sRGB = ( 1 + a ) * sRGB^( 1 / 2.4 ) - a
;
; :CATEGORY:
;    Color Science.
;
; :CALLING SEQUENCE:
;    Result = XYZ2SRGB( xyz )
;
; :INPUTS:
;    xyz
;       A 3-element vector or 3xn array of XYZ tristimulus values
;       (each in the range [0,1]).  The tristimulus values are
;       assumed to have been computed using illuminant D65.
;
; :KEYWORD PARAMETERS:
;    None
;
; :RETURN VALUE:
;    Result is either a 3-element vector or a 3xn array that contains 
;    the computed sRGB color values
;
; :SIDE EFFECTS:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    January, 2008     Original code
;    February 2010     Modified with more precise coefficients
;    July 2010         Corrected error with sRGB values and threshhold
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

FUNCTION XYZ2SRGB, xyz

   M = [ [  3.2404542D, -1.5371385D, -0.4985314D ], $
         [ -0.9692660D,  1.8760108D,  0.0415560D ], $
         [  0.0556434D, -0.2040259D,  1.0572252D ] ]

   xyzIn = DOUBLE( xyz )
   
   sRGB = TRANSPOSE( M ) # xyzIn

   leIndex = WHERE( sRGB LE 0.0031308D, leCount, COMPLEMENT=gtIndex, NCOMPLEMENT=gtCount )
   IF ( leCount GT 0 ) THEN sRGB[leIndex] = sRGB[leIndex] * 12.92D
   IF ( gtCount GT 0 ) THEN sRGB[gtIndex] = 1.055D * sRGB[gtIndex]^( 1D / 2.4D ) - 0.055D

   index = WHERE( sRGB LT 0, count )
   IF ( count GT 0 ) THEN sRGB[index] = 0D

   index = WHERE( sRGB GT 1, count )
   IF ( count GT 0 ) THEN sRGB[index] = 1D

   RETURN, sRGB

END