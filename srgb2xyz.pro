;+
; :NAME:
;    SRGB2XYZ
;
; :PURPOSE:
;    This function converts a provided sRGB color triplet to tristimulus
;    values.  The sRGB values are assumed to be in the range [0,1].  A
;    non-linearity adjustment for the monitor response is applied to the
;    provided color data.
;       (Source: http://www.brucelindbloom.com)
;
;       For [R,G,B] > 0.03928
;          R-hat = [ (R + 0.055) / 1.055 ]^2.4
;          G-hat = [ (G + 0.055) / 1.055 ]^2.4
;          B-hat = [ (B + 0.055) / 1.055 ]^2.4
;
;       For [R,G,B] <= 0.04045
;          R-hat = R / 12.92
;          G-hat = G / 12.92
;          B-hat = B / 12.92
;
;       | X |   | 0.4124564 0.3575761 0.1804375 |   | R-hat |
;       | Y | = | 0.2126729 0.7151522 0.0721750 | * | G-hat |
;       | Z |   | 0.0193339 0.1191920 0.9503041 |   | B-hat |
;
; :CATEGORY:
;    Color Science.
;
; :CALLING SEQUENCE:
;    Result = SRGB2XYZ( rgb )
;
; :INPUTS:
;    rgb
;       A 3-element vector or 3xn array of sRGB color triplet values
;       with each color value in the range [0,1]
;
; :KEYWORD PARAMETRS:
;    None
;
; :RETURN VALUE:
;    Result is either a 3-element vector or a 3xn array that contains 
;    the computed tristimlus values
;
; :SIDE EFFECTS:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    January, 2008     Original code
;    February 2010     Modified with more precise coefficients
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

FUNCTION SRGB2XYZ, rgb

   M = [ [ 0.4124564D, 0.3575761D, 0.1804375D ], $
         [ 0.2126729D, 0.7151522D, 0.0721750D ], $
         [ 0.0193339D, 0.1191920D, 0.9503041D ] ]

   rgbIn = DOUBLE( rgb )

   xyz = ( (rgbIn + 0.055D) / 1.055D )^2.4D
   
   index = WHERE( rgbIn LE 0.04045D, count )
   IF ( count GT 0 ) THEN xyz[index] = rgbIn[index] / 12.92D

   xyz = TRANSPOSE( M ) # xyz

   RETURN, xyz

END