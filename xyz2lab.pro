;+
; :NAME:
;    XYZ2LAB
;
; :PURPOSE:
;    This function returns the converted CIE L*a*b* values for a 
;    provided tristimulus value assuming a particular illuminant,
;    namely
;
;       L* = 116 * f( Y / Yw ) - 16
;       a* = 500 * [f( X / Xw ) - f( Y / Yn )]
;       b* = 200 * [f( Y / Yw ) - f( Z / Zn )]
;
;    where
;       f(x) = x^(1/3)               for x > 0.008856
;       f(x) = 7.787 * x + 16/116    for x <= 0.008856
;
; :CATEGORY:
;    Color Science.
;
; :CALLING SEQUENCE:
;    Result = XYZ2LAB( xyz, ILLUMINANT=illuminant )
;
; :INPUTS:
;    xyz
;       A 3-element vector or 3xn array of tristimulus values
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
;
; :RETURN VALUE:
;    Result is either a 3-element vector or a 3xn array that contains 
;    the computed CIE L*a*b* coordinates
;
; :SIDE EFFECTS:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    January, 2008     Original code
;    April, 2012       Corrected white reference scaling error
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

FUNCTION XYZ2LAB, xyz, ILLUMINANT=illuminant

   xyzWhite = [95.0430D, 100.0000D, 108.8801D]

   IF KEYWORD_SET( illuminant ) THEN BEGIN
      CASE illuminant OF
         0:    xyzWhite = [ 95.0430D, 100.0000D, 108.8801D]
         1:    xyzWhite = [109.8490D, 100.0000D,  35.5825D]
         2:    xyzWhite = [ 99.1858D, 100.0000D,  67.3938D]
         3:    xyzWhite = [100.9610D, 100.0000D,  64.3506D]
         4:    xyzWhite = [ 95.0416D, 100.0000D, 108.7489D]
         5:    xyzWhite = [ 96.4274D, 100.0000D,  82.4211D]
         ELSE: xyzWhite = [ 95.0430D, 100.0000D, 108.8801D]
      ENDCASE
   ENDIF
   xyzWhite = xyzWhite / 100

   xyzIn = DOUBLE( xyz )
   
   ratio = [ xyzIn[0,*] / xyzWhite[0], $
             xyzIn[1,*] / xyzWhite[1], $
             xyzIn[2,*] / xyzWhite[2] ]

   index = WHERE( ratio GT 0.008856D, count )
   IF ( count GT 0 ) THEN ratio[index] = ratio[index]^(1D/3D)
   index = WHERE( ratio LE 0.008856D, count )
   IF ( count GT 0 ) THEN ratio[index] = 7.787D*ratio[index] + 16D/116D

   L = 116D*ratio[1,*] - 16D
   a = 500D*( ratio[0,*] - ratio[1,*] )
   b = 200D*( ratio[1,*] - ratio[2,*] )

   Lab = [ L, a, b ]

   RETURN, Lab

END