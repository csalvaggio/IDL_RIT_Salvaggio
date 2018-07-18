;+
; :NAME:
;    LAB2XYZ
;
; :PURPOSE:
;    This function returns the converted tristimulus values for a
;    provided CIE L*a*b* value assuming a particular illuminant,
;    namely
;
;       X = xr * Xr
;       Y = yr * Yr
;       Z = zr * Zr
;
;   where
;       xr = fx^3                       for fx^3 > epsilon
;       xr = (116 * fx - 16) / kappa    for fx^3 <= epsilon
;       yr = ((L + 16) / 116 ) / kappa  for L > kappa * epsilon
;       yr = L / kappa                  for L <= kappa * epsilon
;       zr = fz^3                       for fz^3 > epsilon
;       zr = (116 * fz - 16) / kappa    for fz^3 <= epsilon
;
;       fx = (a / 500) + fy
;       fy = (L + 16) / 116
;       fz = fy - (b / 200)
;
;       epsilon = 0.008856
;       kappa = 903.3
;
;   and
;       Xr, Yr, Zr are the white reference for the specified illuminant
;
; :CATEGORY:
;    Color Science.
;
; :CALLING SEQUENCE:
;    Result = LAB2XYZ( Lab, ILLUMINANT=illuminant )
;
; :INPUTS:
;    Lab
;       A 3-element vector or 3xn array of CIE L*a*b* coordinates
;
; :KEYWORD PARAMETERS:
;    ILLUMINANT
;       An optional keyword to describe the illuminant that should
;       be used for the CIE L*a*b* to XYZ conversion
;          0 - D65 (Daylight) [DEFAULT]
;          1 - Illuminant A (Tungsten)
;          2 - Illuminant F2 (Cool White Fluorescent)
;          3 - Illuminant F11 (Narrow Band Fluorescent)
;          4 - Illuminant F7 (Daylight Fluorescent)
;          5 - Illuminant F8 (Daylight Fluorescent)
;
; :RETURN VALUE:
;    Result is either a 3-element vector or a 3xn array that contains 
;    the computed tristimulus values
;
; :SIDE EFFECTS:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    February, 2010    Original code
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

FUNCTION LAB2XYZ, Lab, ILLUMINANT=illuminant

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

   fy = ( Lab[0,*] + 16D ) / 116D
   fx = ( Lab[1,*] / 500D ) + fy
   fz = fy - ( Lab[2,*] / 200D )

   epsilon = 0.008856D
   kappa = 903.3D

   numPoints = N_ELEMENTS( Lab[0,*] )
   xr = DBLARR( numPoints )
   yr = DBLARR( numPoints )
   zr = DBLARR( numPoints )

   index = WHERE( (fx^3) GT epsilon, count )
   IF ( count GT 0 ) THEN xr[index] = fx[index]^3D
   index = WHERE( (fx^3) LE epsilon, count )
   IF ( count GT 0 ) THEN xr[index] = ( 116D * fx[index] - 16D ) / kappa

   index = WHERE( Lab[0,*] GT (kappa*epsilon), count )
   IF ( count GT 0 ) THEN yr[index] = ( ( Lab[0,index] + 16D ) / 116D )^3D
   index = WHERE( Lab[0,*] LE (kappa*epsilon), count )
   IF ( count GT 0 ) THEN yr[index] = Lab[0,index] / kappa

   index = WHERE( (fz^3) GT epsilon, count )
   IF ( count GT 0 ) THEN zr[index] = fz[index]^3D
   index = WHERE( (fz^3) LE epsilon, count )
   IF ( count GT 0 ) THEN zr[index] = ( 116D * fz[index] - 16D ) / kappa

   X = xr * xyzWhite[0]   
   Y = yr * xyzWhite[1]   
   Z = zr * xyzWhite[2]   

   XYZ = TRANSPOSE( REFORM( [ X, Y, Z ], numPoints, 3 ) )

   RETURN, XYZ

END