;+
; :NAME:
;    PLOT_SPHERICAL
;
; :PURPOSE:
;    This function will create a three-dimensional quadilateral-faceted
;    plot of spherical coordinate data.  The data provided must
;    represent magnitudes defined at a regularly-spaced set of declination
;    and azimuth angles.
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    result = PLOT_SPHERICAL( declination, $
;                             azimuth, $
;                             magnitude, $
;                             DEGREES=inDegrees, $
;                             NO_AXES=noAxes )
;
; :INPUTS:
;    declination
;       An array of declination angles at which the magnitudes are defined.
;    azimuth
;       An array of azimuth angles at which the magntiudes are defined.
;    magnitude
;       An array of data values at the specified azimuth and zenith angles.
;
;    NOTE: The order of the data contained in the arrays described above must must
;          be consistent (a magnitude at a declination and a azimuth must all occupy
;          the same array position).  The relative ordering of the data can be
;          arbitrary as the function will sort the data for proper plotting.
;
; :KEYWORD PARAMETERS:
;    DEGREES
;       Set this keyword to interpret the provided declination and azimuth angles
;       in degrees.  If this keyword is omitted, the angles are assumed to be 
;       specified in radians.
;    NO_AXES
;       Set this keyword to omit drawing axes for the produced plot.
;
; :ERROR CHECKING:
;    None.
;
; :SIDE EFFECTS:
;    None.
;
; :REQUIRES:
;    BSORT
;       This routine can be obtained at http://idlastro.gsfc.nasa.gov/ftp/pro/misc/bsort.pro
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    January, 2012     Original code
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

FUNCTION PLOT_SPHERICAL, declination, $
                         azimuth, $
                         magnitude, $
                         DEGREES=inDegrees, $
                         NO_AXES=noAxes

;+
; If the provided angles are provided in degrees, convert to radians
;-
   IF KEYWORD_SET( inDegrees ) THEN BEGIN
      theta = declination * !DTOR
      phi = azimuth * !DTOR
   ENDIF
   value = magnitude

;+
; Sort the input data to group common declination angles together in the list
;-
   sortIndex = REVERSE( BSORT( theta ) )
   theta = theta[ sortIndex ]
   phi = phi[ sortIndex ]
   value = magnitude[ sortIndex ]

;+
; For each group of declination angles, sort the list by azimuth angle
;-
   numberTheta = N_ELEMENTS( UNIQ( theta[ BSORT( theta ) ] ) )
   numberPhi = N_ELEMENTS( UNIQ( phi[ BSORT( phi ) ] ) )
   FOR tIndex = 0, numberTheta-1 DO BEGIN
      sortIndex = BSORT( phi[ tIndex*numberPhi:tindex*numberPhi+numberPhi-1 ] )
      tmp = theta[ tIndex*numberPhi:tindex*numberPhi+numberPhi-1 ]
      tmp = tmp[ sortIndex ]
      theta[ tIndex*numberPhi:tindex*numberPhi+numberPhi-1 ] = tmp
      tmp = phi[ tIndex*numberPhi:tindex*numberPhi+numberPhi-1 ]
      tmp = tmp[ sortIndex ]
      phi[ tIndex*numberPhi:tindex*numberPhi+numberPhi-1 ] = tmp
      tmp = value[ tIndex*numberPhi:tindex*numberPhi+numberPhi-1 ]
      tmp = tmp[ sortIndex ]
      value[ tIndex*numberPhi:tindex*numberPhi+numberPhi-1 ] = tmp
   ENDFOR

;+
; Convert the spherical data to Cartesian coordinates
;-
   x = !NULL
   y = !NULL
   z = !NULL
   mIndex = 0
   FOR tIndex = 0, numberTheta-1 DO BEGIN
      FOR pIndex = 0, numberPhi-1 DO BEGIN
         x = [ x, value[ mIndex ] * SIN( theta[ mIndex ] ) * COS( phi[ mIndex ] ) ]
         y = [ y, value[ mIndex ] * SIN( theta[ mIndex ] ) * SIN( phi[ mIndex ] ) ]
         z = [ z, value[ mIndex ] * COS( theta[ mIndex ] ) ]
         mIndex = mIndex + 1
      ENDFOR
   ENDFOR

;+
; Form the list of quadrilaterals to plot
;-
   quadX = !NULL
   quadY = !NULL
   quadZ = !NULL
   FOR tIndex = 0, numberTheta-2 DO BEGIN
      FOR pIndex = 0, numberPhi-1 DO BEGIN
         IF ( pIndex EQ ( numberPhi - 1 ) ) THEN BEGIN
            vertex1 = ( tIndex * numberPhi ) + pIndex
            vertex2 = ( tIndex * numberPhi ) + ( pIndex + 1 ) - numberPhi
            vertex3 = ( tIndex * numberPhi ) + ( pIndex + 1 ) + numberPhi - numberPhi
            vertex4 = ( tIndex * numberPhi ) + pIndex + numberPhi
         ENDIF ELSE BEGIN
            vertex1 = ( tIndex * numberPhi ) + pIndex
            vertex2 = ( tIndex * numberPhi ) + ( pIndex + 1 )
            vertex3 = ( tIndex * numberPhi ) + ( pIndex + 1 ) + numberPhi
            vertex4 = ( tIndex * numberPhi ) + pIndex + numberPhi
         ENDELSE
         quadX = [ [ quadX ], [ x[ vertex1 ], x[ vertex2 ], x[ vertex3 ], x[ vertex4 ], x[ vertex1 ] ] ]
         quadY = [ [ quadY ], [ y[ vertex1 ], y[ vertex2 ], y[ vertex3 ], y[ vertex4 ], y[ vertex1 ] ] ]
         quadZ = [ [ quadZ ], [ z[ vertex1 ], z[ vertex2 ], z[ vertex3 ], z[ vertex4 ], z[ vertex1 ] ] ]
      ENDFOR
   ENDFOR

;+
; Plot the individual quadrilaterals using IDL's built-in PLOT3D function
;-
   numberQuads = N_ELEMENTS( quadX ) / 5

   plt = PLOT3D( [ -1, 1 ], $
                 [ -1, 1 ], $
                 [ -1, 1 ], $
                 /NODATA,$
                 AXIS_STYLE=0, $
                 XRANGE=[ 1.5*MIN( x ), 1.5*MAX( x ) ], $
                 YRANGE=[ 1.5*MIN( y ), 1.5*MAX( y ) ], $
                 ZRANGE=[ 1.5*MIN( z ), 1.5*MAX( z ) ] )

   FOR quad = 0, numberQuads-1 DO BEGIN
      plt = PLOT3D( quadX[*,quad], quadY[*,quad], quadZ[*,quad], /OVERPLOT )
   ENDFOR

;+
; Show axes unless explicitly told not to
;-
   IF NOT KEYWORD_SET( noAxes ) THEN BEGIN
      xAxis = AXIS( 'X', LOCATION=[ 0, 0, 0 ], TITLE='X', MINOR=0 )
      yAxis = AXIS( 'Y', LOCATION=[ 0, 0, 0 ], TITLE='Y', MINOR=0 )
      zAxis = AXIS( 'Z', LOCATION=[ 0, 0, 0 ], TITLE='Z', MINOR=0 )
   ENDIF

;+
; Return the plot object to the calling routine
;-
   RETURN, plt

END