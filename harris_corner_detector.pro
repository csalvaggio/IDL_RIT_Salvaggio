;+
; :NAME:
;    HARRIS_CORNER_DETECTOR
;
; :PURPOSE:
;    This function will find corners in an image.  Corners are defined
;    in this context as localized areas within the image were small
;    shifts in the image in any direction produce a significant
;    change in the composition of the neighborhood.
;
; :CATEGORY:
;    Image Processing.
;
; :CALLING SEQUENCE:
;    Result = $
;       HARRIS_CORNER_DETECTOR( image, $
;                               sigma, $
;                               metric, $
;                               OVERLAY=overlay, $
;                               DEBUG=debug )
;
; :RETURN VALUE:
;    This function will return a 2xN array of coordinates indicating
;    corner locations found throughout the provided image.
;
; :INPUTS:
;    image
;       An array containing the data within which corners are to be
;       located.
;    sigma
;       A scalar defining the standard deviation of the gaussian
;       blurring kernel to be used in the smoothing/averaging operation
;       used to reduce noise in the intermediate gradient images.  This
;       quantity is defined in units of pixels. [DEFAULT=1]
;    metric
;       A flag indicating which response surface metric is to be
;       used.  The metrics are defined as
;          1 - lambda2
;          2 - (lambda1 * lambda2) / (lambda1 + lambda2)
;          3 - (lambda1 * lambda2) - (lambda1 + lambda2)
;          4 - (lambda1 * lambda2) - k*(lambda1 + lambda2)^2 [DEFAULT]
;
; :KEYWORD PARAMETERS:
;    OVERLAY=overlay
;       Indicates a copy of the original image provided is to be returned
;       in a 3-band color image format with red pixels indicating where
;       the corners found were located.
;
;    DEBUG=debug
;       Indicates that debug images/results should be produced at run time
;       if set.
;
; :RETURN VALUE:
;    This function returns a 2xn array containing the image coordinates of the
;    n corner points identified by the FAST detector.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    FIND_CLUSTER_CENTERS
;
; REFERENCE:
;    Harris, C. and M.J. Stephens, A combined corner and edge detector, 
;    Alvey Vision Converence, 1988, pp. 147-152 
;
; :SIDE EFFECTS:
;     None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    September, 2008   Original Code
;    April, 2012       Added debug keyword todisplay intermediate results
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

FUNCTION HARRIS_CORNER_DETECTOR, image, sigma, metric, OVERLAY=overlay, DEBUG=debug

;+
; Parse the debug flag for demonstration image production
;-
   IF NOT KEYWORD_SET( debug ) THEN debug = 0
   IF KEYWORD_SET( debug ) THEN WHILE ( !D.WINDOW GT -1 ) DO WDELETE, !D.WINDOW

;+
; Form a local double-precision floating-point version of the
; provided image
;-
   i = DOUBLE( image )

;+
; Determine the dimensions and number of total pixels in the
; supplied image
;-
   dimensions = SIZE( i, /DIMENSIONS )
   numberSamples = dimensions[0]
   numberLines = dimensions[1]
   numberPixels = numberSamples * numberLines

;+
; Perform a vertical and horizontal first derivative on the image,
; respectively, to locate edge pixels
;-
   x = CONVOL( i, [ -1, 0, 1 ], /EDGE_WRAP )
   y = CONVOL( i, TRANSPOSE( [ -1, 0, 1 ] ), /EDGE_WRAP )

   IF KEYWORD_SET( debug ) THEN BEGIN
      WINDOW, !D.WINDOW+1, XSIZE=numberSamples, YSIZE=numberLines, TITLE='Horizontal Partial Derivative'
      TVSCL, x
      WINDOW, !D.WINDOW+1, XSIZE=numberSamples, YSIZE=numberLines, TITLE='Vertical Partial Derivative'
      TVSCL, y
   ENDIF

;+
; Smooth the two directional-gradient and cross-product images 
; using a gaussiam smoothing kernel to reduce the responses to 
; noise in the original data set
;-
   filterSize = sigma*6
   gaussian = SHIFT( EXP( -0.5 * ( DIST( filterSize ) / sigma )^2 ), filterSize/2, filterSize/2 )
   a = CONVOL( x^2, gaussian, /EDGE_WRAP )
   b = CONVOL( y^2, gaussian, /EDGE_WRAP )
   c = CONVOL( x*y, gaussian, /EDGE_WRAP )

   IF KEYWORD_SET( debug ) THEN BEGIN
      WINDOW, !D.WINDOW+1, XSIZE=numberSamples, YSIZE=numberLines, TITLE='Smoothed Horizontal Gradient (A)'
      TVSCL, a
      WINDOW, !D.WINDOW+1, XSIZE=numberSamples, YSIZE=numberLines, TITLE='Smoothed Vertical Gradient (B)'
      TVSCL, b
      WINDOW, !D.WINDOW+1, XSIZE=numberSamples, YSIZE=numberLines, TITLE='Smoothed Cross Gradient (C)'
      TVSCL, c
   ENDIF

;+
; Form a matrix of gradient response values for each pixel in the
; image of the form
;    A C
;    C B
;-
   m = DBLARR( 2, 2, numberPixels )
   m[0,0,*] = REFORM( a )
   m[1,0,*] = REFORM( c )
   m[0,1,*] = REFORM( c )
   m[1,1,*] = REFORM( b )

;+
; Compute the eigenvalues for the matrix M formed for each pixel
; above
;-
   eigenvalues = DBLARR( 2, numberPixels )
   FOR pixel=0L, numberPixels-1 DO BEGIN
      eigenvalues[*,pixel] = EIGENQL( m[*,*,pixel] )
   ENDFOR

   IF KEYWORD_SET( debug ) THEN BEGIN
      WINDOW, !D.WINDOW+1, XSIZE=numberSamples, YSIZE=numberLines, TITLE='Eigenvalue 1'
      TVSCL, REFORM( eigenvalues[0,*], numberSamples, numberLines )
      WINDOW, !D.WINDOW+1, XSIZE=numberSamples, YSIZE=numberLines, TITLE='Eigenvalue 2'
      TVSCL, REFORM( eigenvalues[1,*], numberSamples, numberLines )
   ENDIF

;+
; Compute a response surface image based upon the eigenvalues
; determined for each of the pixels according to the user-
; specified metric
;-
   k = 0.01
   response = DBLARR( numberPixels )
   FOR pixel=0L, numberPixels-1 DO BEGIN
      CASE metric OF
         1: response[pixel] = eigenvalues[1,pixel]

         2: response[pixel] = eigenvalues[0,pixel] * eigenvalues[1,pixel] / $
                              ( eigenvalues[0,pixel] + eigenvalues[1,pixel] )

         3: response[pixel] = eigenvalues[0,pixel] * eigenvalues[1,pixel] - $
                              ( eigenvalues[0,pixel] + eigenvalues[1,pixel] )

         4: response[pixel] = eigenvalues[0,pixel] * eigenvalues[1,pixel] - $
                              k * ( eigenvalues[0,pixel] + eigenvalues[1,pixel] )^2
      ENDCASE
   ENDFOR
   response = REFORM( response, numberSamples, numberLines )

   IF KEYWORD_SET( debug ) THEN BEGIN
      WINDOW, !D.WINDOW+1, XSIZE=numberSamples, YSIZE=numberLines, TITLE='Response'
      TVSCL, response
   ENDIF

;+
; Compute an appropriate threshold value to form a binary cluster 
; image to be used for corner determination
   ;-
   threshold = 0.25
   threshold = MIN( response ) + threshold * ( MAX( response ) - MIN( response ) )
   
;+
; Find the corners in the thresholded version of the response image
;-
   corners = FIND_CLUSTER_CENTERS( response, threshold )

;+
; Create a color image from the original grey-scale version containing
; red pixels at the corner locations
;-
   IF ( N_ELEMENTS( corners ) GT 0 ) THEN BEGIN
      markerImage = image
      overlay = BYTARR( 3, numberSamples, numberLines )
      FOR corner=0, N_ELEMENTS( corners )/2-1 DO BEGIN
         x = corners[0,corner]
         y = corners[1,corner]
         markerImage[x-1,y-1] = 255B
         markerImage[x,y-1] = 255B
         markerImage[x+1,y-1] = 255B
         markerImage[x-1,y] = 255B
         markerImage[x,y] = 255B
         markerImage[x+1,y] = 255B
         markerImage[x-1,y+1] = 255B
         markerImage[x,y+1] = 255B
         markerImage[x+1,y+1] = 255B
      ENDFOR
      overlay[0,*,*] = markerImage
      overlay[1,*,*] = image
      overlay[2,*,*] = image
   ENDIF

;+
; Return the 2xN array containing the corner coordinates to the calling
; module
;-
   RETURN, corners

END
