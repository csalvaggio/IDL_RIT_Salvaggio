;+
; :NAME:
;    RMS_EDGE
;
; :PURPOSE:
;    This function accepts two images, original and altered. The 
;    two images must have the same dimensions. The original image 
;    is converted to luminance, and then an edge mask is 
;    calculated. The edge mask is found by applying a gradient 
;    operator, applying a threshold, and then dilating the result. 
;    The edge mask is applied to both input images, and the RMS 
;    deviation is found for each channel separately.
;
; :CATEGORY:
;    Statistics.
;
; :CALLING SEQUENCE:
;    Result = RMS_EDGE( original, altered )
;
; :INPUTS:
;    original
;       A color or greyscale image to serve as the reference.
;    mean
;       A color or greyscale image to be compared to the reference.
;
; :KEYWORD PARAMETERS:
;    None
;
; :RETURN VALUE:
;    Result is either a scalar (if provided input were greyscale
;    images) or a 3-element vecotr (if provided input were color
;    images) containing the individual band RMS value in the vicinity
;    of edges.
;
; :ERROR CHECKING:
;    RETURN, -1 
;       if no significant edges with magnitude greater than the 
;       threshold could be found
;
; :SIDE EFFECTS:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Juliet Bernstein
;    December, 2007    Original code
;    September, 2012   Converted to a general-purpose routine (Carl Salvaggio)
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

FUNCTION RMS_EDGE, original, altered

;+
; If the provided images are of different dimensions terminate execution
;-
   dimensions = SIZE( original )
   IF ( TOTAL( dimensions - SIZE( altered ) ) NE 0 ) THEN BEGIN
      MESSAGE, 'Provided images do not have the same dimensions'
   ENDIF

;+
; Prepare an image from which to compute the edges (luminance for a color 
; input or direct greyscale input)
;-
   IF ( dimensions[0] GT 2 ) THEN BEGIN
      numberBands = dimensions[1]
      numberSamples = dimensions[2]
      numberLines = dimensions[3]
      R = FLOAT( REFORM( original[0,*,*] ) )
      G = FLOAT( REFORM( original[1,*,*] ) )
      B = FLOAT( REFORM( original[2,*,*] ) )
      gray = BYTE( 0.299*R + 0.587*G + 0.114*B )
   ENDIF ELSE BEGIN
      numberBands = 1;
      numberSamples = dimensions[1]
      numberLines = dimensions[2]
      gray = original
   ENDELSE

;+
; Compute a Sobel gradient to locate the edges on the greyscale image
;-
   edges = SOBEL(gray)

;+
; Threshold and normalize the edge image
;-
   threshold = 0.2 * MAX( edges )
   thresholded = edges GT threshold

;+
; Grow the edges using a morphological dilation
;-
   radius = 2
   structureElement = SHIFT( DIST( 2*radius + 1 ), radius, radius ) LE radius
   dilateImg = REPLICATE( MIN( thresholded ), numbersamples+2, numberlines+2 )
   dilateImg[1,1] = thresholded
   dilateImg = DILATE( dilateImg, structureElement )
   dilated = dilateImg[ 1:numbersamples, 1:numberlines ]

;+
; Find the locations and number of edge pixels
;-
   index = WHERE( dilated, count )
   IF ( count NE 0 ) THEN BEGIN

;+
; Compute the RMS error at the edge pixels for each channel
;-
      rms = DBLARR( numberbands )
      FOR i = 0, numberbands-1 DO BEGIN
         origband = REFORM( original[i,*,*] )
         alteredband = REFORM( altered[i,*,*] )
         error = FLOAT( origband[index] ) - FLOAT( alteredband[index] )
         rms[i] = SQRT( MEAN( error * error ) )
      ENDFOR
  ENDIF ELSE BEGIN

;+
; If the magnitude of no edges exceed threshold terminate execution
;-
    MESSAGE, 'There are no significant edges in the provided image'
  ENDELSE

;+
; Return the RMS error in the vicinity of the edges
;-
  RETURN, rms

END