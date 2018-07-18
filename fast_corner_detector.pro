;+
; :NAME:
;    FAST_CORNER_DETECTOR
;
; :PURPOSE:
;    This function will return a 2xn array of corner coordinates identified
;    in the provided greyscale image.
;
; :CATEGORY:
;    Image Processing.
;
; :PARAMETERS:
;    image
;       A two-dimensional (m x n) array containing the greyscale image to be used
;       for feature/corner extraction.
;
; :KEYWORDS:
;    DIFFERENCE_THRESHOLD
;       An optional named variable containing the brightness threshold to 
;       be used to determine whether a surrounding pixels is brighter than
;       or darker than the candidate corner pixel (default value is 50). 
;    CONTIGUOUS_THRESHOLD
;       An optional named variable containg the number of contiguous pixels
;       that must appear in sequence in order for a candidate pixel to be 
;       considered a corner pixel (default value is 12).
;    NON_MAXIMAL_SUPPRESSION
;       An optional keyword indicating whether non-maximal suppression 
;       should be used to eliminate "clumping" of identified corner points.
;    CORNER_IMAGE
;       An optional named variable that will contain, upon return, a visualization
;       of the corner points (shown in red) laid upon the original image (the
;       returned value is a 3 x m x n color image array).
;    VERBOSE
;       An optional keyword indicating whether diagnostic information should
;       be displayed to the console during execution.
;
; :RETURN VALUE:
;    This function returns a 2xn array containing the image coordinates of the
;    n corner points identified by the FAST detector.
;
; :REFERENCE:
;       E. Rosten and T. Drummond, "Machine learning for high-speed corner 
;       detection,” in Computer Vision - ECCV 2006, A. Leonardis, H. Bischof, 
;       and A. Pinz, eds., Lecture Notes in Computer Science 3951, pp. 430–443, 
;       Springer Berlin / Heidelberg, 2006.
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    Original Code:    Sep 25, 2010
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

FUNCTION FAST_CORNER_DETECTOR, image, $
                               DIFFERENCE_THRESHOLD=differenceThreshold, $
                               CONTIGUOUS_THRESHOLD=contiguousThreshold, $
                               NON_MAXIMAL_SUPPRESSION=nonmaximalSuppression, $
                               CORNER_IMAGE=cornerImage, $
                               VERBOSE=verbose

;+
; If not provided, set the default values for the brightness difference
; and the contiguous pixel thresholds
;-
   IF NOT KEYWORD_SET( differenceThreshold ) THEN differenceThreshold = 50
   IF NOT KEYWORD_SET( contiguousThreshold ) THEN contiguousThreshold = 12

;+
; Set up the shifts to be used to reach the 16-pixel surrounding circle
;-
   shifts = [ [0,-3], $
              [1,-3], $
              [2,-2], $
              [3,-1], $
              [3,0], $
              [3,1], $
              [2,2], $
              [1,3], $
              [0,3], $
              [-1,3], $
              [-2,2], $
              [-3,1], $
              [-3,0], $
              [-3,-1], $
              [-2,-2], $
              [-1,-3] ]

;+
; Determine the dimensions of the provided image
;-
   dimensions = SIZE( image, /DIMENSIONS )
   numberSamples = LONG64( dimensions[0] )
   numberLines = LONG64( dimensions[1] )
   dataType = SIZE( image, /TYPE )

;+
; Initiate execution timer
;-
   startTime = SYSTIME( /SECONDS )

;+
; Compute the brightness count differences between the center pixel and each
; of the pixels in the surrounding circle
;-
   pixelDifferences = INTARR( numberSamples, numberLines, 16 )
   FOR i = 0, 15 DO BEGIN
      pixelDifferences[*,*,i] = FIX( image ) - SHIFT( FIX( image ), shifts[0,i], shifts[1,i] )
   ENDFOR

;+
; Mark those locations where the center pixel is darker than the pixels in the
; surrounding circle
;-
   darkerPixels = BYTARR( numberSamples, numberLines, 16 )
   index = WHERE( pixelDifferences GT differenceThreshold, count )
   IF ( count GT 0 ) THEN darkerPixels[index] = 1

;+
; Mark those locations where the center pixel is brighter than the pixels in the
; surrounding circle
;-
   brighterPixels = BYTARR( numberSamples, numberLines, 16 )
   index = WHERE( pixelDifferences LT -differenceThreshold, count )
   IF ( count GT 0 ) THEN brighterPixels[index] = 1

;+
; Determine corners by finding those pixels that are surrounded by the specified
; number contiguous brighter or darker pixels
;-
   cornerImage = BYTARR( numberSamples, numberLines )
   FOR i = 0, 15 DO BEGIN
      contiguousSequence = SHIFT( brighterPixels, 0, 0, i )
      numberContiguous = TOTAL( contiguousSequence[*,*,0:contiguousThreshold-1], 3, /INTEGER )
      index = WHERE( numberContiguous EQ contiguousThreshold, count )
      IF ( count GT 0 ) THEN cornerImage[index] = 1

      contiguousSequence = SHIFT( darkerPixels, 0, 0, i )
      numberContiguous = TOTAL( contiguousSequence[*,*,0:contiguousThreshold-1], 3, /INTEGER )
      index = WHERE( numberContiguous EQ contiguousThreshold, count )
      IF ( count GT 0 ) THEN cornerImage[index] = 1
   ENDFOR
   numberCorners = TOTAL( cornerImage, /INTEGER )
   IF KEYWORD_SET( verbose ) THEN PRINT, 'Number of corners = ', STRTRIM( numberCorners, 2 )

;+
; If desired, compute the score function for non-maximal suppression
;-
   IF KEYWORD_SET( nonmaximalSuppression ) THEN BEGIN

      vBright = TOTAL( ABS( pixelDifferences * brighterPixels ) - differenceThreshold, 3 )
      vDark = TOTAL( ABS( pixelDifferences * darkerPixels ) - differenceThreshold, 3 )
      v = vDark
      index = WHERE( vBright GT vDark, count )
      IF ( count GT 0 ) THEN v[index] = vBright[index]

;+
; If desired, apply non-maximal suppression to adjacent corner pixels
;-
      neighborhoodWidth = 3
      neighborhoodHeight = 3
      suppressionMap = cornerImage * v
      FOR lineShift = -neighborhoodHeight/2, neighborhoodHeight/2 DO BEGIN
         FOR sampleShift = -neighborhoodWidth/2, neighborhoodWidth/2 DO BEGIN
            index = WHERE( suppressionMap LT SHIFT( suppressionMap, sampleShift, lineShift ), count )
            IF ( count GT 0 ) THEN cornerImage[index] = 0
         ENDFOR
      ENDFOR
      numberCorners = TOTAL( cornerImage, /INTEGER )
      IF KEYWORD_SET( verbose ) THEN PRINT, 'Number of corners (after non-maximal suppression) = ', STRTRIM( numberCorners, 2 )
   ENDIF

;+
; Display the execution time
;-
   executionTime = SYSTIME( /SECONDS ) - startTime
   IF KEYWORD_SET( verbose ) THEN PRINT, 'Execution time = ', STRTRIM( executionTime, 2 ), ' seconds'

;+
; Construct a visualization of where the corner points fall on the 
; original image
;-
   index = WHERE( cornerImage EQ 1, count )

   cornerImage = MAKE_ARRAY( 3, numberSamples, numberLines, TYPE=dataType )
   cornerImage[0,*,*] = image
   cornerImage[1,*,*] = image
   cornerImage[2,*,*] = image

   IF ( count GT 0 ) THEN BEGIN
      cornerImage[index*3] = 255
      cornerImage[index*3+1] = 0
      cornerImage[index*3+2] = 0
   ENDIF
   
;+
; Return the corner coordinate list to the calling module
;-
   IF ( count GT 0 ) THEN RETURN, ARRAY_INDICES( image, index ) ELSE RETURN, -1L
   
END