;+
; :NAME:
;    QUAD_TO_QUAD_PROJECTIVE_TRANSFORM
;
; :PURPOSE:
;    This function will perform a quadrilateral-to-quadrilateral projective
;    transform of the provided image.  The transformation is defined using 
;    the provided quadrilateral vertices defined in a clockwise order
;    beginning at the upper left-hand corner.  The provided image coordinates 
;    are targeted to fall at the provided map coordinates.
;
; :CATEGORY:
;    GENERAL.
;
; :CALLING SEQUENCE:
;    result = QUAD_TO_QUAD_PROJECTIVE_TRANSFORM( image, $
;                                                imageQuad, $
;                                                mapQuad, $
;                                                MAP_DIMENSIONS=mapDimensions, $
;                                                BACKGROUND_VALUE=backgroundValue
;
; :INPUTS:
;    image
;       A two-dimensional or three-dimensional array containg either greyscale
;       or color image data, respectively.
;    imageQuad
;       An 8-element vector containing the vertices of the quadilateral enclosing
;       the region on the image to be transformed.  The vector has the form
;          [ xUL, yUL, xUR, yUR, xLR, yLR, xLL, yLL ]
;    mapQuad
;       An 8-element vector containing the vertices of the quadilateral enclosing
;       the region on the map to be targeted.  The vector has the form
;          [ uUL, vUL, uUR, vUR, uLR, vLR, uLL, vLL ]
;
; :KEYWORD PARAMETRS:
;      MAP_DIMENSIONS
;         Set this keyword to a 2-element vector containing the [width,height] of
;         the targeted map region.  If this is omitted, a region corresponding to 
;         the bounding rectangle around the quadrilateral vertices will be used.
;      BACKGROUND_VALUE
;         Set this keyword to the value to be placed in the transformed data
;         at points that fall outside the boundaries of the provided image to be
;         transformed [DEFAULT=0].
;
; :ERROR CHECKING:
;    None.
;
; :SIDE EFFECTS:
;    None.
;
; :REQUIRES:
;    NORMH
;    PROJECTIVE_MAPPING_MATRIX
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

FUNCTION QUAD_TO_QUAD_PROJECTIVE_TRANSFORM, image, $
                                            imageQuad, $
                                            mapQuad, $
                                            MAP_DIMENSIONS=mapDimensions, $
                                            BACKGROUND_VALUE=backgroundValue

;+
; Based on the provided quadrilateral vertices for the image and the targeted
; map, determine the projective mapping matrix
;-
   imageX = [ DOUBLE( imageQuad[0] ), DOUBLE( imageQuad[2] ), DOUBLE( imageQuad[4] ), DOUBLE( imageQuad[6] ) ]
   imageY = [ DOUBLE( imageQuad[1] ), DOUBLE( imageQuad[3] ), DOUBLE( imageQuad[5] ), DOUBLE( imageQuad[7] ) ]
   mapX = [ DOUBLE( mapQuad[0] ), DOUBLE( mapQuad[2] ), DOUBLE( mapQuad[4] ), DOUBLE( mapQuad[6] ) ]
   mapY = [ DOUBLE( mapQuad[1] ), DOUBLE( mapQuad[3] ), DOUBLE( mapQuad[5] ), DOUBLE( mapQuad[7] ) ]
   mappingMatrix = PROJECTIVE_MAPPING_MATRIX( imageX, imageY, mapX, mapY )

;+
; Establish the minimum and maximum x- and y-coordinates for the provided 
; target map region as well as the dimensions of the bounding rectangle if
; the map dimensions are not provided directly
;-
   IF KEYWORD_SET( mapDimensions ) THEN BEGIN
      minSample = 0
      maxSample = mapDimensions[0]-1
      minLine = 0
      maxLine = mapDimensions[1]-1
   ENDIF ELSE BEGIN
      minSample = MIN( mapX )
      maxSample = MAX( mapX )
      minLine = MIN( mapY )
      maxLine = MAX( mapY )
      mapDimensions = [ maxSample-minSample+1, maxLine-minLine+1 ]
   ENDELSE

;+
; Form a matrix of map coordinates to which the tranformation of the image data
; should be targeted (the matrix will contain the homogenous coordinates in the
; columns)
;-
   samples = ( INDGEN( mapDimensions[0] )+minSample ) # REPLICATE( 1, mapDimensions[1] )
   lines = ( INDGEN( mapDimensions[1] )+minLine ) ## REPLICATE( 1, mapDimensions[0] )
   mapCoordinates = REFORM( [ REFORM( samples, mapDimensions[0]*mapDimensions[1] ), $
                              REFORM( lines, mapDimensions[0]*mapDimensions[1] ), $
                              REPLICATE( 1, mapDimensions[0]*mapDimensions[1] ) ], $
                            mapDimensions[0]*mapDimensions[1], 3 )

;+
; Transform the map coordinates to image coordinate space to establish where the
; image data should be resampled
;-
   imageCoordinates = LONG( NORMH( mappingMatrix ## mapCoordinates ) )

;+
; Compute the size of the provided image data to be transformed so that the
; boundaries of resampling are established
;-
   numberDimensions = SIZE( image, /N_DIMENSIONS )
   CASE numberDimensions OF
      2: BEGIN
            imageDimensions = SIZE( image, /DIMENSIONS )
            numberChannels = 1
         END
      3: BEGIN
            imageDimensions = [ (SIZE( image, /DIMENSIONS ))[1], (SIZE( image, /DIMENSIONS ))[2] ]
            numberChannels = 3
         END
   ENDCASE

;+
; Convert the two-dimensional image coordinates to one-dimensional indices for
; resampling
;-
   imageIndices = imageCoordinates[*,1]*imageDimensions[0] + imageCoordinates[*,0]

;+
; Created the projective transfomed (mapped) image for each channel of the
; provided image data
;-
   mappedImage = BYTARR( numberChannels, mapDimensions[0], mapDimensions[1] )
   FOR channel = 0, numberChannels-1 DO BEGIN
      mappedImage[channel,*,*] = image[numberChannels*imageIndices+channel]
   ENDFOR

;+
; Determine which of the mapped coordinates are out of bounds with respect
; to the provided image data and replace the resampled values at these locations
; with the background value [DEFAULT=0]
;-
   IF NOT KEYWORD_SET( backgroundValue ) THEN backgroundValue = 0
   outBounds = !NULL
   outBounds = [ outBounds, WHERE( imageCoordinates[*,0] LT 0, /NULL ) ]
   outBounds = [ outBounds, WHERE( imageCoordinates[*,0] GE imageDimensions[0], /NULL ) ]
   outBounds = [ outBounds, WHERE( imageCoordinates[*,1] LT 0, /NULL ) ]
   outBounds = [ outBounds, WHERE( imageCoordinates[*,1] GE imageDimensions[1], /NULL ) ]
   IF ( N_ELEMENTS( outBounds ) GT 0 ) THEN BEGIN
      FOR channel = 0, numberChannels-1 DO BEGIN
         mappedImage[numberChannels*outBounds+channel] = backgroundValue
      ENDFOR
   ENDIF

;+
; Return the mapped image data
;-
   RETURN, REFORM( mappedImage )

END