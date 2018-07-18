;+
; :NAME:
;    SCRAMBLE_IMAGE
;
; :PURPOSE:
;    This function returns a version of the provided image in which
;    the digital count value locations have been scrambled a specified
;    number of times (numberTransformations) using toral automorphism
;    (G. Voyatzis and I. Pitas, Applications of toral automorphism in
;    image watermarking, Proceedings of the IEEE Conference on Image
;    Processing, Volume 2, pp. 237-240, 1996).
;
; :CATEGORY:
;    Image Processing.
;
; :CALLING SEQUENCE:
;    Result = SCRAMBLE_IMAGE( image, $
;                             key, $
;                             numberTransformations, 
;                             [PERIOD=period], $
;                             [/UNSCRAMBLE] )
;
; :INPUTS:
;    image
;       An NxN two-dimensional array containing the image to be 
;       scrambled.
;    key
;       A secret key used to define the toral automorphism
;       transformation matrix.
;    numberTransformations
;       The number of toral automorphism transforms to be applied to 
;       scramble the pixel positions (in the case where the image is 
;       being unscrambled, this argument should still be defined as the 
;       original number of transformations used to scramble the image, 
;       the congugate number with respect to the period of the 
;       specified toral automorphism due to the provide key will be
;       computed).
;
; :KEYWORD PARAMETRS:
;      PERIOD
;         The period of the toral automorphism defined by the
;         supplied secret key.
;      UNSCRAMBLE
;         Set this keyword to unscramble the scambled image by applying
;         the conjugate number of transformations K to the number of
;         transfomations used to originally scramble the image N where;
;         the period of the toral automorphism T defined by the
;         supplied secret key follows the constraint that T = N + K.
;
; :RETURN VALUE:
;    Result is a scrambled version of the provided image that has
;    undergone a specied number of transformations or a -1 upon
;    failure.
;
; :ERROR CHECKING:
;    RETURN, -1
;       If the number of transformations requested by the calling routine
;       exceeds the period of the toral automorphism defined by the provide
;       secret key (key).
;    RETURN, -2
;       If the provide image is not square.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    November, 2007    Original code
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

FUNCTION SCRAMBLE_IMAGE, image, $
                         key, $
                         numberTransformations, $
                         PERIOD=period, $
                         UNSCRAMBLE=unscramble

;*************************************************************************
; Determine the dimensions of the provided image and confirm that it is square
;*************************************************************************
   dims = SIZE( image, /DIMENSIONS )
   n = dims[0]

   IF ( dims[0] NE dims[1] ) THEN RETURN, -2

;*************************************************************************
; Specify the toral automorphism transformation matrix specified by the 
; provided secret key
;*************************************************************************
   tMatrix = [ [ 1, 1 ], [ key, key+1 ] ]

;*************************************************************************
; Set up a matrix containing the original pixel coordinate pairs (x,y)
; for every pixel in the image
;*************************************************************************
   oCoords = LONARR( 2, n, n )
   FOR row = 0, n-1 DO oCoords[0,*,row] = INDGEN( n )
   FOR col = 0, n-1 DO oCoords[1,col,*] = INDGEN( n )
   oCoords = REFORM( oCoords, 2, n*n )

   tCoords = oCoords

;*************************************************************************
; Determine the period for the toral automorphism defined by the 
; transformation matrix previously computed and confirm that the number
; of transformations specified is less than this period
;*************************************************************************
   iteration = 0L
   REPEAT BEGIN
      tCoords = ( tCoords ## tMatrix ) MOD n
      iteration = iteration + 1
   ENDREP UNTIL ( TOTAL( oCoords EQ tCoords, /INTEGER ) EQ (2*n*n) ) 
   period = iteration

   IF ( numberTransformations GT period ) THEN RETURN, -1

;*************************************************************************
; Determine the actual number of iterations to carry out dependent on
; whether this is a scramble or unscramble operation
;*************************************************************************
   IF KEYWORD_SET( unscramble ) THEN BEGIN
      nIterations = period - numberTransformations
   ENDIF ELSE BEGIN
      nIterations = numberTransformations
   ENDELSE

;*************************************************************************
; Perform the specified number of transformations of the coordinates
;*************************************************************************
   FOR iteration = 1, nIterations DO BEGIN
      tCoords = ( tCoords ## tMatrix ) MOD n
   ENDFOR

   scrambledIndices = REFORM( tCoords[1,*] * n + tCoords[0,*], n*n )

;*************************************************************************
; Use the transformed coordinates to scramble/unscramble the pixel
; locations and return the scrambled/unscrambled image
;*************************************************************************
   RETURN, REFORM( image[scrambledIndices], n, n )

END
