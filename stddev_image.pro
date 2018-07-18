;+
; :NAME:
;    STDDEV_IMAGE
;
; :PURPOSE:
;    This function will compute the standard deviation in a local
;    neighborhood around each pixel in a greyscale image.
;
; :CATEGORY:
;    Image Processing.
;
; :CALLING SEQUENCE:
;    Result = STDDEV_IMAGE( image, neighborhoodSize )
;
; :INPUTS:
;    image
;       A 2-dimensional array containing the greyscale image data
;    neighborhoodSize
;       A scalar defining both the horizontal and vertical dimension of
;       the local neighorhood in which to compute the standard deviation
;
; :KEYWORD PARAMETERS:
;    None
;
; :RETURN VALUE:
;    Result is a double precision image, the same dimension as the input,
;    containing the neighborhood standard deviation for each pixel.
;
; :SIDE EFFECTS:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Philip Salvaggio
;    February, 2011    Original code
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

FUNCTION STDDEV_IMAGE, image, neighborhoodSize

   localImage = DOUBLE( image )

   imageSquared = localImage * localImage

   RETURN, SQRT( ( CONVOL( imageSquared, REPLICATE( 1, neighborhoodSize, neighborhoodSize ), /EDGE_WRAP ) - $
                   CONVOL( localImage, REPLICATE( 1, neighborhoodSize, neighborhoodSize ), /EDGE_WRAP )^2 / $
                   DOUBLE( neighborhoodSize )^2 ) / ( DOUBLE( neighborhoodSize )^2 - 1 ) )

END