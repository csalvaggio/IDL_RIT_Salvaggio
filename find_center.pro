;+
; :NAME:
;    FIND_CENTER
;
; :PURPOSE:
;    This function will look at a cluster of pixels in a 
;    binary image and determine the mean coodinate of 
;    that cluster.  It is assumed that there is only ONE
;    cluster of pixels in the provided image.  If there 
;    are more, then the mean coordinate of ALL cluster
;    pixels will be returned, which is probably an 
;    undesirable result.
;
; :CATEGORY:
;    Image Processing.
;
; :CALLING SEQUENCE:
;    Result = $
;       FIND_CENTER( clusterImage )
;
; :RETURN VALUE:
;    This function will return a 2-element vector containing the 
;    centroid coordinate for the provided cluster of pixels or a 
;    scalar value of -1 if a single point or fewer was provided.
;
; :INPUTS:
;    clusterImage:
;       A binary image containing a cluster of pixels for
;       which the mean coordinate (centroid) is to be 
;       computed.
;
; :KEYWORD PARAMETERS:
;    None
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    None
;
; :REFERENCE:
;     Gonzalez, R.C. and R.E. Woods, Digital Image Processing, 
;     Third Edition, Prentice Hall, Upper Saddle River, New Jersey, 
;     2007 (ISBN: 0131687288X / ISBN-13: 978-0131687288)
;
; :MODIFICATION HISTORY:
;    Written by:       Philip Salvaggio
;    June, 2008        Original code
;    September, 2008   Converted to a general-purpose routine (Carl Salvaggio)
;-

FUNCTION FIND_CENTER, clusterImage

;+
; Create a local copy of the input array to protect data in the 
; calling module
;-
   localImage = clusterImage

;+
; Threshold the array to be certain that the input data set is binary
;-
   localImage[ WHERE( localImage GT 0 ) ] = 1
   localImage = BYTE( localImage )
   
;+
; Determine the location of all cluster pixels
;-
   indices = WHERE( localImage EQ 1 )

;+
; If a cluster of pixels exists (more than a single pixel) then
; determine the 2-dimensional array indices for all cluster points
; and return the mean coordinate for each dimension, otherwise,
; return an error condition
;-
   numberPoints = N_ELEMENTS( indices )
   IF NOT( (numberPoints EQ 1) && (indices EQ -1) ) THEN BEGIN
      indices = ARRAY_INDICES( localImage, indices )
      RETURN, [ ROUND( MEAN( indices[0,*] ) ), ROUND( MEAN( indices[1,*] ) ) ]
   ENDIF ELSE BEGIN
      RETURN, -1
   ENDELSE

END