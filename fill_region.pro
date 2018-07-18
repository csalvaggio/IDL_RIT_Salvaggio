;+
; :NAME:
;    FILL_REGION
;
; :PURPOSE:
;    This function will use morphological operations to
;    fill the interior region of a provided boundary
;    image.
;
; :CATEGORY:
;    Image Processing.
;
; :CALLING SEQUENCE:
;    Result = $
;       FILL_REGION( boundary, $
;                    point, $
;                    sizeLimit, $
;                    STATUS=status )
;
; :RETURN VALUE:
;    This function will return an array of byte data type with
;    the same dimensions as the provided input image that contains
;    a filled boundary region.
;
; :INPUTS:
;    boundary
;       A binary image containing a cluster of pixels for
;       which the mean coordinate (centroid) is to be 
;       computed.
;    point
;       A 2-element vector defining a point located within the
;       provided boundary from which region growth should begin.
;    sizeLimit
;       The maximum number of pixels that the region should be
;        allowed to grow to (to prevent runaways -OR- to limit
;        region size).
;
; :KEYWORD PARAMETERS:
;    STATUS=status
;       A returned boolean variable indicating if the number of 
;       pixels in the grown region has exceeded the limit specified.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    FILL_REGIN
;    FIND_CENTER
;
; :REFERENCE:
;     Gonzalez, R.C. and R.E. Woods, Digital Image Processing, 
;     Third Edition, Prentice Hall, Upper Saddle River, New Jersey, 
;     2007 (ISBN: 0131687288X / ISBN-13: 978-0131687288)
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    March, 2007       Original code
;    June, 2008        Size limit return status added to indicate
;                      runaway situations (Philip Salvaggio)
;-

FUNCTION FILL_REGION, boundary, point, sizeLimit, STATUS=status

;+
; Create a local copy of the input array to protect data in the 
; calling module
;-
   localBoundary = boundary

;+
; Threshold the array to be certain that the input data set is binary
;-
   localBoundary[ WHERE( localBoundary GT 0 ) ] = 1
   localBoundary = BYTE( localBoundary )

;+
; Determine the dimensions of the provided boundary image
;-
   dimensions = SIZE( localBoundary, /DIMENSIONS )
   numberSamples = dimensions[0]
   numberLines = dimensions[1]
   
;+
; Create a binary array to contain the region pixels and
; set the provided initial point on
;-
   filledRegion = REPLICATE( 0B, numberSamples, numberLines )
   filledRegion[point[0],point[1]] = 1B

;+
; Iteratively grow the filled region commencing at the provided
; initial point using a series of morphological dialations and
; boundary condition checks as described by Gonzalez (2007)
;-
   k = 0
   structure = [ [ 0, 1, 0 ], $
                 [ 1, 1, 1 ], $
                 [ 0, 1, 0 ] ]
   REPEAT BEGIN
      previous = TOTAL( filledRegion, /INTEGER )
      filledRegion = DILATE( filledRegion, structure ) AND ( NOT localBoundary )
      k = TOTAL( filledRegion, /INTEGER )
   ENDREP UNTIL ( ( previous EQ k ) OR ( k GT sizeLimit ) )

;+
; Return the grown binary region array to the calling module if
; the region is finite and with the specified size limit, otherwise, 
; return an error condition
;-
   IF ( k LE sizeLimit ) THEN BEGIN
      status = 1
      RETURN, filledRegion
   ENDIF ELSE BEGIN
      status = 0
      RETURN, filledRegion
   ENDELSE

END