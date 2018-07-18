;+
; :NAME:
;     FIND_CLUSTER_CENTERS
;
; :PURPOSE:
;     This function will find the centroid coordinates for clusters
;     located throughout a provided image.  The clusters will first
;     be refined by thresholding the provided image using the 
;     supplied digital count threshold value.
;
; :CATEGORY:
;     Image Processing.
;
; :CALLING SEQUENCE:
;     Result = $
;        FIND_CLUSTER_CENTERS( image, $
;                              threshold )
;
; :RETURN VALUE:
;     This function will return a 2xN array of cluster centroid values
;     that have been located in the thresholded version of the image
;     provided.
;
; :INPUTS:
;     image:
;        An array containing the clusters of points for which the 
;        centroid coordinates are to be found.
;     threshold:
;        A scalar defining the digital count threshold to be used
;        in the creation of the binary version of the provided array.
;
; :KEYWORD PARAMETERS:
;     NONE
;
; :RETURN VALUE:
;    This function returns a 2xn array containing the cluster center coordinates
;    for the clusters identified by this routine.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    FILL_REGIN
;    FIND_CENTER
;
; REFERENCE:
;     Gonzalez, R.C. and R.E. Woods, Digital Image Processing, 
;     Third Edition, Prentice Hall, Upper Saddle River, New Jersey, 
;     2007 (ISBN: 0131687288X / ISBN-13: 978-0131687288)
;
; MODIFICATION HISTORY:
;     Written by:       Philip Salvaggio
;     March, 2007       Original code
;     September, 2008   Converted to a general-purpose routine (Carl Salvaggio)
;-

FUNCTION FIND_CLUSTER_CENTERS, image, threshold, RUNAWAY_LIMIT=runawayLimit

;+
; If not provided by the calling module, set the maximum size (in pixels)
; of the growth region
;-
   IF NOT KEYWORD_SET( runawayLimit ) THEN runawayLimit = 100

;+
; Determine the dimensions of the provided cluster image
;-
   dimensions = SIZE( image, /DIMENSIONS )
   numberSamples = dimensions[0]
   numberLines = dimensions[1]

;+
; Threshold the array to create a binary version of the provided
; cluster image
;-
   thresholdedImage = BYTARR( numberSamples, numberLines )
   thresholdedImage[ WHERE( image GE threshold ) ] = 1B

;+
; Create a binary-boundary image outlining each cluster in the thresholded
; data array using the technique described by Gonzalez (2007)
;-
   structuringElement = [ [ 1, 1, 1 ], $
                          [ 1, 1, 1 ], $
                          [ 1, 1, 1 ] ]
   boundaryImage = thresholdedImage XOR DILATE( thresholdedImage, structuringElement )

;+
; Make an outline around the entire image to prevents runaways
;-
   boundaryImage[*,0] = 1
   boundaryImage[*,numberLines-1] = 1
   boundaryImage[0,*] = 1
   boundaryImage[numberSamples-1,*] = 1

;+
; Create a version of the thresholded image with a one pixel wide
; black outline surrounding the entire image to be used to prevent
; growth runaway at the borders of the image
;-
   boundaryRemovedThresholdedImage = BYTARR( numberSamples, numberLines )
   boundaryRemovedThresholdedImage[1:numberSamples-2,1:numberLines-2] = $
      thresholdedImage[1:numberSamples-2,1:numberLines-2]

;+
; Determine all locations of pixels within clusters to determine
; possible locations for the start of region growth
;-
   whiteIndices = WHERE( boundaryRemovedThresholdedImage )

;+
; Iterate through each possible cluster origin and grow/fill the region
; that contains each origin point
;-
firstCenter = 1
structuringElement = [ [ 0, 1, 0 ], $
                       [ 1, 1, 1 ], $
                       [ 0, 1, 0 ] ]
   FOR i=0L, N_ELEMENTS( whiteIndices )-1 DO BEGIN
      indices = ARRAY_INDICES( boundaryRemovedThresholdedImage, whiteIndices[i] )
      IF ( boundaryRemovedThresholdedImage[indices[0],indices[1]] ) THEN BEGIN
         filledImage = FILL_REGION( boundaryImage, indices, runawayLimit, STATUS=notRunaway )

;+
; Once a region is filled, remove it from the set of regions/clusters to be
; examined by removing it from the boundary-removed thresholded image (that is,
; setting all the locations in the current cluster to zero
;-
         boundaryRemovedThresholdedImage = boundaryRemovedThresholdedImage XOR $
                                           ( DILATE( filledImage, structuringElement ) AND $
                                             boundaryRemovedThresholdedImage )

;+
; If a runaway condition was not encountered for the growth of the current 
; cluster, then compute and record the centroid coordinate, otherwise, move
; on to the next cluster
;-
         IF ( notRunaway ) THEN BEGIN
            center = FIND_CENTER( filledImage )
            IF ( firstCenter ) THEN BEGIN
               centers = [ [ center[0], center[1] ] ]
               firstCenter = 0
            ENDIF ELSE BEGIN
               centers = [ centers, [ center[0], center[1] ] ]
            ENDELSE
         ENDIF 
      ENDIF
   ENDFOR

;+
; Reconfigure the centroid coordinate array into a 2xN format and
; return to the calling module
;-
   RETURN, REFORM( centers, 2, N_ELEMENTS( centers )/2 )

END