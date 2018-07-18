;+
; :NAME:
;    SPATIAL_FILTER
;
; :PURPOSE:
;    This function will return a response from the spatial filtering of a one-, two-,
;    or three-dimensional array with the provided one- or two-dimensional filter.
;    If specified, this routine will properly rotate the filter before performing the 
;    filtering operation to perform pure mathematical convolution.
;
; :CATEGORY:
;    Image Processing.
;
; :CALLING SEQUENCE:
;    Result = SPATIAL_FILTER( array, filter [, /ROTATE] [, /VERBOSE] )
;
; :INPUTS:
;    array
;       A one-, two-, or three-dimensional array containing the data on which to
;       perform the spatial filtering.
;    filter
;       A one- or two-dimensional array containing the weights for the filter.
;
; :KEYWORD PARAMETRS:
;    ROTATE
;       An optional parameter that if set will cause the filter to be
;       rotated before performing the filtering operation.
;    VERBOSE
;       An optional parameter that if set will cause performance timing
;       information to be reported to the console.
;
; :RETURN VALUE:
;    Result is a double-precision array, the same size as the original array
;    that contains the response of the spatial filtering.  The response will be 
;    normalized by the weight of the filter.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    April, 2012       Original code
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

FUNCTION SPATIAL_FILTER, array, filter, ROTATE=rotate, VERBOSE=verbose

;+
; Parse the optional keyword parameters
;-
   IF NOT KEYWORD_SET( rotate ) THEN rotate = 0
   IF NOT KEYWORD_SET( verbose ) THEN verbose = 0

;+
; Determine the dimensions of the provided array and force it to
; be represented locally as a three-dimensional array for processing
;-
   aDimensions = SIZE( array, /DIMENSIONS )
   CASE SIZE( array, /N_DIMENSIONS ) OF
      1: BEGIN
            aPlanes = 1
            aSamples = aDimensions[0]
            aLines = 1
         END
      2: BEGIN
            aPlanes = 1
            aSamples = aDimensions[0]
            aLines = aDimensions[1]
         END
      3: BEGIN
            aPlanes = aDimensions[0]
            aSamples = aDimensions[1]
            aLines = aDimensions[2]
         END
   ENDCASE
   localArray = DOUBLE( REFORM( array, aPlanes, aSamples, aLines ) )

;+
; Determine the dimensions of the provided filter and force it to
; be represented locally as a two-dimensional array for processing
;-
   fDimensions = SIZE( filter, /DIMENSIONS )
   CASE SIZE( filter, /N_DIMENSIONS ) OF
      1: BEGIN
            fSamples = fDimensions[0]
            fLines = 1
         END
      2: BEGIN
            fSamples = fDimensions[0]
            fLines = fDimensions[1]
         END
   ENDCASE

;+
; If specified, rotate the filter prior to filtering
;-
   IF ( rotate ) THEN BEGIN
      localFilter = DOUBLE( ROTATE( REFORM( filter, fSamples, fLines ), 2 ) )
   ENDIF ELSE BEGIN
      localFilter = DOUBLE( REFORM( filter, fSamples, fLines ) )
   ENDELSE

;+
; Perform the convolution operation
;-
   response = localArray * 0D
   startTime = SYSTIME( /SECONDS )
   FOR aPlane = 0, aPlanes-1 DO BEGIN
      a = REFORM( localArray[aPlane,*,*], 1, aSamples, aLines )
      FOR fLine = 0, fLines-1 DO BEGIN
         FOR fSample = 0, fSamples-1 DO BEGIN
            weight = localFilter[fSample,fLine]
            sampleShift = fSamples/2 - fSample
            lineShift =  fLines/2 - fLine
            response[aPlane,*,*] = response[aPlane,*,*] + weight * SHIFT( a, 0, sampleShift, lineShift )
         ENDFOR
      ENDFOR
   ENDFOR
   IF ( verbose ) THEN PRINT, SYSTIME( /SECONDS ) - startTime, ' seconds'

;+
; Normalize the response by the weight of the filter
;-
   filterWeight = TOTAL( localFilter, /DOUBLE )
   IF ( filterWeight NE 0 ) THEN response = response / filterWeight

;+
; Return the response to the calling routine
;-
   RETURN, REFORM( response )

END