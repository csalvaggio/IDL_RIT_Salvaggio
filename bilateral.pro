;+
; :NAME:
;    BILATERAL
;
; :PURPOSE:
;    This function performs bilateral filtering of a greyscale or multi-
;    band color image.  The filtering is based on a Gaussian distribution
;    in both the distance and range domain and depends on the user-provided
;    standard deviations in both of these domains.  This function is capable
;    of smoothing the provided image while maintaing edge detail.
;
;       (Source: C. Tomasi and R. Manduchi, "Bilateral filtering for gray 
;                and color images", Proceedings of the 1998 IEEE International
;                Conference on Computer Vision, Bombay, India)
;
; :CATEGORY:
;    Image Processing.
;
; :CALLING SEQUENCE:
;    Result = BILATERAL( image, distanceDeviation, rangeDeviation
;                        [,NUMBER_DEVIATIONS=number_deviations],
;                        [,/USE_LAB],
;                        [,/EDGE_ZERO | ,/EDGE_TRUNCATE | ,/EDGE_WRAP] )
;
; :INPUTS:
;    image
;       A 2-dimensional (greyscale) or 3-dimensional (multiband/color) array
;       containing the image data to be filtered.
;    distanceDeviation
;       A scalar or vector of standard deviations to be used to define the
;       spatial portion of the bilateral filter.  If a vector is provided,
;       an iterative application of this filter is applied.
;    rangeDeviation
;       A scalar or vector of standard deviations to be used to define the
;       range portion of the bilateral filter.  If a vector is provided,
;       an iterative application of this filter is applied.
;
;       NOTE: The number of elements provided for the distanceDeviation and
;             the rangeDeviation parameters must be the same.
;
; :KEYWORD PARAMETRS:
;    NUMBER_DEVIATIONS
;       An optional parameter naming a variable containing the number of standard
;       deviations that should be used when defining the distance filter's
;       width (the default value is 3).
;    USE_LAB
;       An optional parameter indicating that the range distance computations
;       should take place in the CIE L*a*b* color space (rather than in
;       the raw digital count space).  This is believed to provide a better
;       result as the distance in this space correspond closely to perceptual
;       color differences.
;    EDGE_ZERO
;       An optional parameter indicating that the provided image should be
;       zero-padded to allow the output response to be the same size as the
;       input.
;    EDGE_TRUNCATE
;       An optional parameter indicating that the provided image should be
;       padded with the outlining columns/rows to allow the output response
;       to be the same size as the input.
;    EDGE_WRAP
;       An optional parameter indicating that the provided image should be
;       wrapped to allow the output response to be the same size as the input.
;
; :RETURN VALUE:
;    Result is a greyscale or multiband image array (matching the input
;    image) containing the bilateral filter response surface produced using
;    the user-provided distance and range standard deviations.
;
; :SIDE EFFECTS:
;    The resulting response image will have a black border unless an optional
;    edge treatment is specified.
;
; :REQUIRES:
;    GAUSSIAN
;    SRGB2LAB_IMAGE
;    SRGB2XYZ
;    XYZ2LAB
;    LAB2SRGB_IMAGE
;    LAB2XYZ
;    XYZ2SRGB
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    February, 2010    Original code
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

FUNCTION BILATERAL, image, $
                    distanceDeviation, $
                    rangeDeviation, $
                    NUMBER_DEVIATIONS=number_deviations, $
                    USE_LAB=use_lab, $
                    EDGE_ZERO=edge_zero, $
                    EDGE_TRUNCATE=edge_truncate, $
                    EDGE_WRAP=edge_wrap, $
                    MOVIE_ROW=movie_row

;+
; Check if the user provided the number of standard deviations with which
; to determine the width of the distance filter (if none is provided, the
; default value is set to 3).
;-
   IF NOT KEYWORD_SET( number_deviations ) THEN number_deviations = 3

;+
; Check if the user provided multiple edge treatments, and if so, report
; an error and halt execution.
;-
   numberTreatments = 0
   IF KEYWORD_SET( edge_zero ) THEN numberTreatments += 1
   IF KEYWORD_SET( edge_truncate ) THEN numberTreatments += 1
   IF KEYWORD_SET( edge_wrap ) THEN numberTreatments += 1
   IF ( numberTreatments GT 1 ) THEN BEGIN
      MESSAGE, "Multiple edge treatments cannot be specifed"
   ENDIF

;+
; Check if the user provided equal numbers of distance and range standard
; deviations, and if not, report an error and halt execution.
;-
   IF ( N_ELEMENTS( distanceDeviation ) NE N_ELEMENTS( rangeDeviation ) ) THEN BEGIN
      MESSAGE, "The same number of distance and range standard deviations must be provided"
   ENDIF

;+
; Determine the number of dimensions in the provided image array. Either two (2)
; or three (3), for greyscale or multiband, are valid.  If the number is valid 
; then set up the dimensional variables for the rest of the function, otherwise,
; report and error and halt execution.
;-
   numberDimensions = SIZE( image, /N_DIMENSIONS )
   dimension = SIZE( image, /DIMENSIONS )
   CASE numberDimensions OF
      2: BEGIN
            imageBands = 1
            imageDimension = [ dimension[0], dimension[1] ]
         END
      3: BEGIN
            imageBands = dimension[0]
            imageDimension = [ dimension[1], dimension[2] ]
         END
      ELSE: $
         BEGIN
            MESSAGE, "Supplied array has an inappropriate number of dimensions"
         END
   ENDCASE

;+
; Check that if the user has requested the use of the CIE L*a*b* color space
; for the range distance computation, that the provided imgae is a 3-band
; color image.  If not, report an error and halt execution. 
;-
   IF ( KEYWORD_SET( use_lab ) AND (imageBands NE 3) ) THEN BEGIN
      MESSAGE, "Supplied array must be 3-band color ([3,*,*]) to use L*a*b* color space option"
   ENDIF

;+
; Compute the maximum dimension that the distance filter will take on from
; the provided standard deviation for the distance filter and the number
; of standard deviations to consider when constructing the filter.
;-
   filterRadius = number_deviations * MAX( distanceDeviation )
   filterDimension = 2 * filterRadius + 1

;+
; Check if the filter dimension is larger than either of the dimensions of the
; provided image, if so, report an error and halt execution.
;-
   IF ( (filterDimension GT imageDimension[0]) OR $
        (filterDimension GT imageDimension[1]) ) THEN BEGIN
      MESSAGE, "Computed filter dimension cannot be larger than the supplied array dimension"
   ENDIF

;+
; Determine the data type of the provided image so that it can be used later to
; make sure that the computed response image returned has the same data type as
; the input.
;-
   imageType = SIZE( image, /TYPE )

;+
; Set up the filtering iteration loop
;-
   iterationImage = image
   numberIterations = N_ELEMENTS( distanceDeviation )

   FOR iteration = 0, numberIterations-1 DO BEGIN

;+
; Create a padded image array to treat edge effects and place the provided
; image in the "center" of this array.  This will be set for the no treatment
; or "zero padding" option.
;-
      paddedImage = MAKE_ARRAY( imageBands, $
                                imageDimension[0] + 2*filterRadius, $
                                imageDimension[1] + 2*filterRadius, $
                                TYPE=imageType )
      paddedImage[*, $
                  filterRadius:imageDimension[0]+filterRadius-1, $
                  filterRadius:imageDimension[1]+filterRadius-1] = iterationImage

;+
; If the "truncate" edge treatment was specifed, replicate the first and last
; column of pixels in the original image out to the edge of the padded image
; followed by a replication of the top and bottom row (including the replicated
; column data) to the edges of the padded image.
;-
      IF KEYWORD_SET( edge_truncate ) THEN BEGIN
         FOR col = 0, filterRadius-1 DO BEGIN
            paddedImage[*,col,*] = paddedImage[*,filterRadius,*]
         ENDFOR
         FOR col = imageDimension[0]+filterRadius, imageDimension[0]+2*filterRadius-1 DO BEGIN
            paddedImage[*,col,*] = paddedImage[*,imageDimension[0]+filterRadius-1,*]
         ENDFOR
         FOR row = 0, filterRadius-1 DO BEGIN
            paddedImage[*,*,row] = paddedImage[*,*,filterRadius]
         ENDFOR
         FOR row = imageDimension[1]+filterRadius, imageDimension[1]+2*filterRadius-1 DO BEGIN
            paddedImage[*,*,row] = paddedImage[*,*,imageDimension[1]+filterRadius-1]
         ENDFOR
      ENDIF

;+
; If the "wrap" edge treatment was specifed, fill in the columns of the padding area 
; with data from the opposite sides of the image.  Repeat this for the rows of the
; padding area from the top and bottom of the image.
;-
      IF KEYWORD_SET( edge_wrap ) THEN BEGIN
         FOR col = 0, filterRadius-1 DO BEGIN
            paddedImage[*,col,*] = paddedImage[*,col+imageDimension[0],*]
         ENDFOR
         FOR col = imageDimension[0]+filterRadius, imageDimension[0]+2*filterRadius-1 DO BEGIN
            paddedImage[*,col,*] = paddedImage[*,col-imageDimension[0],*]
         ENDFOR
         FOR row = 0, filterRadius-1 DO BEGIN
            paddedImage[*,*,row] = paddedImage[*,*,row+imageDimension[1]]
         ENDFOR
         FOR row = imageDimension[1]+filterRadius, imageDimension[1]+2*filterRadius-1 DO BEGIN
            paddedImage[*,*,row] = paddedImage[*,*,row-imageDimension[1]]
         ENDFOR
      ENDIF

;+
; Determine the starting location (col,row) for the filtering operation.  This is
; dependent on whether edge treatment is specified or not.  If no edge treatment
; is specifed, the response will be smaller than the original image with black
; pixels in the border/non-filtered area.
;-
      IF ( KEYWORD_SET( edge_zero ) OR $
           KEYWORD_SET( edge_truncate ) OR $
           KEYWORD_SET( edge_wrap ) ) THEN BEGIN
         startCol = filterRadius
         endCol = imageDimension[0]+filterRadius-1
         startRow = filterRadius
         endRow = imageDimension[1]+filterRadius-1
      ENDIF ELSE BEGIN
         startCol = 2*filterRadius
         endCol = imageDimension[0]-1
         startRow = 2*filterRadius
         endRow = imageDimension[1]-1
      ENDELSE

;+
; Create the response image array with the same data type and dimensions as the
; provided input image.
;-
      response = MAKE_ARRAY( imageBands, $
                             imageDimension[0], $
                             imageDimension[1], $
                             TYPE=imageType )

;+
; Compute the distance/spatial portion of the bilateral filter using a Gaussian
; distribution and the standard deviation provided.  This portion of the filter
; is constant for the entire iteration.
;-
      distanceFilter = SHIFT( DIST( filterDimension, filterDimension ), filterRadius, filterRadius )
      distanceFilter = GAUSSIAN( distanceFilter, 0, distanceDeviation[iteration], /UNSCALED )

;+
; Create an array, the same size as the distance filter, to hold the center pixel's 
; digital count value from the image/color space transform for the range distance 
; calculation.
;-
      centerValue = DBLARR( imageBands, filterDimension, filterDimension )

;+
; Create the "space" in which to carry out the range distance calculations.  This
; will be either the
;    1) original digital count space
;    2) CIE L*a*b* space
;-
      IF KEYWORD_SET( use_lab ) THEN BEGIN
         rangeSpace = SRGB2LAB_IMAGE( paddedImage )
      ENDIF ELSE BEGIN
         rangeSpace = paddedImage
      ENDELSE

;+
; Move through the image to apply the non-linear filter.
;-
      FOR row = startRow, endRow DO BEGIN
         FOR col = startCol, endCol DO BEGIN

;+
; Select the center value from the range space currently being operated
; on and replicate it throughout the array to use in the range difference 
; computation.
;-
            FOR band = 0, imageBands-1 DO BEGIN
               centerValue[band,*,*] = rangeSpace[band,col,row]
            ENDFOR

;+
; Extract a spatial subimage from the range space to use in the range
; difference computation.
;-
            subRange = rangeSpace[*, $
                                  col-filterRadius:col+filterRadius, $
                                  row-filterRadius:row+filterRadius]

;+
; Compute the range portion of the bilateral filter using a Gaussian
; distribution and the standard deviation provided.  This portion of the filter
; is variable and dependent on the image content in the current subimage.
;-
            rangeFilter = SQRT( TOTAL( (DOUBLE( subRange ) - centerValue)^2, 1 ) )
            rangeFilter = GAUSSIAN( rangeFilter, 0, rangeDeviation[iteration], /UNSCALED )

;+
; Determine the current bilateral filter and the corresponding normalization
; factor for the current subimage.
;-
            bilateralFilter = distanceFilter * rangeFilter
            normalizationFactor = TOTAL( bilateralFilter )

;+
; Create a movie of the non-linear filters produced for each pixel location
; across a specified row of the image being filtered.
;-
   IF KEYWORD_SET( movie_row ) THEN BEGIN
      IF ( row EQ (movie_row+startRow) ) THEN BEGIN
         IF ( col EQ startCol ) THEN BEGIN
            WINDOW, !D.WINDOW+1, XSIZE=640, YSIZE=480
            WINDOW, !D.WINDOW+1, XSIZE=640, YSIZE=480
            WINDOW, !D.WINDOW+1, XSIZE=640, YSIZE=480
            WINDOW, !D.WINDOW+1, XSIZE=1620, YSIZE=480
         ENDIF
         currentWindow = !D.WINDOW
         WSET, currentWindow - 3
         SHADE_SURF, distanceFilter, $
                     XRANGE=[-2,32], $
                     XSTYLE=1, $
                     YRANGE=[-2,32], $
                     YSTYLE=1, $
                     ZRANGE=[0,1], $
                     ZSTYLE=1, $
                     COLOR='000000'x, $
                     BACKGROUND='ffffff'x, $
                     CHARSIZE=2, $
                     TITLE='Distance Filter', $
                     SUBTITLE='(' + STRCOMPRESS( STRING( col-startCol, FORMAT='(i03)' ) ) + $
                              ',' + STRCOMPRESS( STRING( movie_row, FORMAT='(i03)' ) ) + ')'
         d = TVRD()
         d = d[50:589,*]
         WSET, currentWindow - 2
         SHADE_SURF, rangeFilter, $
                     XRANGE=[-2,32], $
                     XSTYLE=1, $
                     YRANGE=[-2,32], $
                     YSTYLE=1, $
                     ZRANGE=[0,1], $
                     ZSTYLE=1, $
                     COLOR='000000'x, $
                     BACKGROUND='ffffff'x, $
                     CHARSIZE=2, $
                     TITLE='Range Filter', $
                     SUBTITLE='(' + STRCOMPRESS( STRING( col-startCol, FORMAT='(i03)' ) ) + $
                              ',' + STRCOMPRESS( STRING( movie_row, FORMAT='(i03)' ) ) + ')'
         r = TVRD()
         r = r[50:589,*]
         WSET, currentWindow - 1
         SHADE_SURF, bilateralFilter, $
                     XRANGE=[-2,32], $
                     XSTYLE=1, $
                     YRANGE=[-2,32], $
                     YSTYLE=1, $
                     ZRANGE=[0,1], $
                     ZSTYLE=1, $
                     COLOR='000000'x, $
                     BACKGROUND='ffffff'x, $
                     CHARSIZE=2, $
                     TITLE='Bilateral Filter', $
                     SUBTITLE='(' + STRCOMPRESS( STRING( col-startCol, FORMAT='(i03)' ) ) + $
                              ',' + STRCOMPRESS( STRING( movie_row, FORMAT='(i03)' ) ) + ')'
         b = TVRD()
         b = b[50:589,*]
         WSET, currentWindow
         TV, d, 0
         TV, r, 1
         TV, b, 2
         frame = TVRD()
         systimeArray = STRSPLIT( SYSTIME(), /EXTRACT )
         yearString = systimeArray[4]
         dayString = STRMID( '0' + systimeArray[2], 1, /REVERSE_OFFSET )
         CASE systimeArray[1] OF
            'Jan': monthString = '01'
            'Feb': monthString = '02'
            'Mar': monthString = '03'
            'Apr': monthString = '04'
            'May': monthString = '05'
            'Jun': monthString = '06'
            'Jul': monthString = '07'
            'Aug': monthString = '08'
            'Sep': monthString = '09'
            'Oct': monthString = '10'
            'Nov': monthString = '11'
            'Dec': monthString = '12'
         ENDCASE
         movieDirectory = '/Users/cnspci/Movies/IDL/' + yearString + monthString + dayString + '/'
         IF NOT FILE_TEST( movieDirectory, /DIRECTORY ) THEN FILE_MKDIR, movieDirectory
         WRITE_PNG, movieDirectory + $
                    STRCOMPRESS( STRING( col, FORMAT='(i04)' ), /REMOVE_ALL ) + '.png', frame
      ENDIF
   ENDIF

;+
; Extract a spatial subimage from the original image space to use in the 
; filtering computation.
;-
            subImage = paddedImage[*, $
                                   col-filterRadius:col+filterRadius, $
                                   row-filterRadius:row+filterRadius]

;+
; Carry out the bilateral filtering for each band of the original image.
;-
            FOR band = 0, imageBands-1 DO BEGIN
               response[band,col-filterRadius,row-filterRadius] = $
                  TOTAL( subImage[band,*,*] * bilateralFilter ) / normalizationFactor
            ENDFOR

         ENDFOR
      ENDFOR

;+
; Update the image to be filtered with the current response for the next 
; filtering iteration.
;-
      iterationImage = response

   ENDFOR

;+
; Return the response image to the calling routine (eliminating any single
; element dimensions from the response array).
;-
   RETURN, REFORM( response )

END