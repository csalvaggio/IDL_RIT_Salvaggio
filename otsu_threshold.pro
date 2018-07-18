;+
; :NAME:
;    OTSU_THRESHOLD
;
; :PURPOSE:
;    This function uses Otsu's method to automatically perform histogram shape-based
;    image thresholding, or, the reduction of a gray-level image to a binary image.
;    The algorithm assumes that the image to be thresholded contains two classes of
;    pixels (e.g. foreground and background) then calculates the optimum threshold
;    separating those two classes so that their combined spread (inter-class variance)
;    is maximal.
;
; :CATEGORY:
;    Image Processing.
;
; :CALLING SEQUENCE:
;    Result = OTSU_THRESHOLD( image [, THRESHOLDED_IMAGE=thresholdedImage] [, /VERBOSE] )
;
; :INPUTS:
;    image
;       A two-dimensional greyscale image array containing the data to be thresholded.
;
; :KEYWORD PARAMETRS:
;    THRESHOLDED_IMAGE
;       An optional parameter specifying a named variable to contain the binary image 
;       computed using the determined threshold.
;    VERBOSE
;       An optional parameter that if set will cause a plot of the image histogram
;       depicting the threshold value to be displayed.
;
; :RETURN VALUE:
;    Result is a scalar containing the determined threshold.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    April, 2011       Original code
;    October, 2012     Vectorized to run significantly faster (using the 
;                      recommended modifications provided by Gianguido Cianci)
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

FUNCTION OTSU_THRESHOLD, image, THRESHOLDED_IMAGE=thresholdedImage, VERBOSE=verbose

;+
; Turn off accumulated exception reporting to dismiss exceptions caused by
; empty histogram bins in division operations
;-
   !EXCEPT=0

;+
; Compute the image histogram and probability denisty function
;-
   minimumDC = MIN( image )
   h = HISTOGRAM( image - minimumDC, OMIN=minDC, OMAX=maxDC, BINSIZE=1 )
   pdf = h / TOTAL( h, /DOUBLE )
   reversedPDF = REVERSE( pdf )

;+
; Iterate through all possible thresholds and compute the interclass variance
; at each level
;-
   cdf = TOTAL( pdf, /DOUBLE, /CUMULATIVE )
   omega1 = cdf[0:N_ELEMENTS(pdf)-2]
   mu1 = TOTAL( pdf * INDGEN(maxDC), /DOUBLE, /CUMULATIVE ) / cdf
   omega2 = (REVERSE( TOTAL( reversedPDF, /DOUBLE, /CUMULATIVE ) ))[1:*]
   mu2 = REVERSE( TOTAL( reversedPDF * ( maxDC - INDGEN( maxDC ) ), /DOUBLE, /CUMULATIVE ) ) / omega2
   interclassVariance = [ 0, omega1 * omega2 * ( mu1 - mu2 )^2 ]

;+
; Determine the threshold by finding the level at which the maximum interclass
; variance occurs
;-
   maximumVariance = MAX( interclassVariance, threshold )
   threshold = threshold + minimumDC

;+
; Compute the binary image using the determined threshold value
;-
   thresholdedImage = image GT threshold

;+
; Plot the image histogram and display the determined threshold
;-
   IF KEYWORD_SET( verbose ) THEN BEGIN
      p1 = PLOT( INDGEN( maxDC+1 ), [ REPLICATE( 0, minimumDC ), h ], XSTYLE=1 )
      p2 = PLOT( [threshold,threshold+0.00000001], [0,MAX( h )], /OVERPLOT )
      p2.COLOR = 'red'
      p1.XTITLE = 'Digital Count (DC)'
      p1.YTITLE = 'Number of Pixels'
   ENDIF

;+
; Return the threshold to the calling routine
;-
   RETURN, threshold

END