;+
; :NAME:
;    RANSAC
;
; :PURPOSE:
;    RANSAC is an abbreviation for "RANdom SAmple Consensus". It is an 
;    iterative method to estimate parameters of a mathematical model 
;    from a set of observed data which contains outliers. It is a 
;    non-deterministic algorithm in the sense that it produces a 
;    reasonable result only with a certain probability, with this 
;    probability increasing as more iterations are allowed. The 
;    algorithm was first published by Fischler and Bolles in 1981.
;
;    A basic assumption is that the data consists of "inliers", i.e., 
;    data whose distribution can be explained by some set of model 
;    parameters, and "outliers" which are data that do not fit the 
;    model. In addition to this, the data can be subject to noise. 
;    The outliers can come, e.g., from extreme values of the noise 
;    or from erroneous measurements or incorrect hypotheses about the 
;    interpretation of data. RANSAC also assumes that, given a 
;    (usually small) set of inliers, there exists a procedure which 
;    can estimate the parameters of a model that optimally 
;    explains or fits this data (http://en.wikipedia.org/wiki/RANSAC).
;
; :CATEGORY:
;    Statistics.
;
; :CALLING SEQUENCE:
;    Result = $
;       RANSAC( data, $
;               fittingFunction, $
;               distanceFunction, $
;               minimumSamples, $
;               distanceThreshold )
;
; :INPUTS:
;    data
;       Data set for which you are trying to determine a 
;       model.  The data set is an array for which the 
;       number of columns corresponds to the number of 
;       dimensions in the data and the number of rows 
;       represents the number of observations [d,npts].
;    fittingFunction
;       A string representing the name of the fitting 
;       function to be used for the data.  The calling 
;       sequence parameters must be ( data, status ) where 
;       a status of 1 represents a successful fit and a 
;       status of 0 indicates that some error occurred.
;    distanceFunction
;       A string representing the name of the distance 
;       function that determines the inlier points in the 
;       data set (data) that fall within a specified 
;       threshold distance (distanceThreshold) of the 
;       model defined by the fit parameters (modelParameters). 
;       This function returns the row indices of the 
;       provided data array that are inliers for the model.
;    minimumSamples
;       The minimum number of points that are required to 
;       fit the desired model.
;    distanceThreshold
;       The maximum distance a data point may be located 
;       from the determined model to still be considered 
;       an inlier.
;
; :KEYWORD PARAMETERS:
;    MAXIMUM_TRIALS
;       The maximum number of iteratons (default=1000).
;    MAXIMUM_ATTEMPTS
;       The maximum number of attempts that will be made 
;       to fit a non-degenrate model for a particular set 
;       of randomly selected points (default=100).
;    PROBABILITY_ERROR_FREE_SELECTION
;       The desired probability of choosing at least one 
;       sample free from outliers (default=0.99)
;    INLIERS
;       An optionally returned array containing the row indices 
;       corresponding to the rows of the provided data set 
;       that represent the best set of inliers chosen from the 
;       data set.
;    VERBOSE
;       If set, this routine will return diagnostic information.
;    WIDGETID
;       If set, this routine will return diagnostic information
;       to the provided widget element ID using the SET_VALUE
;       parameter and the WIDGET_CONTROL procedure.
;
; :RETURN VALUE:
;    Result is an array containing the parameters of the best fit model
;    determined during the RANSAC process.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    RANDOM_SAMPLE
;
; :REFERENCE:
;     Fischler, Martin A. and Robert C. Bolles, Random sample consensus: 
;     A paradigm for model fitting with aplications to image analysis 
;     and automated cartography, Transactions of ACM - Graphics and 
;     Image Processing, Volume 24, Number 6, June 1981, pp. 381-395.
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    September, 2008   Original code
;    October, 2008     Modified to work with GUI by adding WIDGET parameter
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

FUNCTION RANSAC, data, $
                 fittingFunction, $
                 distanceFunction, $
                 minimumSamples, $
                 distanceThreshold, $
                 MAXIMUM_TRIALS=maximumTrials, $
                 MAXIMUM_ATTEMPTS=maximumAttempts, $
                 PROBABILITY_ERROR_FREE_SELECTION=$
                    probabilityErrorFreeSelection, $
                 INLIERS=bestInliers, $
                 VERBOSE=verbose, $
                 WIDGETID=widgetID
   
;+
; Check if the user provided the runtime parameters (if none are
; provided, the default values are set).
;-
   IF NOT KEYWORD_SET( maximumTrials ) THEN maximumTrials = 1000
   IF NOT KEYWORD_SET( maximumAttempts ) THEN maximumAttempts = 100
   IF NOT KEYWORD_SET( probabilityErrorFreeSelection ) THEN $
      probabilityErrorFreeSelection = 0.99

;+
; Determine the number of independent variables as well as the
; number of points/observations provided
;-
   dimensions = SIZE( data, /DIMENSIONS )
   numberDimensions = dimensions[0]-1
   numberPoints = dimensions[1]

;+
; Initialize the iteration parameters
;-
   trial = 0
   numberTrialsRequired = 1
   bestScore = 0
   seed = SYSTIME( /SECONDS )

;+
; Perform the RANSAC process iterations to identify the best 
; model parameters
;-
   WHILE ( numberTrialsRequired GT trial ) DO BEGIN

      status = 0
      attempt = 0
      WHILE ( status EQ 0 ) DO BEGIN
         randomIndices = RANDOM_SAMPLE( seed, $
                                        numberPoints, $
                                        minimumSamples )

         modelParameters = CALL_FUNCTION( fittingFunction, $
                                          data[*,randomIndices], $
                                          status )
         attempt = attempt + 1
         IF ( attempt GE maximumAttempts ) THEN BREAK
      ENDWHILE

      IF ( status ) THEN BEGIN
         inliers = CALL_FUNCTION( distanceFunction, $
                                  modelParameters, $
                                  data, $
                                  distanceThreshold )

         numberInliers = N_ELEMENTS( inliers )
         IF ( numberInliers GE bestScore ) THEN BEGIN
            bestScore = numberInliers
            bestModelParameters = modelParameters
            bestInliers = inliers

            fractionInliers = DOUBLE( numberInliers ) / $
                              DOUBLE( numberPoints )
            probabilityNoOutliers = 1 - fractionInliers^minimumSamples
            numberTrialsRequired = $
               ALOG( 1 - probabilityErrorFreeSelection ) / $
               ALOG( probabilityNoOutliers )
         ENDIF

         trial = trial + 1

         IF ( trial GT maximumTrials ) THEN BEGIN
            PRINT, "%RANSAC: Reached maximum number of trials"
            RETURN, -1
         ENDIF

;+
; Optionally provide feedback to the console to monitor the RANSAC
; process
;-
         IF KEYWORD_SET( verbose ) THEN BEGIN
            msg = STRING( [10B] )
            msg = msg + "Trial " + $
                        STRCOMPRESS( STRING( trial ), $
                                     /REMOVE_ALL ) +$
                        " of " + $
                        STRCOMPRESS( STRING( numberTrialsRequired ), $
                                     /REMOVE_ALL ) + $
                        " trials required"
            msg = msg + STRING( [10B] )
            msg = msg + "   Random points selected: "
            FOR index = 0, N_ELEMENTS( randomIndices )-1 DO BEGIN
               msg = msg + STRING( randomIndices[index] )
            ENDFOR
            msg = msg + STRING( [10B] )
            msg = msg + "   Model parameters computed using " + $
                        STRCOMPRESS( STRING( attempt ), /REMOVE_ALL ) + $
                        " attempt(s)"
            FOR index = 0, N_ELEMENTS( modelParameters )-1 DO BEGIN
               msg = msg + STRING( modelParameters[index] )
            ENDFOR
            msg = msg + STRING( [10B] )
            msg = msg + "   Indices of inliers: "
            FOR index = 0, N_ELEMENTS( inliers )-1 DO BEGIN
               msg = msg + STRING( inliers[index] )
            ENDFOR
            msg = msg + STRING( [10B] )
            PRINT, msg
            IF KEYWORD_SET( widgetID ) THEN BEGIN
               WIDGET_CONTROL, widgetID, SET_VALUE=msg, /APPEND
            ENDIF
         ENDIF
      ENDIF

   ENDWHILE

;+
; Return the model parameters to the calling routine
;-
   RETURN, bestModelParameters

END