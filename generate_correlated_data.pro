;+
; :NAME:
;    GENERATE_CORRELATED_DATA
;
; :PURPOSE:
;    This function will return a list of k normally-distributed,
;    correlated, n-dimensional random points [with a specified
;    mean vector and set of univariate standard deviations].
;
; :CATEGORY:
;    Statistics.
;
; :CALLING SEQUENCE:
;    Result = $
;       GENERATE_CORRELATED_DATA, seed,
;                                 numberPoints,
;                                 correlationMatrix,
;                                 MEAN_VECTOR=meanVector,
;                                 STANDARD_DEVIATION=standardDeviation,
;
; :INPUTS:
;    seed
;       A variable or constant (long integer) used to initialize
;       the random sequence on input, and in which the state of the
;       random number generator is saved on output.
;    numberPoints
;       A variable or constant containing the number of n-dimensional
;       values to be produced.
;    correlationMatrix
;       A two-dimensional matrix describing the correlation to be
;       exhibited between variables.  This matrix will also define
;       the dimensionality of the produced data set (for example, a
;       3x3 correlation matrix will cause 3-dimensional data to be
;       generated).
;
; :KEYWORD PARAMETERS:
;    MEAN_VECTOR
;       A vector defining the mean for each dimension of the generated
;       data set.  If omitted, the mean vector will be filled with 0's.
;    STANDARD_DEVIATION
;       A vector defining the standard deviation for each dimension of
;       the generated data set.  If omitted, the standard deviation vector
;       will be filled with 1's.
;
; :RETURN VALUE:
;    A matrix containing multi-dimensional, normally-distributed,
;    correlated random values.  The random values will be contained
;    in the rows of the returned matrix.
;    
;    If the dimensions of the provided descriptive statistics are
;    incompatible, a scaler value of -1 will be returned.
;    
;    If the dimensions of the data set, as determined from the 
;    provided correlation matrix is less than 2, then a scalar
;    value of -2 will be returned.
;    
; :SIDE EFFECTS:
;    If the seed is specifed as any data type besides a long integer, it
;    will be truncated and changed to this data type.
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    April, 2009       Original code
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

FUNCTION GENERATE_CORRELATED_DATA, seed, $
                                   numberPoints, $
                                   correlationMatrix, $
                                   MEAN_VECTOR=meanVector, $
                                   STANDARD_DEVIATION=standardDeviation

;+
; Determine the dimensionality of the points to be produced from
; the provided correlation matrix
;-
   numberDimensions = SQRT( N_ELEMENTS( correlationMatrix ) )

;+
; Determine if optional parameters were specified, and if not,
; insert default values
;-
   IF NOT KEYWORD_SET( meanVector ) THEN BEGIN
      meanVector = REPLICATE( 0D, numberDimensions )
   ENDIF
   numberMeans = N_ELEMENTS( meanVector )

   IF NOT KEYWORD_SET( standardDeviation ) THEN BEGIN
      standardDeviation = REPLICATE( 1D, numberDimensions )
   ENDIF
   numberStandardDeviations = N_ELEMENTS( standardDeviation )

;+
; Make sure that the dimensions of the provided correlation
; matrix and the provided mean and standard deviation vectors
; are compatible
;-
   IF ( numberMeans NE numberStandardDeviations ) OR $
      ( numberStandardDeviations NE numberDimensions ) OR $
      ( numberDimensions NE numberMeans ) THEN BEGIN
      PRINT, ""
      PRINT, "%GENERATE_CORRELATED_DATA: mean/standard deviation/correlation dimensions incompatable"
      PRINT, ""
      RETURN, -1
   ENDIF

   IF ( numberDimensions LT 2 ) THEN BEGIN
      PRINT, ""
      PRINT, "%GENERATE_CORRELATED_DATA: Must have statistics for at least 2 dimensions"
      PRINT, ""
      RETURN, -2
   ENDIF

;+
; Generated a numberDimensions-by-numberPoints matrix of unit normal
; random values.
;-
   seed = LONG( seed )
   data = RANDOMN( seed, numberDimensions, numberPoints, /DOUBLE )

;+
; Perform a Cholesky decomposition of the upper-triangular form
; of the provided correlation matrix
;-
   u = DBLARR( numberDimensions, numberDimensions )
   FOR j = 0, numberDimensions-1 DO BEGIN
      FOR i = j, numberDimensions-1 DO BEGIN
         u[i,j] = correlationMatrix[i,j]
      ENDFOR
   ENDFOR
   LA_CHOLDC, u, /UPPER, /DOUBLE

;+
; Using the Cholesky decomposition of the correlation matrix,
; transform the previously generated unit normal random values
; to a correlated data set (based on the provided correlation
; matrix)
;-
   correlatedData = data ## u

;+
; Modify the unit normal correlated random values to exhibit the
; mean and standard deviation vectors provided
;-
   gain = TRANSPOSE( REPLICATE( 1, numberPoints ) ) ## standardDeviation
   offset = TRANSPOSE( REPLICATE( 1, numberPoints ) ) ## meanVector
   correlatedData = correlatedData * gain + offset

;+
; Return the generated data set to the calling routine
;-
   RETURN, correlatedData

END