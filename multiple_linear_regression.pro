;+
; :NAME:
;    MULTIPLE_LINEAR_REGRESSION
;
; :PURPOSE:
;    This function will perform a full-order multiple linear regression
;    by constructing a full-order design matrix including all possible
;    combinations of the provided independent variables (for example, if
;    5 independent variables are provided and a constant/intercept term is
;    desired for a third-order model (order=3), then all 6-CHOOSE-3 three-
;    term variable combinations will be placed in the design matrix).
;
; :CATEGORY:
;    Statistics.
;
; :CALLING SEQUENCE:
;    Result = $
;       MULTIPLE_LINEAR_REGRESSION( independentData, $
;                                   dependentData, $
;                                   order, $
;                                   [NO_INTERCEPT=no_intercept], $
;                                   [DESIGN_MATRIX=design_matrix], $
;                                   [ANALYSIS=analysis], $
;                                   [SIGNIFICANCE_LEVEL=significance_level]
;
; :INPUTS:
;    independentData
;       A "number of independent variables" by "number of observations" array or
;       a "number of observations" vector of independent variable values.
;    dependentData
;       A "number of observations" vector of dependent variable points.
;    order
;       The order of the model to be constructed.  If a constant/intercept 
;       term is desired, the design matrix will be "number-of-variables"+1-
;       CHOOSE-order by "number of observations" in size.
;
; :KEYWORD PARAMETERS:
;    NO_INTERCEPT
;       This keyword will cause a no-intercept regression to be carried out.
;    DESIGN_MATRIX
;       This keyword will cause the named variable to contain the constructed
;       design matrix upon return.
;    ANALYSIS
;       This keyword will cause the named variable to contain a structure upon
;       return that contains the regression analysis terms including:
;          ANOVA
;             A structure containing the elements of a multiple regression
;             ANOVA table, including
;                SSREGRESSION
;                   The regression sum of squared error.
;                SSRESIDUAL
;                   The residual sum of squared error.
;                SSTOTAL
;                   The total sum of squared error.
;                DOFREGRESSION
;                   Regression degrees of freedom (p)
;                DOFRESIDUAL
;                   Residual degrees of freedom (n-p)
;                DOFTOTAL
;                   Total degrees of freedom (n-1)
;                MSREGRESSION
;                   The regression mean squared error.
;                MSRESIDUAL
;                   The residual mean squared error.
;                MSTOTAL
;                   The total mean squared error.
;                F
;                   The F-statistic used to determine the significance of
;                   the regression (MSREGRESSION / MSRESIDUAL)
;                PVALUE
;                   The probability of exceeding the F-statistic for this
;                   regression (if the p-Value is less than the significance
;                   level, then the null hypothesis that all coefficients are
;                   0 can be rejected and the regression is considered
;                   significant)
;                RSQUARED
;                   The coefficient of determination.
;          STANDARDERRORS
;             A vector containing the standard error for each coefficient determined
;             for the regression for using in significance testing.
;          TVALUES
;             A vector containing the t-value for each coefficient determined
;             for the regression for using in significance testing.
;          PVALUES
;             A vector containing the p-value for each coefficient determined
;             for the regression for using in significance testing.
;          COEFFICIENTSSIGNIFICANT
;             A vector containing values of 1 for each coefficient that was
;             determined to be significant at a level of significance provided
;             by the calling routine, and 0 otherwise.
;    SIGNIFICANCE_LEVEL
;       The named variable/constant contains the two-tailed level of significance
;       at which the coefficients are evaluated (if not provided, a value of 0.05
;       is used by default).
;
; :RETURN VALUE:
;    A "number of variables"+1-CHOOSE-order element vector containing the 
;    multiple linear regression coefficients for the constructed design matrix.
;     
; :SIDE EFFECTS:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;                      Philip Salvaggio
;    June, 2010        Original code
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

PRO GENERATE_COMBINATIONS_RECURSIVE, numberToChoose, $
                                     outOf, $
                                     recursionDepth, $
                                     recursiveElementPath, $
                                     currentElement, $
                                     currentCombinations

   IF ( recursionDepth EQ numberToChoose ) THEN BEGIN
      IF ( numberToChoose EQ 1 ) THEN BEGIN
         currentCombinations = [ currentCombinations, currentElement ]
      ENDIF ELSE BEGIN
         currentCombinations = [ currentCombinations, recursiveElementPath, currentElement ]
      ENDELSE
      RETURN
   ENDIF

   previousRecursiveElementPath = recursiveElementPath
   IF ( recursiveElementPath[0] EQ -1 ) THEN BEGIN
      recursiveElementPath[0] = currentElement
   ENDIF ELSE BEGIN
      recursiveElementPath = [ recursiveElementPath, currentElement ]
   ENDELSE

   FOR element = currentElement, outOf-1 DO BEGIN
      GENERATE_COMBINATIONS_RECURSIVE, numberToChoose, $
                                       outOf, $
                                       recursionDepth+1, $
                                       recursiveElementPath, $
                                       element, $
                                       currentCombinations
   ENDFOR

   recursiveElementPath = previousRecursiveElementPath

END  



FUNCTION GENERATE_COMBINATIONS, numberToChoose, outOf

   recursiveElementPath = [-1]
   currentCombinations = [-1]

   FOR startingElement=0, outOf-1 DO BEGIN
      GENERATE_COMBINATIONS_RECURSIVE, numberToChoose, $
                                       outOf, $
                                       1, $
                                       recursiveElementPath, $
                                       startingElement, $
                                       currentCombinations
   ENDFOR

   numberCombinations = LONG( FACTORIAL( outOf + numberToChoose - 1 ) / $
                              FACTORIAL( numberToChoose ) / $
                              FACTORIAL( outOf - 1 ) )
   
   RETURN, REFORM( currentCombinations[1:N_ELEMENTS( currentCombinations )-1], numberToChoose, numberCombinations )

END



FUNCTION BUILD_DESIGN_MATRIX, data, order

   dimensions = SIZE( data, /DIMENSIONS )
   numberObservations = dimensions[0]
   numberVariables = dimensions[1]

   combinationIndices = GENERATE_COMBINATIONS( order, numberVariables )
   numberCombinations = LONG( FACTORIAL( numberVariables + order - 1 ) / $
                              FACTORIAL( order ) / $
                              FACTORIAL( numberVariables - 1 ) )

   designMatrix = -1
   FOR combination=0, numberCombinations-1 DO BEGIN
      dataVector = REPLICATE( 1D, numberObservations )
      FOR variable=0, order-1 DO BEGIN
         dataVector = dataVector * REFORM( data[*,combinationIndices[variable,combination]] )
      ENDFOR
      designMatrix = [ designMatrix, dataVector ]
   ENDFOR
   designMatrix = designMatrix[1:N_ELEMENTS( designMatrix )-1]

   RETURN, REFORM( designMatrix, numberObservations, numberCombinations )

END



FUNCTION MULTIPLE_LINEAR_REGRESSION, independentData, $
                                     dependentData, $
                                     order, $
                                     NO_INTERCEPT=no_intercept, $
                                     DESIGN_MATRIX=design_matrix, $
                                     ANALYSIS=analysis, $
                                     SIGNIFICANCE_LEVEL=significance_level

;+
; Make a local copy of the provided dependent and independent data arrays
; converted to double precision and reorganized so that each independent
; variable is in a row of the local copy
;-
   localDependentData = DOUBLE( dependentData )
   localIndependentData = TRANSPOSE( DOUBLE( independentData ) )

;+
; Determine the number of observations and variables provided
;-
   dimensions = SIZE( localIndependentData, /DIMENSIONS )
   CASE N_ELEMENTS( dimensions ) OF
      1: BEGIN
            numberObservations = dimensions[0]
            numberVariables = 1
         END
      2: BEGIN
            numberObservations = dimensions[0]
            numberVariables = dimensions[1]
         END
      ELSE: MESSAGE, 'Specified independent data array must be either one or two dimensional'
   ENDCASE

;+
; If an intercept/constant term is to be included in the regression, prepend
; a row of ones (1's) to the independent data array to account for this
;-
   IF NOT KEYWORD_SET( no_intercept ) THEN BEGIN
      ones = REPLICATE( 1D, numberObservations )
      localIndependentData = [ ones, REFORM( localIndependentData, numberObservations*numberVariables ) ]
      localIndependentData = REFORM( localIndependentData, numberObservations, numberVariables+1 )
   ENDIF

;+
; Build a design/model matrix for the independent data that contains all
; combinations of the variables for the specified model order
;-
   designMatrix = BUILD_DESIGN_MATRIX( localIndependentData, order )
   design_matrix = TRANSPOSE( designMatrix )

;+
; Check that there are enough observations to support the desired model
;-
   dimensions = SIZE( designMatrix, /DIMENSIONS )
   numberCoefficients = dimensions[1]
   IF ( numberObservations LE numberCoefficients ) THEN MESSAGE, 'Insufficient number of observations to support model'

;+
; Compute the multiple linear least squares regression coefficients if the
; product of the transpose of the design matrix with itself is not singular
;-
   designMatrix_transpose_designMatrix = TRANSPOSE( designMatrix ) # designMatrix
   determinant = LA_DETERM( designMatrix_transpose_designMatrix, /DOUBLE, /CHECK )
   IF ( determinant EQ 0 ) THEN MESSAGE, 'The product of the transpose of the design matrix with itself is singular'
   designMatrix_transpose_designMatrix_inverse = LA_INVERT( designMatrix_transpose_designMatrix, /DOUBLE )

   coefficients = designMatrix_transpose_designMatrix_inverse # TRANSPOSE( designMatrix ) # localDependentData

;+
; Compute the predicted values from the model using the computed coefficients
; and the design matrix that was formed
;-
   predictedDependentData = coefficients # TRANSPOSE( designMatrix )

;+
; Compute the residuals for the provided dependent data and the model predictions
;-
   residuals = localDependentData - predictedDependentData

;+
; Compute the model ANOVA parameters
;-
   ssRegression = TOTAL( ( predictedDependentData - MEAN( localDependentData, /DOUBLE ) )^2, /DOUBLE )
   ssResidual = TOTAL( residuals^2, /DOUBLE )
   ssTotal = TOTAL( ( localDependentData - MEAN( localDependentData, /DOUBLE ) )^2, /DOUBLE )
   rSquared = 1D - ssResidual / ssTotal

   dofRegression = numberCoefficients
   dofResidual = numberObservations - numberCoefficients
   dofTotal = numberObservations - 1

   msRegression = ssRegression / dofRegression
   msResidual = ssResidual / dofResidual
   msTotal = ssTotal / dofTotal

   F = msRegression / msResidual

   pValue = 1D - F_PDF( F, dofRegression, dofResidual )

;+
; Determine the significance of each of the computed model coefficients
; (assuming a 2-tailed Student-T distribution)
;-
   residualVariance = ( TRANSPOSE( residuals ) # residuals ) / dofResidual
   residualCovariance = residualVariance[0] * designMatrix_transpose_designMatrix_inverse

   standardErrors = DBLARR( numberCoefficients )
   FOR i = 0, numberCoefficients-1 DO standardErrors[i] = SQRT( residualCovariance[i,i] )

   tValues = coefficients / standardErrors
   pValues = 2D * ( 1D - T_PDF( ABS( tValues ), dofResidual ) )

   IF NOT KEYWORD_SET( significance_level ) THEN significance_level = 0.05
   index = WHERE( pValues LE significance_level/numberCoefficients, count )
   coefficientsSignificant = LONARR( numberCoefficients )
   IF ( count GT 0 ) THEN coefficientsSignificant[index] = 1

;+
; Construct the structure to be returned to the calling module that will
; contain the regression analysis parameters
;-   
   analysis = { anova:{ ssRegression:ssRegression, $
                        ssResidual:ssResidual, $
                        ssTotal:ssTotal, $
                        dofRegression:dofRegression, $
                        dofResidual:dofResidual, $
                        dofTotal:dofTotal,$
                        msRegression:msRegression, $
                        msResidual:msResidual, $
                        msTotal:msTotal,$
                        F:F, $
                        pValue:pValue, $
                        rSquared:rSquared }, $
                standardErrors:standardErrors, $
                tValues:tValues, $
                pValues:pValues, $
                coefficientsSignificant:coefficientsSignificant }

;+
; Clear the error status on a floating-point underflow, report all other
; floating-point arithmetic errors/warnings
;-
   IF ( CHECK_MATH() EQ 32 ) THEN !ERROR = 0

;+
; Return the regression coefficients to the calling module
;-
   RETURN, coefficients

END