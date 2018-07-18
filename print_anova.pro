;+
; :NAME:
;    PRINT_ANOVA
;
; :PURPOSE:
;    This procedure will display the ANOVA table given in the analyis 
;    structure along with the coefficients computed by the 
;    MULTIPLE_LINEAR_REGRESSION module.
;
; :CATEGORY:
;    Statistics.
;
; :CALLING SEQUENCE:
;    PRINT_ANOVA, coefficients, analysis
;
; :INPUTS:
;    coefficients
;       A vector containing the regression coefficients produced by the
;       MULTIPLE_LINEAR_REGRESSION routine.
;    analysis
;       A structure containing the ANOVA table elements produced by the
;       MULTIPLE_LINEAR_REGRESSION routine.
;
; :KEYWORD PARAMETERS:
;    None
;
; :SIDE EFFECTS:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
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

PRO PRINT_ANOVA, coefficients, analysis

   PRINT, ''
   PRINT, '                              ANALYSIS OF VARIANCE'
   PRINT, '-------------------------------------------------------------------------------'
   PRINT, '  SOURCE             DF        SS            MS            F-VALUE      Pr > F'
   PRINT, '-------------------------------------------------------------------------------'
   PRINT, 'Model', $
          analysis.anova.dofRegression, $
          analysis.anova.ssRegression, $
          analysis.anova.msRegression, $
          analysis.anova.F, $
          analysis.anova.pValue, $
          FORMAT='(a8, i16, f14.5, f14.5, f14.2, f12.4)'
   PRINT, 'Error', $
          analysis.anova.dofResidual, $
          analysis.anova.ssResidual, $
          analysis.anova.msResidual, $
          FORMAT='(a8, i16, f14.5, f14.5)'
   PRINT, 'Total', $
          analysis.anova.dofTotal, $
          analysis.anova.ssTotal, $
          analysis.anova.msTotal, $
          FORMAT='(a8, i16, f14.5, f14.5)'
   PRINT, '-------------------------------------------------------------------------------'
   PRINT, ''
   PRINT, 'R-Squared', $
          analysis.anova.rSquared, $
          FORMAT='(a20, f13.4)'
   PRINT, ''
   PRINT, ''
   PRINT, '                              PARAMETER ESTIMATES'
   PRINT, '-------------------------------------------------------------------------------'
   PRINT, '                       PARAMETER        STANDARD'
   PRINT, 'Variable      DF        ESTIMATE          ERROR       t-VALUE        Pr > |t|'
   PRINT, '-------------------------------------------------------------------------------'
   FOR i = 0, N_ELEMENTS( coefficients )-1 DO BEGIN
      PRINT, i+1, $
             1, $
             coefficients[i], $
             analysis.standardErrors[i], $
             analysis.tValues[i], $
             analysis.pValues[i], $
             FORMAT='(i8, i8, f16.5, f16.5, f13.2, f15.4)'
   ENDFOR
   PRINT, '-------------------------------------------------------------------------------'

END