;+
; :NAME:
;    PRINT_LATEX_TABLE
;
; :PURPOSE:
;    This procedure will create LaTeX code for a table from a provided
;    two-dimensional array.
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    PRINT_LATEX_TABLE, var, FORMAT=format
;
; :INPUTS:
;    var
;       A two-dimensional array containing the data to be placed in a
;       LaTeX tabular element. 
;
; :KEYWORD PARAMETRS:
;    FORMAT
;       A keyword parameter set equal to a scalar string or an array of
;       strings containing FORTRAN-style specification(s) for the format
;       to be used for every or each column in the produced table,
;       respectively.  Some valid examples are
;          FORMAT='i7'
;          FORMAT=['f10.3','f7.2','f12.4']
;
; :RETURN VALUE:
;    None
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    September, 2010   Original code
;    September, 2011   Added optional support for FORTRAN-style format
;                      specification
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

PRO PRINT_LATEX_TABLE, var, FORMAT=format

   dimensions = SIZE( var, /DIMENSIONS )

   tableFormat = REPLICATE( '', dimensions[0] )
   IF KEYWORD_SET( format ) THEN BEGIN
      IF ( (N_ELEMENTS( format ) NE 1) AND (N_ELEMENTS( format ) NE dimensions[0]) ) THEN BEGIN
         MESSAGE, "Length of FORMAT string does not match the number of columns in the provided array", LEVEL=0
      ENDIF
      IF ( N_ELEMENTS( format ) EQ 1 ) THEN BEGIN
         FOR column = 0, dimensions[0]-1 DO tableFormat[column] = '(' + format + ')'
      ENDIF ELSE BEGIN
         FOR column = 0, dimensions[0]-1 DO tableFormat[column] = '(' + format[column] + ')'
      ENDELSE
   ENDIF

   latexFormat = '{'
   FOR column = 0, dimensions[0]-1 DO latexFormat = latexFormat + ' c'
   latexFormat = latexFormat + ' }

   PRINT, '\begin{tabular}' + latexFormat

   FOR row = 0, dimensions[1]-1 DO BEGIN
      currentRow = STRCOMPRESS( STRING( var[0,row], FORMAT=tableFormat[0], /PRINT ), /REMOVE_ALL )
      FOR column = 1, dimensions[0]-1 DO BEGIN
         currentRow = currentRow + ' & '
         currentRow = currentRow + $
                      STRCOMPRESS( STRING( var[column,row], FORMAT=tableFormat[column], /PRINT ), /REMOVE_ALL )
      ENDFOR
      currentRow = currentRow + ' \\'
      PRINT, currentRow
   ENDFOR

   PRINT, '\end{tabular}'

END