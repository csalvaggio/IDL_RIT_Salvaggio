;+
; :NAME:
;    CREATE_DCT_MATRIX
;
; :PURPOSE:
;    This function computes the DCT matrix that can be used in place of the
;    discrete cosine transform for square, two-dimensional block data.  This
;    matrix, M, is used as Y = M ## X ## M' to compute the DCT, Y, of the 
;    square, two-dimensional data array X.
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    Result = CREATE_DCT_MATRIX( dimension )
;
; :INPUTS:
;    dimension
;       A scalar describing the dimension of the square DCT matrix to be
;       determined.  This parameter specifies the number of columns/row
;       in the returned matrix.
;
; :KEYWORD PARAMETRS:
;    None
;
; :RETURN VALUE:
;    Result is a double precision, floating point array containing the coefficients
;    to compute the DCT of any block array of the specified size.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    February, 2009    Original code
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

FUNCTION CREATE_DCT_MATRIX, dimension

   matrix = DBLARR( dimension, dimension )

   matrix[*,0] = SQRT( 1D / dimension )

   column = DINDGEN( dimension ) + 1
   FOR row = 2, dimension DO BEGIN
      matrix[*,row-1] = COS( !DPI * ( 2 * column - 1 ) * ( row - 1 ) / 2 / dimension ) 
   ENDFOR
   matrix[*,1:dimension-1] *= SQRT( 2D / dimension )

   RETURN, matrix

END