;+
; :NAME:
;    GAUSSIAN
;
; :PURPOSE:
;    This function computes the value of a Gaussian distribution
;    for a given random variate and a specified mean and standard
;    deviation.
;
; :CATEGORY:
;    Statistics.
;
; :CALLING SEQUENCE:
;    Result = GAUSSIAN( x, mean, standardDeviation [, /UNSCALED] )
;
; :INPUTS:
;    x
;       A scalar or vector containing the random variates for which
;       the Gaussian distribution value(s) should be computed.
;    mean
;       A scalar defining the mean of the Gaussian distribution.
;    standardDeviation
;       A scalar defining the standard deviation of the Gaussian
;       distribution.
;
; :KEYWORD PARAMETERS:
;    UNSCALED
;       An optional flag indicating that the exponential term should
;       not be scaled (for applications that require that).  When this
;       flag is specified, the value returned will be 1.0 when x is
;       equal to the mean.
;
; :RETURN VALUE:
;    Result is a scalar or vector containing the values from a 
;    Gaussian distribution for the provided random variate(s) and
;    the specified mean and standard deviation.
;
; :SIDE EFFECTS:
;    None
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

FUNCTION GAUSSIAN, x, mean, standardDeviation, UNSCALED=unscaled

   d = DOUBLE( x ) - DOUBLE( mean )   
   sigma = DOUBLE( standardDeviation )

   scale = KEYWORD_SET( unscaled ) ? 1D : 1D / SQRT( 2D * !DPI * sigma^2 )

   RETURN, scale * EXP( -0.5D * ( d / sigma )^2 )

END