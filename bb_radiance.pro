;+
; :NAME:
;    BB_RADIANCE
;
; :PURPOSE:
;    This function will return either the spectral radiance(s) at a 
;    specific wavelength or at a discrete set of wavelengths provided 
;    in an array or it will return the spectrally integrated radiance 
;    across a bandpass specified by the wavelength limits for a 
;    provided absolute temperature.
;
; :CATEGORY:
;    Radiometry.
;
; :CALLING SEQUENCE:
;    Result = $
;       BB_RADIANCE( absoluteTemperature, $
;                    wavelength, $
;                    LIMITS=limits, $
;                    UNITS=units
;
; :INPUTS:
;    absoluteTemperature
;       A scalar indicating the absolute temperature for 
;       which you would like to compute the spectral or 
;       spectrally-integrated radiance [K].
;    wavelength
;       A scalar or array variable containing the wavelength
;       or wavelengths for which you would like to compute
;       the blackbody radiance [microns].
;
; :KEYWORD PARAMETERS:
;    LIMITS
;       A two-element vector defining the lower and upper
;       limit of the bandpass for which you would like to
;       compute the spectrally-integrated radiance [microns].
;    UNITS
;       A scalar indicating the units that the returned
;       radiance should have, namely
;          0:  W /  m^2 / sr / micron (DEFAULT)
;          1:  W / cm^2 / sr / micron
;          2: mW /  m^2 / sr / micron
;          3: mW / cm^2 / sr / micron
;
; :RETURN VALUE:
;    Result will contain the spectral or spectrally-integrated blackbody radiance
;
; :SIDE EFFECTS:
;    NONE
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    December, 2008    Original code
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

FUNCTION BB_RADIANCE, absoluteTemperature, wavelength, LIMITS=limits, UNITS=units

;+
; If the UNITS keyword is not specified, then use the DEFAULT value of [W / m^2 / sr / micron]
; for spectral radiance
;-
   IF NOT KEYWORD_SET( units ) THEN units = 0

;+
; Be certain that either a wavelength, wavelength array, or wavelength limits have been provided
;-
   IF ( N_ELEMENTS( wavelength ) EQ 0 ) AND ( NOT KEYWORD_SET( limits ) ) THEN BEGIN
      MESSAGE, "You must provide a wavelength/wavelength array or specify the LIMITS keyword", LEVEL=0
   ENDIF

;+
; Be certain that specific wavelengths and wavelength limits have not both been provided
;-
   IF ( N_ELEMENTS( wavelength ) NE 0 ) AND ( KEYWORD_SET( limits ) ) THEN BEGIN
      MESSAGE, "You can only provide a wavelength/wavelength array OR specify the LIMITS keyword", LEVEL=0
   ENDIF

;+
; Make sure that the computations will be carried out using double precision accuracy
;-
   T = DOUBLE( absoluteTemperature )
   IF N_ELEMENTS( wavelength ) NE 0 THEN lambda = DOUBLE( wavelength )

;+
; If wavelength limits have been provided, generate an array of wavelength values to be used
;-
   IF KEYWORD_SET( limits ) THEN BEGIN
      wavelengthLimits = DOUBLE( limits )
      numberIncrements = 100000
      lambda = DINDGEN( numberIncrements + 1 ) / numberIncrements * $
               ( wavelengthLimits[1] - wavelengthLimits[0] ) + wavelengthLimits[0]
   ENDIF

;+
; Compute the spectral blackbody radiance using Planck's law
;-
   c1 = 3.74151e08  ; W / m^2 / micron
   c2 = 1.43879e04  ; micron K
   M = c1 / lambda^5 / ( EXP( c2 / lambda / T ) - 1 )
   L = M / !DPI

;+
; Convert the computed spectral blackbody radiance to the user specified units
;-
   CASE units OF
      1: L = L / 10000.0                   ;  W / cm^2 / sr / micron
      2: L = L * 1000.0                    ; mW /  m^2 / sr / micron
      3: L = L * 1000.0 / 10000.0          ; mW / cm^2 / sr / micron
      ELSE:                                ;  W /  m^2 / sr / micron (DEFAULT)
   ENDCASE

;+
; If wavelength limits have been specified, compute the integrated radiance over the bandpass
;-
   IF KEYWORD_SET( limits ) THEN BEGIN
      dLambda = ( wavelengthLimits[1] - wavelengthLimits[0] ) / numberIncrements 
      L = TOTAL( L * dLambda )
   ENDIF

;+
; Return the spectral or spectrally-integrated blackbody radiance
;-
   RETURN, L

END