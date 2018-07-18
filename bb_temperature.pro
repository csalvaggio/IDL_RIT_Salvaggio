;+
; :NAME:
;    BB_TEMPERATURE
;
; :PURPOSE:
;    This function will return the blackbody temperature corresponding
;    to either the provided spectral radiance or the spectrally-
;    integrated radiance at the specified wavelength or within the
;    specified bandpass.
;
; :CATEGORY:
;    Radiometry.
;
; :CALLING SEQUENCE:
;    Result = $
;       BB_TEMPERATURE( radiance, $
;                       wavelength, $
;                       LIMITS=limits, $
;                       UNITS=units
;
; :INPUTS:
;    radiance
;       A scalar indicating the spectral radiance or the
;       intgrated spectral radaince for which the blackbody
;       temperature is to be determined.
;    wavelength
;       A scalar variable containing the wavelength for
;       which you would like to compute the blackbody 
;       temperature [microns].
;
; :KEYWORD PARAMETERS:
;    LIMITS
;       A two-element vector defining the lower and upper
;       limit of the bandpass over which the provide spectral
;       or spectrally-integrated radiance defined [microns].
;    UNITS
;       A scalar indicating the units of the provided radiance
;       value, namely
;          0:  W /  m^2 / sr / micron (DEFAULT)
;          1:  W / cm^2 / sr / micron
;          2: mW /  m^2 / sr / micron
;          3: mW / cm^2 / sr / micron
;
; :RESTRICTIONS:
;    The blackbody temperature must be in the range 1 to 1000000K
;
; :RETURN VALUE:
;    Result will contain the blackbody temperature for the provided spectral 
;    or spectrally-integrated blackbody radiance
;
; :SIDE EFFECTS:
;    None
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

FUNCTION BB_TEMPERATURE, radiance, wavelength, LIMITS=limits, UNITS=units

   ;+
   ; If the UNITS keyword is not specified, then use the DEFAULT value of [W / m^2 / sr / micron]
   ; for spectral radiance
   ;-
   IF NOT KEYWORD_SET( units ) THEN units = 0

   ;+
   ; Be certain that either a single wavelength or wavelength limits have been provided
   ;-
   IF ( N_ELEMENTS( wavelength ) EQ 0 ) AND ( NOT KEYWORD_SET( limits ) ) THEN BEGIN
      MESSAGE, "You must provide a wavelength or specify the LIMITS keyword", LEVEL=0
   ENDIF

   ;+
   ; Be certain that specific wavelengths and wavelength limits have not both been provided
   ;-
   IF ( N_ELEMENTS( wavelength ) NE 0 ) AND ( KEYWORD_SET( limits ) ) THEN BEGIN
      MESSAGE, "You can only provide a wavelength OR specify the LIMITS keyword", LEVEL=0
   ENDIF

   ;+
   ; Make sure that the computations will be carried out using double precision accuracy
   ; and that if a wavelength has been provided, it is a scalar
   ;-
   radianceProvided = DOUBLE( radiance )
   IF N_ELEMENTS( wavelength ) NE 0 THEN BEGIN
      lambda = DOUBLE( wavelength )
      IF ( N_ELEMENTS( lambda ) NE 1 ) THEN BEGIN
         MESSAGE, "You can only provide a scalar wavelength for this computation", LEVEL=0
      ENDIF
   ENDIF

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
   ; Search for the absolute blackbody temperature corresponding to the provided radiance
   ;-
   lowerTemperature = 1D
   upperTemperature = 1000000D

   error = !VALUES.D_INFINITY
   tolerance = 0.000001D

   WHILE ( error GT tolerance ) DO BEGIN
      T = ( upperTemperature + lowerTemperature ) / 2
      IF KEYWORD_SET( limits ) THEN BEGIN
         L = BB_RADIANCE( T, LIMITS=wavelengthLimits, UNITS=units )
      ENDIF ELSE BEGIN
         L = BB_RADIANCE( T, lambda, UNITS=units )
      ENDELSE
      IF ( L GT radianceProvided ) THEN BEGIN
         upperTemperature = T
      ENDIF ELSE BEGIN
         lowerTemperature = T
      ENDELSE
      error = ABS( radianceProvided - L )
   ENDWHILE

   ;+
   ; Return the computed blackbody temperature
   ;-
   RETURN, T

END