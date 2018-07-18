;+
; :NAME:
;    RANDOM_SAMPLE
;
; :PURPOSE:
;    This function will return a vector of "n" random elements
;    selected from either a provided vector OR from a produced
;    vector of "N" elements using a long data index generator.
;    The returned random elements will be unique, that is, no 
;    individual element in the array may be selected twice.
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    Result = $
;       RANDOM_SAMPLE( seed, array, numberToSelect )
;
; :INPUTS:
;    seed
;       A variable or constant used to initialize the random 
;       sequence on input, and in which the state of the random 
;       number generator is saved on output.
;    array
;       A one-dimensional vector containing the data from which
;       the random sample of specified size is to be selected
;       -OR- a scalar specifying the number of elements to be
;       placed in an array by the long data index generator
;       function from which the random sample will be selected
;       (this usage is recommended to produce random indices to
;       be used rather that directly selecting a random sample).
;    numberToSelect
;       The number of random samples to select from the provided
;       or generated vector.
;
; :KEYWORD PARAMETERS:
;    None
;
; :RESTRICTIONS:
;    The numbetToSelect must be less than the length of the provided
;    or generated vector.
;
; :EXAMPLES:
;    EXAMPLE 1
;       seed = 135L
;       array = [ 9, 2, 3, 4, 6, 3 ]
;       numberToSelect = 2
;       sample = RANDOM_SAMPLE( seed, array, numberToSelect )
;
;    The above example will select 2 random elements from the supplied
;    array and return them in a 2-element vector.
;
;    EXAMPLE 2
;       seed = 135L
;       numberOfElements = 10
;       numberToSelect = 3
;       sample = RANDOM_SAMPLE( seed, NumberOfElements, numberToSelect )
;
;    The above example will select 2 random elements from a sequentially
;    filled array of 10 elements and return them in a 2-element vector.
;
; :RETURN VALUE:
;    Result is an array containing the parameters of the best fit model
;    determined during the RANSAC process.
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    September, 2008   Original code
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

FUNCTION RANDOM_SAMPLE, seed, array, numberToSelect

;+
; Create a local copy of the provided array so that the original
; is not modified in the calling function/procedure
;-
   localArray = array

;+
; Determine the number of elements in the provided array OR if
; array is a scalar, produce a sequentially filled array using
; the long data index generator and set the number of points 
; equal to the provided scalar value
;-
   numberPoints = N_ELEMENTS( localArray )
   IF ( numberPoints EQ 1 ) THEN BEGIN
      numberPoints = localArray
      localArray = INDGEN( numberPoints )
      items = LONARR( numberToSelect )
   ENDIF ELSE BEGIN
      items = localArray[0:numberToSelect-1]
   ENDELSE

;+
; Be certain the number of points to select is less than the 
; number of points provided -OR- generated
;-
   IF ( numberPoints LT numberToSelect ) THEN BEGIN
      PRINT, "%RANDOM_SAMPLE: "
      PRINT, "Trying to select more points than provided"
      RETURN, -1
   ENDIF

;+
; For each random sample to produce, select it from a sublist of the 
; provided values where the sublist is reduced in length by one each 
; time a value is selected, and the value selected is replaced with 
; the value just beyond the end of the current sublist (to assure
; unique selection - no repeats)
;-
   FOR i = 0, numberToSelect-1 DO BEGIN
      randomValue = FLOOR( (numberPoints-i) * RANDOMU( seed ) )
      items[i] = localArray[randomValue]
      localArray[randomValue] = localArray[numberPoints-i-1]
   ENDFOR

;+
; Return the selected random samples OR the number of indices 
; requested
;-
   RETURN, items

END