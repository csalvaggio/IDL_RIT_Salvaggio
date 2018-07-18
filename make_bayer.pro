;+
; :NAME:
;    MAKE_BAYER
;
; :PURPOSE:
;    This function will construct a document mode image from a provided
;    three-band color image [3,*,*] using the following Bayer pattern
;    [ [G,R], [B,G] ].
;
; :CATEGORY:
;    Image Processing.
;
; :CALLING SEQUENCE:
;    Result = MAKE_BAYER( image, [R=r], [G=g], [B=b], 
;                                [COMPOSITE=composite], 
;                                [FILEBASENAME=fileBasename] )
;
; :INPUTS:
;    image
;       The three-band color image array to be converted to 
;       document mode (the band dimension must be located in
;       the first array dimension).
;
; :KEYWORD PARAMETERS:
;    R
;       This keyword will cause the red color component image to be 
;       assigned to the provided named variable.
;    G
;       This keyword will cause the green color component image to be 
;       assigned to the provided named variable.
;    B
;       This keyword will cause the blue color component image to be 
;       assigned to the provided named variable.
;    COMPOSITE
;       This keyword will cause the red, green, and blue color component 
;       images to be assembled into a sparsely populated composite image
;       and assigned to the provided named variable (only the filtered 
;       color positions will be present in each color channel).
;    FILEBASENAME
;       A string containing the full path and base filename for the
;       individual components, the composite, and the document mode image
;       to be written to.  If specified all five (5) files will be
;       created with no regard for previously existing files with the
;       same filenames.
;
; :RETURN VALUE:
;    An array containing the document mode image data.
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

FUNCTION MAKE_BAYER, image, R=r, G=g, B=b, COMPOSITE=composite, FILEBASENAME=fileBasename

;+
; Determine the dimensions of the provided image array
;-
   dimensions = SIZE( image, /DIMENSIONS )

;+
; If the first dimension of the image array is not 3 then report an error
; and halt execution
;-
   IF ( dimensions[0] NE 3 ) THEN BEGIN
      MESSAGE, 'Provided image must contain three bands and these bands must be located in the first array dimension'
   ENDIF

;+
; Parse the spatial dimensions of the image array
;-
   numberSamples = dimensions[1]
   numberLines = dimensions[2]

;+
; Create a multiplicative three-dimensional mask with which to extract
; the individual filter array components
;      G R
;      B G
;+
   mask = BYTARR( 3, numberSamples, numberLines )

   FOR line = 0, numberLines-1, 2 DO BEGIN
      FOR sample = 1, numberSamples-1, 2 DO BEGIN
         mask[0,sample,line] = 1
      ENDFOR
   ENDFOR

   FOR line = 0, numberLines-1 DO BEGIN
      IF ( (line MOD 2) EQ 0 ) THEN BEGIN
         FOR sample = 0, numberSamples-1, 2 DO BEGIN
            mask[1,sample,line] = 1
         ENDFOR
      ENDIF ELSE BEGIN
         FOR sample = 1, numberSamples-1, 2 DO BEGIN
            mask[1,sample,line] = 1
         ENDFOR
      ENDELSE
   ENDFOR

   FOR line = 1, numberLines-1, 2 DO BEGIN
      FOR sample = 0, numberSamples-1, 2 DO BEGIN
         mask[2,sample,line] = 1
      ENDFOR
   ENDFOR

;+
; Create a composite RGB image containing just the individual color filter
; array components in the respective color channels
;-
   composite = image * mask
   IF KEYWORD_SET( fileBasename ) THEN WRITE_PNG, fileBasename + '_composite.png', composite

;+
; Extract the individual color channels from the composite image
;-
   r = REFORM( composite[0,*,*] )
   IF KEYWORD_SET( fileBasename ) THEN WRITE_PNG, fileBasename + '_red.png', r
   g = REFORM( composite[1,*,*] )
   IF KEYWORD_SET( fileBasename ) THEN WRITE_PNG, fileBasename + '_green.png', g
   b = REFORM( composite[2,*,*] )
   IF KEYWORD_SET( fileBasename ) THEN WRITE_PNG, fileBasename + '_blue.png', b

;+
; Create the document mode image by summing the individual color component images
;-
   document = r + g + b
   IF KEYWORD_SET( fileBasename ) THEN WRITE_PNG, fileBasename + '_document.png', document

;+
; Return the document mode image to the calling module
;-
   RETURN, document

END