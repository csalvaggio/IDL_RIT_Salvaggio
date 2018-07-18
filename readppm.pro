;+
; :NAME:
;    READPPM
;
; :PURPOSE:
;    This function serves to read a Netpbm format portable pixmap 
;    format (PPM) image.
;
; :CATEGORY:
;    Image Processing.
;
; :CALLING SEQUENCE:
;    Result = READPPM( filename )
;
; :INPUTS:
;    filename
;       The PPM image filename to be read.
;
; :KEYWORD PARAMETERS:
;    SWAP
;       This keyword will cause the function to change endian
;       on the image data read from the file.  This will only
;       effect 16-bit per pixel (UINT) images.
;
; :RETURN VALUE:
;    An array containing the image data.  This array will be of the 
;    correct data type for the provided image (BYTE or UINT) and will
;    appear in BIP interleave order (bands, samples, lines).  If the
;    filename specified does not contain a valid PPM format image, the
;    function will report this to the console and terminate execution.
;     
; :SIDE EFFECTS:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    November, 2009    Original code
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

FUNCTION READPPM, filename, SWAP=swap

;+
; Open the file on disk for read
;-
   OPENR, lun, filename, /GET_LUN

;+
; Read the first line of the file (ASCII) to check the magic
; cookie for this image, if it is found to be invalid, terminate
; execution
;-
   magicCookie = ""
   READF, lun, magicCookie
   IF ( magicCookie NE "P6" ) THEN MESSAGE, "Provided filename does not contain a valid binary PPM image"

;+
; Read the image metadata from the second and third lines of
; the file (ASCII)
;-
   numberSamples = -1LL
   numberLines = -1LL
   READF, lun, numberSamples, numberLines

   maxDC = -1LL
   READF, lun, maxDC

;+
; Set up an appropriately-sized and -typed array to receive
; the binary image data
;-
   CASE maxDC OF
      255:   image = BYTARR( 3, numberSamples, numberLines, /NOZERO )
      65535: image = UINTARR( 3, numberSamples, numberLines, /NOZERO )
   ENDCASE

;+
; Perform an unformatted read of the image data (BINARY) from
; the file
;-
   READU, lun, image

;+
; Release the logical unit number and close the image file
; on disk
;-
   FREE_LUN, lun

;+
; Perform an endian swap if the keyword has been set by the
; calling module
;-
   IF KEYWORD_SET( swap ) THEN image = SWAP_ENDIAN( image )

;+
; Return the image array to the calling module
;-
   RETURN, image

END