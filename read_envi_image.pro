;+
; :NAME:
;     READ_ENVI_IMAGE
;
; :PURPOSE:
;     This function serves to read an ENVI image/header directly into
;     an IDL program without the need to first open that image in ENVI
;     and use the ENVI_* routines to do so.
;
; :CATEGORY:
;     Image Processing.
;
; :CALLING SEQUENCE:
;     Result = READ_ENVI_IMAGE( filename, HEADER=header, OUTPUT_INTERLEAVE=outputInterleave )
;
; :INPUTS:
;     filename:
;        The ENVI image filename to be read.
;
; :KEYWORD PARAMETERS:
;     HEADER:
;        A named variable to receive a structure containing the header 
;        data read in from the accompanying ENVI image header file.
;     OUTPUT_INTERLEAVE:
;        An optional named variable indicating the interleave for the
;        returned array containing the image data.  Valid values are
;        "bip", "bil", and "bsq" [Default is "bip"]
;
; :RETURN VALUE:
;     An array containing the image data.  This array will be of the 
;     correct data type for the provided image and will appear in BIP
;     interleave order (bands, samples, lines).  If any error is 
;     encountered during this process, the scalar -1 will be returned.
; 
; :SIDE EFFECTS:
;     NONE
;
; :MODIFICATION HISTORY:
;     Written by:       Carl Salvaggio
;     July, 2009        Original code
;     December, 2009    OS family dependent swap endian added
;     November, 2013    The original FOR loop implementation for rearranging
;                       data into BIP format was replaced with a faster transpose
;                       routine (Jason Casey)
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

FUNCTION READ_ENVI_IMAGE, filename, HEADER=header, OUTPUT_INTERLEAVE=outputInterleave

;+
; Check for existence of the provided ENVI image file
;-
   IF NOT FILE_TEST( filename ) THEN BEGIN
      MESSAGE, "Image filename provided does not exist", /CONTINUE
      RETURN, -1
   ENDIF

;+
; Check to see if the output interleave keyword is set
;-
   IF NOT KEYWORD_SET( outputInterleave ) THEN BEGIN
      outputInterleave = "bip"
   ENDIF ELSE BEGIN
      outputInterleave = STRLOWCASE( outputInterleave )
   ENDELSE

;+
; Check for the existence of the default ENVI header file (the ENVI
; image filename plus the ".hdr" extension), if this does not exist,
; then provide the user with a dialog pickfile box with which they 
; can locate the appropriate ENVI header file
;-
   headerFilename = filename + ".hdr"
   IF NOT FILE_TEST( headerFilename ) THEN BEGIN
      headerFilename = DIALOG_PICKFILE( TITLE="Please select ENVI header file", $
                                        PATH=FILE_DIRNAME( filename ), $
                                        FILTER="*.hdr" )
      IF ( headerFilename EQ "" ) THEN BEGIN
         MESSAGE, "A valid ENVI header file was not provided", /CONTINUE
         RETURN, -1
      ENDIF
   ENDIF

;+
; Define the header structure by reading the ENVI header file
;-
   header = READ_ENVI_HEADER( headerFilename )
   
;+
; Read the image data into a vector of the proper size and data 
; type (skip any header bytes in the image file if they are present)
;-
   originalImage = $
      READ_BINARY( filename, $
                   DATA_DIMS=(header.bands * header.samples * header.lines), $
                   DATA_TYPE=header.data_type, $
                   DATA_START=header.header_offset )

;+
; Reform the vector into an image array according to the interleave
; type given in the header
;-
   CASE header.interleave OF
      "bsq": originalImage = REFORM( originalImage, header.samples, header.lines, header.bands )
      "bil": originalImage = REFORM( originalImage, header.samples, header.bands, header.lines )
      "bip": originalImage = REFORM( originalImage, header.bands, header.samples, header.lines )
   ENDCASE

;+
; Rearrange the pixels to match the specified output interleave format
;-
   CASE header.interleave OF
      "bsq": $
         BEGIN
            CASE outputInterleave OF
               "bsq": image = originalImage
               "bip": image = TRANSPOSE( originalImage, [2,0,1] )
               "bil": image = TRANSPOSE( originalImage, [0,2,1] )
            ENDCASE
         END
      "bip": $
         BEGIN
            CASE outputInterleave OF
               "bsq": image = TRANSPOSE( originalImage, [2,0,1] )
               "bip": image = originalImage
               "bil": image = TRANSPOSE( originalImage, [1,0,2] )
            ENDCASE
         END
      "bil": $
         BEGIN
            CASE outputInterleave OF
               "bsq": image = TRANSPOSE( originalImage, [0,2,1] )
               "bip": image = TRANSPOSE( originalImage, [1,0,2] )
               "bil": image = originalImage
            ENDCASE
         END
   ENDCASE

;+
; Eliminate any unary dimension in the image array
;-
   image = REFORM( image )

;+
; Change the endian if necessary
;-
   CASE header.byte_order OF
      0: IF ( !VERSION.OS_FAMILY EQ "unix" ) THEN SWAP_ENDIAN_INPLACE, image
      1: IF ( !VERSION.OS_FAMILY EQ "windows" ) THEN SWAP_ENDIAN_INPLACE, image
      ELSE:
   ENDCASE

;+
; Return the image to the calling routine
;-   
   RETURN, image

END
