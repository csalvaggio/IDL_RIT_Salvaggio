;+
; :NAME:
;     READ_ENVI_HEADER
;
; :PURPOSE:
;     This function serves to read an ENVI header directly into
;     an IDL program.
;
; :CATEGORY:
;     Image Processing.
;
; :CALLING SEQUENCE:
;     Result = READ_ENVI_HEADER( filename )
;
; :INPUTS:
;     filename:
;        The ENVI header filename to be read.
;
; :KEYWORD PARAMETERS:
;     NONE
;
; :RETURN VALUE:
;     A structure containing the ENVI header information from the
;     provided file.  If any error is encountered during this process, 
;     the scalar -1 will be returned.
;     
; :SIDE EFFECTS:
;     NONE
;
; :MODIFICATION HISTORY:
;     Written by:       Carl Salvaggio
;     July, 2009        Original code
;     December, 2009    Header now includes the byte order
;     December, 2013    Changed default header offset value to 0 bytes
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

FUNCTION READ_ENVI_HEADER, filename

;+
; Define the header structure
;-
   header = { description:"", $
              samples:-1L, $
              lines:-1L, $
              bands:-1L, $
              header_offset:0L, $
              file_type:"", $
              data_type:-1L, $
              interleave:"", $
              sensor_type:"", $
              byte_order:-1L, $
              wavelength_units:"", $
              z_plot_range:[-1D,-1D], $
              z_plot_titles:["",""], $
              band_names:PTR_NEW(), $
              wavelength:PTR_NEW() $
            }

;+
; Open the provided ENVI header file
;-
   OPENR, lun, filename, /GET_LUN

;+
; Read the first record and determine if this is a valid
; ENVI header file
;-
   str = ""
   READF, lun, str
   str = STRTRIM( STRCOMPRESS( str ), 2 )
   IF ( str NE "ENVI" ) THEN BEGIN
      MESSAGE, "ENVI header file has an invalid format", /CONTINUE
      RETURN, -1
   ENDIF

;+
; Read each subsequent record until the end of file is reached
;-
   WHILE NOT EOF( lun ) DO BEGIN

      READF, lun, str

;+
; If the current record does not have zero length, proceed to
; parse the information for this record
;-
      IF ( STRLEN( str ) GT 0 ) THEN BEGIN

;+
; Determine if the current record is the beginning of a new
; name/value pair or if it is the continuation of a multiple
; line value and parse the pair appropriately
;-
         str = STRCOMPRESS( str )
         equalPosition = STRPOS( str, "=" )
         IF ( equalPosition GT 0 ) THEN BEGIN
            name = STRTRIM( STRMID( str, 0, equalPosition ), 2 )
            value = STRTRIM( STRMID( str, equalPosition+1 ), 2 )
            multilineValue = STRTRIM( value, 2 )
         ENDIF ELSE BEGIN
            multilineValue = multilineValue + " " + STRTRIM( str, 2 )
         ENDELSE

;+
; If the value is defined across multiple lines, see if both the
; beginning and ending delimiters are present: if not, concatenate
; the current line with the previous line and continue reading data, 
; otherwise, strip the delimiters and proceed to parse the completed
; value
;-
         IF ( STRPOS( multilineValue, "{" ) NE -1 ) AND ( STRPOS( multilineValue, "}" ) NE -1 ) THEN BEGIN
            multilineComplete = 1
            multilineValue = STRTRIM( multilineValue, 2 )
            multilineValue = STRMID( multilineValue, 1, STRLEN( multilineValue )-2 )
         ENDIF ELSE BEGIN
            multilineComplete = 0
         ENDELSE

;+
; Parse the name/value pair
;-
         CASE STRLOWCASE( name ) OF
            "description": IF multilineComplete THEN header.description = multilineValue
            "samples": header.samples = LONG( value )
            "lines": header.lines = LONG( value )
            "bands": header.bands = LONG( value )
            "header offset": header.header_offset = LONG( value )
            "file type": header.file_type = value
            "data type": header.data_type = LONG( value )
            "interleave": header.interleave = STRLOWCASE( value )
            "sensor type": header.sensor_type = value
            "byte order": header.byte_order = LONG( value )
            "wavelength units": header.wavelength_units = value
            "z plot range": IF multilineComplete THEN header.z_plot_range = DOUBLE( STRSPLIT( multilineValue, ",", /EXTRACT ) )
            "z plot titles": IF multilineComplete THEN header.z_plot_titles = STRSPLIT( multilineValue, ",", /EXTRACT )
            "band names": IF multilineComplete THEN header.band_names = PTR_NEW( STRSPLIT( multilineValue, ",", /EXTRACT ) )
            "wavelength": IF multilineComplete THEN header.wavelength = PTR_NEW( DOUBLE( STRSPLIT( multilineValue, ",", /EXTRACT ) ) )
            ELSE:
         ENDCASE

      ENDIF

   ENDWHILE

;+
; Release the current logical unit number
;-
   FREE_LUN, lun

;+
; Return the filled header structure to the calling routine
;-
   RETURN, header

END
