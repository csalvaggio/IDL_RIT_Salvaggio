;+
; :NAME:
;    WRITE_PHOTOSHOP_CUSTOM_FILTER
;
; :PURPOSE:
;    This procedure will write a custom Photoshop filter to disk
;    in the proper binary format.
;
; :CATEGORY:
;    Image Processing.
;
; :CALLING SEQUENCE:
;    WRITE_PHOTOSHOP_CUSTOM_FILTER, filename, $
;                                   kernel, $
;                                   SCALE=scale, $
;                                   OFFSET=offset
;
; :INPUTS:
;    filename
;       The filename in which to save the custom Photoshop filter.
;    kernel
;       A 5x5 array of short integers containing the weights that
;       make up the custom filter.  If the data type is anything
;       but a short integer, the weight values will be truncated
;       and converted to short integer types automatically.  The
;       original data will remain unaltered.
;
; :KEYWORD PARAMETERS:
;    SCALE
;       This keyword will set the scale to be used when applying
;       the filter in Photoshop.  The response from the filter
;       will be divided by the value specified.  If this keyword
;       is not set, the scale will be computed as the sum of the
;       individual weights making up the kernel.  If this sum is
;       zero, then the scale will be set to one.
;    OFFSET
;       This keyword will set the offset to be used when applying
;       the filter in Photoshop.  The value specified will be added
;       to the response at each location.  If this keyword is not
;       specified, then the offset will be set to zero.
;
; :SIDE EFFECTS:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    February, 2013    Original code
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

PRO WRITE_PHOTOSHOP_CUSTOM_FILTER, filename, kernel, SCALE=scale, OFFSET=offset

   localKernel = FIX( kernel )

   OPENW, lun, filename, /GET_LUN
   WRITEU, lun, SWAP_ENDIAN( localKernel )

   IF KEYWORD_SET( scale ) THEN BEGIN
      localScale = FIX( scale )
   ENDIF ELSE BEGIN
      localScale = FIX( TOTAL( localKernel ) )
      IF ( localScale EQ 0 ) THEN BEGIN
         localScale = FIX( 1 )
      ENDIF
   ENDELSE
   WRITEU, lun, SWAP_ENDIAN( localScale )

   IF KEYWORD_SET( offset ) THEN BEGIN
      localOffset = FIX( offset )
   ENDIF ELSE BEGIN
      localOffset = FIX( 0 )
   ENDELSE
   WRITEU, lun, SWAP_ENDIAN( localOffset )

   FREE_LUN, lun

END