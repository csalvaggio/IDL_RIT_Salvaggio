;+
; :NAME:
;    PRINT_STRUCTURE
;
; :PURPOSE:
;    This procedure will print the contents of a structure to the
;    console, in a name/value pair format.  This will allow the user to
;    view the contents of the sturucture in a more readable format.
;
; :CATEGORY:
;    General.
;
; :CALLING SEQUENCE:
;    PRINT_STRUCTURE, structure, HELP=help
;
; :INPUTS:
;    structure
;       A structure data element whose contents are to be displayed to
;       the console. 
;
; :KEYWORD PARAMETRS:
;    HELP
;       Set this keyword to display the IDL HELP procedure information
;       for each structure element.
;
; :RETURN VALUE:
;    None
;
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    None
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    September, 2010   Original code
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

PRO PRINT_STRUCTURE, structure, HELP=help

   numberTags = N_TAGS( structure )
   tagNames = TAG_NAMES( structure )
   FOR tag = 0, numberTags-1 DO BEGIN
      PRINT, tagNames[tag]
      IF KEYWORD_SET( help ) THEN HELP, structure.(tag)
      PRINT, structure.(tag)
   ENDFOR

END