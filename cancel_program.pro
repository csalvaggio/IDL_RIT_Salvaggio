;+
; :NAME:
;    CANCEL_PROGRAM
;
; :PURPOSE:
;    This procedure deletes all windows created by the calling routine, 
;    closes all files, and destroys the calling routine/widget.
;
; :CATEGORY:
;    Interface.
;
; :CALLING SEQUENCE:
;    CANCEL_PROGRAM, event
;
; :INPUTS:
;    event
;       The event structure from the previous action/calling routine.
;
; :KEYWORD PARAMETRS:
;    None
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
;    March, 2000       Original code
;    December, 2004    Modified to close all open file
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

PRO CANCEL_PROGRAM, event 
 
   WHILE ( !D.WINDOW GE 0 ) DO WDELETE, !D.WINDOW 
 
   CLOSE, /ALL 
   WIDGET_CONTROL, event.TOP, /DESTROY 
 
END 
