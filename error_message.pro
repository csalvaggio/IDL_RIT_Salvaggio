;+
; :NAME:
;    ERROR_MESSAGE
;
; :PURPOSE:
;    This procedure creates an error message modal dialog box.
;
; :CATEGORY:
;    Interface.
;
; :CALLING SEQUENCE:
;    ERROR_MESSAGE, event, widgetUNAME, errorString
;
; :INPUTS:
;    event
;       The event structure from the previous action/calling routine.
;    widgetUNAME
;       The UNAME of the widget over which to place the error dialog
;       box. 
;    errorString
;       A string or array of strings containing the error message
;       text to be displayed. 
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

PRO ERROR_MESSAGE, event, widgetUNAME, errorString

      status = DIALOG_MESSAGE( errorString, $
                               /ERROR, $
                               DIALOG_PARENT= $
                                  WIDGET_INFO( event.TOP, $
                                               FIND_BY_UNAME=widgetUNAME ) )

END
