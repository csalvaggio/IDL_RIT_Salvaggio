;+
; :NAME:
;    PAUSE_EXECUTION
;
; :PURPOSE:
;    This procedure pauses a routine's execution by creating a modal 
;    dialog box containing a message.  Execution is paused until
;    this dialog box is dismissed.
;
; :CATEGORY:
;    Interface.
;
; :CALLING SEQUENCE:
;    PAUSE_EXECUTION, event, widgetUNAME, messageString
;
; :INPUTS:
;    event
;       The event structure from the previous action/calling routine.
;    widgetUNAME
;       The UNAME of the widget over which to place the dialog box. 
;    errorString
;       A string or array of strings containing the message text to
;       be displayed. 
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
;    August, 2001      Original code
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

PRO PAUSE_EXECUTION, event, widgetUNAME, messageString

      status = DIALOG_MESSAGE( messageString, $
                               /INFORMATION, $
                               DIALOG_PARENT = $
                                  WIDGET_INFO( event.TOP, $
                                               FIND_BY_UNAME= widgetUNAME ) )

END
