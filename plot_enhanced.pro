;+
; :NAME:
;    PLOT_ENHANCED
;
; :PURPOSE:
;    This procedure serves as a wrapper around the IDL PLOT procedure that
;    adds the ability to produce encapulated Postscript versions of the plots
;    using the original IDL plot format or with the popular GNUPLOT package
;    which is available for most platforms (this package must be installed
;    separately and available in your path before this capability will be
;    functional).
;
; :CATEGORY:
;    Graphics.
;
; :CALLING SEQUENCE:
;    PLOT_ENHANCED, [x], 
;                   y, 
;                   TYPE=type, $
;                   FILENAME=filename, $
;                   ORIENTATION=orientation, $
;                   KEEP=keep, $                   
;                   ... (see IDL PLOT procedure for other optional parameters)
;
; :INPUTS:
;       x
;          A vector containing the values to be plotted on the abscissa or
;          the independent variable (this parameter is optional and if
;          omitted will be replaced by the point number)
;       y
;          A vector containing the values to be plotted on the ordinate or
;          the dependent variable (this parameter is required)
;
; :KEYWORD PARAMETERS:
;       TYPE
;          An optional keyword parameter that specifies the type of plot to
;          produce; possible values are
;             TYPE='IDL'      causes this procedure to act in the same way as
;                             the IDL PLOT procedure
;             TYPE='PS'       causes a Postscript version of the IDL plot to be
;                             created in the file specified using the FILENAME
;                             parameter
;             TYPE='GNUPLOT'  causes a Postscript version of a GNUPLOT plot to 
;                             be created in the file specified using the 
;                             FILENAME parameter (optionally the GNUPLOT script
;                             can be saved by using the KEEP parameter)
;       FILENAME
;          A required keyword parameter if the TYPE keyword is set to either
;          'PS' or 'GNUPLOT' that specifies the name of the file to contain
;          the output rendering (if not provided, a DIALOG_PICKFILE() will be
;          presented to the user)
;       ORIENTATION
;          An optional keyword parameter indicating the orientation of the plot;
;          possible values ar 'LANDSCAPE' (default), 'SQUARE', or 'PORTRAIT'
;       KEEP
;          An optional keyword parameter that indicates that the GNUPLOT script
;          file should also be saved with a ".gnuplot" extension appended to
;          the FILENAME parameter (this has an effect only when TYPE='GNUPLOT')
;
; :RESULT:
;    An on-screen or in file rendering (encapsulated Postscript format) of the 
;    graphs of vector arguments.
;    
; :SIDE EFFECTS:
;    None
;
; :REQUIRES:
;    TAG_EXISTS
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio, Philip Salvaggio
;    October, 2009     Original code
;    November, 2009    Added additional GNUPLOT functionality
;                      Added TAG_EXISTS function to search extra keywords
;                      structure for PLOT options (alleviating some of the
;                      limitations of using KEYWORD_SET for this purpose, 
;                      namely that undefined tags resulted in a run-time
;                      error)
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

PRO PLOT_ENHANCED, x, $
                   y, $
                   TYPE=type, $
                   FILENAME=filename, $
                   ORIENTATION=orientation, $
                   KEEP=keep, $
                   _EXTRA=extraKeywords

;+
; If one parameter is provided, the vector parameter is plotted on the ordinate 
; versus the point number on the abscissa.  If this is the case, create a point
; number vector to pass on to PLOT.
;-
   IF ( N_PARAMS() EQ 1 ) THEN BEGIN
      localY = x
      localX = LINDGEN( N_ELEMENTS( localY ) )
   ENDIF ELSE BEGIN
      localX = x
      localY = y
   ENDELSE

;+
; Check if the type (TYPE) keyword is used and define the plot type 
; appropriately.
;-
   plotType = 'IDL'
   IF KEYWORD_SET( type ) THEN BEGIN
      plotType = STRUPCASE( type )
      CASE plotType OF
         'IDL':
         'PS':
         'GNUPLOT':
         ELSE: plotType = 'IDL'
      ENDCASE
   ENDIF

;+
; A filename is required for the GNUPLOT and PS plot types.  If one was
; not specified using the filename (FILENAME) keyword parameter, present the
; user with a dialog box to provide the filename at this point.
;-
   IF ( (plotType EQ 'PS') OR (plotType EQ 'GNUPLOT') ) THEN BEGIN
      outputFilename = ''
      IF KEYWORD_SET( filename ) THEN outputFilename = filename
      WHILE ( STRLEN( outputFilename ) EQ 0 ) DO BEGIN
         CASE plotType OF
            'PS': outputFilename = DIALOG_PICKFILE( TITLE='Select POSTSCRIPT File' )
            'GNUPLOT': outputFilename = DIALOG_PICKFILE( TITLE='Select GNUPLOT File' )
         ENDCASE
      ENDWHILE
   ENDIF
   
;+
; Check if the orientation (ORIENTATION) keyword is used and define the
; orientation type and the plot dimensions appropriately (NOTE: this 
; will be ignored if the plot type is set to IDL).
;-
   orientationType = 'LANDSCAPE'
   xsize = 6
   ysize = 4
   IF KEYWORD_SET( orientation ) THEN BEGIN
      orientationType = STRUPCASE( orientation )
      CASE orientationType OF
         'LANDSCAPE': $
            BEGIN
               xsize = 6
               ysize = 4
            END
         'SQUARE': $
            BEGIN
               xsize = 4
               ysize = 4
            END
         'PORTRAIT': $
            BEGIN
               xsize = 4
               ysize = 6
            END
         ELSE:  $
            BEGIN
               orientationType = 'LANDSCAPE'
               xsize = 6
               ysize = 4
            END
      ENDCASE
   ENDIF

;+
; Configure and produce the appropriate plot type
;-
   CASE plotType OF
      'IDL': $
         BEGIN
            PLOT, localX, localY, _STRICT_EXTRA=extraKeywords
         END

      'PS': $
         BEGIN
            currentDevice = !D.NAME
            SET_PLOT, 'PS'
            DEVICE, FILENAME=outputFilename, $
                    XSIZE=xsize, $
                    YSIZE=ysize, $
                    /INCHES, $
                    /ENCAPSULATED, $
                    PREVIEW=2   
            PLOT, localX, localY, _STRICT_EXTRA=extraKeywords
            DEVICE, /CLOSE
            SET_PLOT, currentDevice
         END

      'GNUPLOT': $
         BEGIN
            OPENW, lun, outputFilename + '.gnuplot', /GET_LUN
            PRINTF, lun, 'set term postscript eps enhanced color'
            PRINTF, lun, 'set output "' + outputFilename + '"'
            PRINTF, lun, 'set size ratio ' + $
               STRCOMPRESS( STRING( DOUBLE( ysize ) / DOUBLE( xsize ) ), /REMOVE_ALL )
            IF TAG_EXISTS( 'XTITLE', extraKeywords ) THEN $
               PRINTF, lun, 'set xlabel "' + extraKeywords.XTITLE + '"'
            IF TAG_EXISTS( 'YTITLE', extraKeywords ) THEN $
               PRINTF, lun, 'set ylabel "' + extraKeywords.YTITLE + '"'
            IF TAG_EXISTS( 'TITLE', extraKeywords ) THEN $
               PRINTF, lun, 'set title "' + extraKeywords.TITLE + '"'
            IF TAG_EXISTS( 'XRANGE', extraKeywords ) THEN $
               PRINTF, lun, 'set xrange [' + $
                            STRCOMPRESS( STRING( extraKeywords.XRANGE[0] ), /REMOVE_ALL ) + $
                            ':' + $
                            STRCOMPRESS( STRING( extraKeywords.XRANGE[1] ), /REMOVE_ALL ) + $
                            ']'
            IF TAG_EXISTS( 'YRANGE', extraKeywords ) THEN $
               PRINTF, lun, 'set yrange [' + $
                            STRCOMPRESS( STRING( extraKeywords.YRANGE[0] ), /REMOVE_ALL ) + $
                            ':' + $
                            STRCOMPRESS( STRING( extraKeywords.YRANGE[1] ), /REMOVE_ALL ) + $
                            ']'
            IF TAG_EXISTS( 'XLOG', extraKeywords ) THEN $
               PRINTF, lun, 'set logscale x' ELSE PRINTF, lun, 'unset logscale x'
            IF TAG_EXISTS( 'YLOG', extraKeywords ) THEN $
               PRINTF, lun, 'set logscale y' ELSE PRINTF, lun, 'unset logscale y'
            PRINTF, lun, 'unset key'
            IF TAG_EXISTS( 'PSYM', extraKeywords ) THEN BEGIN
               IF ( extraKeywords.PSYM EQ 0  ) THEN withClause = 'lines'
               IF ( extraKeywords.PSYM GT 0  ) THEN withClause = 'points'
               IF ( extraKeywords.PSYM EQ 10 ) THEN withClause = 'boxes'
               IF ( extraKeywords.PSYM LT 0  ) THEN withClause = 'linespoints'
            ENDIF ELSE BEGIN
               withClause = 'lines'
            ENDELSE
            PRINTF, lun, 'plot "-" with ' + withClause
            FOR point = 0, N_ELEMENTS( localX )-1 DO BEGIN
               PRINTF, lun, localX[point], localY[point]
            ENDFOR
            PRINTF, lun, 'e'
            FREE_LUN, lun
            SPAWN, 'gnuplot ' + outputFilename + '.gnuplot', EXIT_STATUS=status
            IF ( status EQ 0 ) THEN BEGIN
               IF NOT KEYWORD_SET( keep ) THEN FILE_DELETE, outputFilename + '.gnuplot'
            ENDIF ELSE BEGIN
               MESSAGE, 'A problem occurred with gnuplot, plot script was saved', /CONTINUE
            ENDELSE
         END
   ENDCASE

END