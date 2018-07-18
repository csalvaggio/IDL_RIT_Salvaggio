;+
; :NAME:
;    TES
;
; :PURPOSE:
;    This user-interactive procedure will perform a temperature-emissivity
;    separation using a user-provided sample and downwelling radiance 
;    spectra.  The spectra must be provided as a function of wavelength
;    [micrometers] in units of W/m^2/sr/micrometer.
;
; :REFERENCE:
;    1) Horton, K.A., J.R. Johnson, and P.G. Lucey, 1998, Infrared measurement 
;       of pristine and disturbed soils 2. Environmental effects and field data 
;       reduction, Remote Sensing of Environment, v. 64, p. 47-52.
;    2) Bower, N., R.O. Knuteson, and H.E. Revercomb, High spectral resolution 
;       land surface temperature and emissivity measurements in the thermal 
;       infrared, Publication from the Co-operative Institute for Meteorological 
;       and Satellite Studies at the University of Wisconsin, Madison.
;
; :CATEGORY:
;    Radiometry.
;
; :CALLING SEQUENCE:
;    TES
;
; :INPUTS:
;    NONE
;
; :KEYWORD PARAMETERS:
;    NONE
;
; :RETURN VALUE:
;    NONE
;
; :SIDE EFFECTS:
;    NONE
;
; :REQUIRED:
;    BB_RADIANCE
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    June, 2012        Original code
;    February, 2013    Fixed smoothness metric to be the average squared second
;                      derivative (it was the average squared first derivative)
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

PRO TES_event, event

   WIDGET_CONTROL, event.ID, $
                   GET_UVALUE=widget

   WIDGET_CONTROL, event.TOP, $
                   GET_UVALUE=state

;+
; Set the default condition that the 'Save' button has not been
; pressed to begin processing
;-
   savePressed = 0

   CASE widget OF

      'sampleFilenameTextbox':
      'downwellingFilenameTextbox':
      'smoothnessIntervalLowerTextbox':
      'smoothnessIntervalUpperTextbox':
      'temperatureIntervalLowerTextbox':
      'temperatureIntervalUpperTextbox':
      'plotIntervalLowerTextbox':
      'plotIntervalUpperTextbox':
      'knownTemperatureTextbox':

      'quitButton': $
         BEGIN
            WIDGET_CONTROL, event.TOP, $
                            /DESTROY
            RETURN
         END

      'saveButton': $
         BEGIN
            savePressed = 1
         END

      'knownButton': $
         BEGIN
            IF ( state.method EQ 'smoothness' ) THEN state.method = 'known'
            parametersBase = $
               WIDGET_INFO( event.TOP, $
                            FIND_BY_UNAME='parametersBase' )
            WIDGET_CONTROL, parametersBase, $
                            SENSITIVE=0
            knownTemperatureTextbox = $
               WIDGET_INFO( event.TOP, $
                            FIND_BY_UNAME='knownTemperatureTextbox' )
            WIDGET_CONTROL, knownTemperatureTextbox, $
                            SENSITIVE=1
            plotAreaID = $
               WIDGET_INFO( event.TOP, $
                            FIND_BY_UNAME='squaredSecondDerivativePlotArea' )
            WIDGET_CONTROL, plotAreaID, $
                            GET_VALUE=windowNumber
            WSET, windowNumber
            ERASE
            XYOUTS, 120, 250, 'Select "Emissivity Plot" tab', CHARSIZE=3, CHARTHICK=1, /DEVICE
            plotAreaID = $
               WIDGET_INFO( event.TOP, $
                            FIND_BY_UNAME='smoothnessPlotArea' )
            WIDGET_CONTROL, plotAreaID, $
                            GET_VALUE=windowNumber
            WSET, windowNumber
            ERASE
            XYOUTS, 120, 250, 'Select "Emissivity Plot" tab', CHARSIZE=3, CHARTHICK=1, /DEVICE
            plotAreaID = $
               WIDGET_INFO( event.TOP, $
                            FIND_BY_UNAME='emissivityPlotArea' )
            WIDGET_CONTROL, plotAreaID, $
                            GET_VALUE=windowNumber
            WSET, windowNumber
         END

      'smoothnessButton': $
         BEGIN
            IF ( state.method EQ 'known' ) THEN state.method = 'smoothness'
            parametersBase = $
               WIDGET_INFO( event.TOP, $
                            FIND_BY_UNAME='parametersBase' )
            WIDGET_CONTROL, parametersBase, $
                            SENSITIVE=1
            knownTemperatureTextbox = $
               WIDGET_INFO( event.TOP, $
                            FIND_BY_UNAME='knownTemperatureTextbox' )
            WIDGET_CONTROL, knownTemperatureTextbox, $
                            SENSITIVE=0
         END

      'sampleFilenameButton': $
         BEGIN
            filename = $
               DIALOG_PICKFILE( DIALOG_PARENT=event.TOP, $
                                TITLE='Please select a sample radiance file', $
                                FILTER='*.sac;*.SAc;*.SAC', $
                                /FIX_FILTER, $
                                PATH=state.directoryPath )
            IF ( filename EQ '' ) THEN RETURN
            state.directoryPath = FILE_DIRNAME( filename )
            state.sampleFilename = filename
            textboxID = $
               WIDGET_INFO( event.TOP, $
                            FIND_BY_UNAME='sampleFilenameTextbox' )
            WIDGET_CONTROL, textboxID, $
                            SET_VALUE=state.sampleFilename
            state.ready = 0L
            state.sampleTemplate = PTR_NEW()
         END

      'downwellingFilenameButton': $
         BEGIN
            filename = $
               DIALOG_PICKFILE( DIALOG_PARENT=event.TOP, $
                                TITLE='Please select a downwelling radiance file', $
                                FILTER='*.dwc;*.DWc;*.DWC', $
                                /FIX_FILTER, $
                                PATH=state.directoryPath )
            IF ( filename EQ '' ) THEN RETURN
            state.directoryPath = FILE_DIRNAME( filename )
            state.downwellingFilename = filename
            textboxID = $
               WIDGET_INFO( event.TOP, $
                            FIND_BY_UNAME='downwellingFilenameTextbox' )
            WIDGET_CONTROL, textboxID, $
                            SET_VALUE=state.downwellingFilename
            state.ready = 0L
            state.downwellingTemplate = PTR_NEW()
         END

      'plotBase': $
         BEGIN
            CASE event.TAB OF
               0: state.view = 0
               1: state.view = 1
               2: state.view = 2
            ENDCASE
         END

   ENDCASE

;+
; Test to see if both files have been selected and everything is ready
; to go
;-
   IF ( FILE_TEST( state.sampleFilename ) AND $
        FILE_TEST( state.downwellingFilename ) ) THEN BEGIN
      state.ready = 1
   ENDIF ELSE BEGIN
      state.ready = 0
   ENDELSE

   IF ( state.ready ) THEN BEGIN

;+
; If the view is set to the emssivity plot area, then activate the
; 'Save' button so the current computed emissivity might be saved
;-
      saveButton = $
         WIDGET_INFO( event.TOP, $
                      FIND_BY_UNAME="saveButton" )
      IF ( state.view EQ 0 ) THEN BEGIN
         WIDGET_CONTROL, saveButton, $
                         SENSITIVE=1
      ENDIF ELSE BEGIN
         WIDGET_CONTROL, saveButton, $
                         SENSITIVE=0
      ENDELSE

;+
; Determine the current tab selected and set the active window to 
; the correct value
;-
      plotArea = [ 'emissivityPlotArea', $
                   'squaredSecondDerivativePlotArea', $
                   'smoothnessPlotArea' ]
      plotAreaID = $
         WIDGET_INFO( event.TOP, $
                      FIND_BY_UNAME=plotArea[state.view] )
      WIDGET_CONTROL, plotAreaID, $
                      GET_VALUE=windowNumber
      WSET, windowNumber

;+
; If the template for the sample file has not yet been defined, do
; so before continuing - if it has, use the template that is stored
; in the state variable
;-
      IF ( NOT state.sampleTemplate ) THEN BEGIN
         state.sampleTemplate = $
            PTR_NEW( ASCII_TEMPLATE( state.sampleFilename, CANCEL=cancelled ) )
         IF ( cancelled ) THEN RETURN
         WIDGET_CONTROL, event.TOP, $
                         SET_UVALUE=state
      ENDIF

;+
; If the template for the downwelling file has not yet been defined, do
; so before continuing - if it has, use the template that is stored
; in the state variable
;-
      IF ( NOT state.downwellingTemplate ) THEN BEGIN
         state.downwellingTemplate = $
            PTR_NEW( ASCII_TEMPLATE( state.downwellingFilename, CANCEL=cancelled ) )
         IF ( cancelled ) THEN RETURN
         WIDGET_CONTROL, event.TOP, $
                         SET_UVALUE=state
      ENDIF

;+
; Read the sample wavelength and radiance data from the specified file
; and parse out the individual columns
;-
      data = READ_ASCII( state.sampleFilename, $
                         TEMPLATE=*(state.sampleTemplate) )
      sWavelength = data.FIELD1
      sample = data.FIELD2

;+
; Read the downwelling wavelength and radiance data from the specified file
; and parse out the individual columns
;-
      data = READ_ASCII( state.downwellingFilename, $
                         TEMPLATE=*(state.downwellingTemplate) )
      dWavelength = data.FIELD1
      downwelling = data.FIELD2

;+
; Make certain that the wavelength centers defined for the samples in each
; radiance file are identical - if so create a single wavelength array and
; continue - if not then report error to the user and return
;-
      IF ( TOTAL( ABS( sWavelength - dWavelength ) ) GT 0 ) THEN BEGIN
         errorMessage = 'The wavelength data in the selected files is inconsistent'
         result = DIALOG_MESSAGE( errorMessage, /ERROR, DIALOG_PARENT=event.TOP )
         RETURN
      ENDIF ELSE BEGIN
         wavelength = sWavelength
      ENDELSE

;+
; Parse all of the user defined input strings from the textboxes and
; convert them to double precision floating point values an place them
; in the appropriate structures
;-
      smoothnessIntervalLowerTextbox = $
         WIDGET_INFO( event.TOP, $
                      FIND_BY_UNAME='smoothnessIntervalLowerTextbox' )
      WIDGET_CONTROL, smoothnessIntervalLowerTextbox, $
                      GET_VALUE=smoothnessIntervalLowerWavelength
      smoothnessIntervalUpperTextbox = $
         WIDGET_INFO( event.TOP, $
                      FIND_BY_UNAME='smoothnessIntervalUpperTextbox' )
      WIDGET_CONTROL, smoothnessIntervalUpperTextbox, $
                      GET_VALUE=smoothnessIntervalUpperWavelength
      smoothnessInterval = $
         { lowerWavelength:DOUBLE( smoothnessIntervalLowerWavelength[0] ), $
           upperWavelength:DOUBLE( smoothnessIntervalUpperWavelength[0] ) }

      temperatureIntervalLowerTextbox = $
         WIDGET_INFO( event.TOP, $
                      FIND_BY_UNAME='temperatureIntervalLowerTextbox' )
      WIDGET_CONTROL, temperatureIntervalLowerTextbox, $
                      GET_VALUE=lowerTemperature
      temperatureIntervalUpperTextbox = $
         WIDGET_INFO( event.TOP, $
                      FIND_BY_UNAME='temperatureIntervalUpperTextbox' )
      WIDGET_CONTROL, temperatureIntervalUpperTextbox, $
                      GET_VALUE=upperTemperature
      temperatureInterval = $
         { lowerTemperature:DOUBLE( lowerTemperature[0] ), $
           upperTemperature:DOUBLE( upperTemperature[0] ) }

      plotIntervalLowerTextbox = $
         WIDGET_INFO( event.TOP, $
                      FIND_BY_UNAME='plotIntervalLowerTextbox' )
      WIDGET_CONTROL, plotIntervalLowerTextbox, $
                      GET_VALUE=plotIntervalLowerWavelength
      plotIntervalUpperTextbox = $
         WIDGET_INFO( event.TOP, $
                      FIND_BY_UNAME='plotIntervalUpperTextbox' )
      WIDGET_CONTROL, plotIntervalUpperTextbox, $
                      GET_VALUE=plotIntervalUpperWavelength
      plotInterval = $
         { lowerWavelength:DOUBLE( plotIntervalLowerWavelength[0] ), $
           upperWavelength:DOUBLE( plotIntervalUpperWavelength[0] ) }

      knownTemperatureTextbox = $
         WIDGET_INFO( event.TOP, $
                      FIND_BY_UNAME='knownTemperatureTextbox' )
      WIDGET_CONTROL, knownTemperatureTextbox, $
                      GET_VALUE=knownTemperature
      knownTemperature = DOUBLE( knownTemperature[0] )

;+
; Sort the sample and downwelling radiance values in order of 
; increasing wavelenth
;-
      index = SORT( wavelength )
      wavelength = wavelength[index]
      sample = sample[index]
      downwelling = downwelling[index]

;+
; Choose the appropriate processing to determine temperature based
; on the selected method
;-
      CASE state.method OF
         'smoothness': $
            BEGIN

;+
; Find the indices of the spectral elements that define the lower and upper
; limit of the interval over which the spectral smoothness will be examined
;-
               minimumDifference = $
                  MIN( ABS( wavelength - smoothnessInterval.lowerWavelength ), lowerIndex )
               minimumDifference = $
                  MIN( ABS( wavelength - smoothnessInterval.upperWavelength ), upperIndex )

;+
; Compute the candidate reduced spectral emissivity curves for each
; temperature in the interval specified, then for each curve compute
; the average squared second derivative in the specified smoothness
; interval
;-
               temperatureIncrement = 0.1D
               numberTemperatures = $
                  LONG( ( temperatureInterval.upperTemperature - $
                          temperatureInterval.lowerTemperature ) / $
                        temperatureIncrement ) + 1
               temperature = DINDGEN( numberTemperatures ) / (numberTemperatures - 1) * $
                             ( temperatureInterval.upperTemperature - $
                               temperatureInterval.lowerTemperature ) + $
                             temperatureInterval.lowerTemperature

               averageSquaredDerivative = DBLARR( numberTemperatures )

               FOR temperatureIndex = 0, numberTemperatures-1 DO BEGIN
                  blackbody = BB_RADIANCE( temperature[temperatureIndex], wavelength )
                  emissivity = ( sample - downwelling ) / ( blackbody - downwelling )
                  averageSquaredDerivative[temperatureIndex] = $
                     MEAN( DERIV( DERIV( emissivity[lowerIndex:upperIndex] ) )^2 )

;+
; Plot the emissivity over the smoothness evaluation interval if the
; 'Smoothness Search' tab is selected
;-
                  IF ( state.view EQ 2 ) THEN BEGIN
                     PLOT, wavelength[lowerIndex:upperIndex], $
                           emissivity[lowerIndex:upperIndex], $
                           TITLE  = 'Temperature = ' + $
                                    STRCOMPRESS( STRING( temperature[temperatureIndex], $
                                                         FORMAT='(F7.3)' ), $
                                                 /REMOVE_ALL ) + $
                                    'K', $
                           XRANGE = [smoothnessInterval.lowerWavelength, $
                                     smoothnessInterval.upperWavelength], $
                           XSTYLE = 1, $
                           XTITLE = 'Wavelength (microns)', $
                           YTITLE = 'Emissivity'
                     WAIT, 0.020
                  ENDIF
               ENDFOR

;+
; Determine the temperature that produced the smoothest spectra in 
; the specified smoothness interval
;-
               minimumAverageSquaredDerivative = $
                  MIN( averageSquaredDerivative, temperatureIndex )
               sampleTemperature = temperature[temperatureIndex]

;+
; Plot the average squared second derivative as a function of temperature if the
; 'Average Squared Second Derivative Plot' tab is selected
;-
               IF ( state.view EQ 1 ) THEN BEGIN
                  PLOT, temperature, $
                        averageSquaredDerivative, $
                        /YLOG, $
                        TITLE  = 'Temperature Producing Maximum Smoothness = ' + $
                                 STRCOMPRESS( STRING( sampleTemperature, $
                                                      FORMAT='(F7.3)' ), $
                                              /REMOVE_ALL ) + $
                                 'K', $
                        XRANGE = [temperatureInterval.lowerTemperature, $
                                  temperatureInterval.upperTemperature], $
                        XSTYLE = 1, $
                        XTITLE = 'Temperature (K)', $
                        YTITLE = 'Average Squared Second Derivative'
               ENDIF
            END

         'known': $
            BEGIN
               sampleTemperature = knownTemperature            
            END

      ENDCASE

;+
; Produce the final reduced spectral emissivity at the determined
; sample temperature
;-
      blackbody = BB_RADIANCE( sampleTemperature, wavelength )
      emissivity = ( sample - downwelling ) / ( blackbody - Downwelling )

;+
; Plot the emissivity determined at the computed sample temperature over 
; the specified plot interval if the 'Emissivity Plot' tab is selected
;-
      IF ( state.view EQ 0 ) THEN BEGIN
         minimumDifference = $
            MIN( ABS( wavelength - plotInterval.lowerWavelength ), lowerIndex )
         minimumDifference = $
            MIN( ABS( wavelength - plotInterval.upperWavelength ), upperIndex )
         PLOT, wavelength[lowerIndex:upperIndex], $
               emissivity[lowerIndex:upperIndex], $
               TITLE  = 'Sample Temperature = ' + $
                        STRCOMPRESS( STRING( sampleTemperature, $
                                             FORMAT='(F7.3)' ), $
                                     /REMOVE_ALL ) + $
                        'K', $
               XRANGE = [plotInterval.lowerWavelength, $
                         plotInterval.upperWavelength], $
               XSTYLE = 1, $
               XTITLE = 'Wavelength (microns)', $
               YRANGE = [0, 1.2], $
               YSTYLE = 1, $
               YTITLE = 'Emissivity'
      ENDIF

;+
; Save the computed emissivity determined at the computed sample temperature over 
; the entire wavelength range if the 'Save' button has been pressed
;-
      IF ( savePressed ) THEN BEGIN
         filename = $
            DIALOG_PICKFILE( DIALOG_PARENT=event.TOP, $
                             TITLE='Please select an output emissivity file', $
                             FILTER='*.ems;*.EMS', $
                             /FIX_FILTER, $
                             PATH=state.directoryPath, $
                             /OVERWRITE_PROMPT, $
                             FILE=FILE_BASENAME( state.sampleFilename ) + '.ems' )
         IF ( filename EQ '' ) THEN RETURN
         OPENW, LUN, filename, /GET_LUN
         PRINTF, LUN, 'Wavelength (microns), Emissivity'
         FOR wavelengthIndex = 0, N_ELEMENTS( wavelength )-1 DO BEGIN
            PRINTF, LUN, wavelength[wavelengthIndex], $
                         ', ', $
                         emissivity[wavelengthIndex]
         ENDFOR
         FREE_LUN, LUN
      ENDIF

   ENDIF

   WIDGET_CONTROL, event.TOP, $
                   SET_UVALUE=state

END



PRO TES_gui

   state = $
      { $
         plotWidth:640, $
         plotHeight:480, $
         directoryPath:'', $
         sampleFilename:'', $
         sampleTemplate:PTR_NEW(), $
         downwellingFilename:'', $
         downwellingTemplate:PTR_NEW(), $
         method:'smoothness', $
         ready:0L, $
         view:0L $
      }


   mainBase = $
      WIDGET_BASE( TITLE='TES - Temperature Emissivity Separation', $
                   COLUMN=1, $
                   UVALUE=state, $
                   TLB_FRAME_ATTR=1 )


      filenameBase = $
         WIDGET_BASE( mainBase, $
                      /ALIGN_CENTER, $
                      COLUMN=1, $
                      FRAME=0, $
                      MAP=1 )

         sampleFilenameBase = $
            WIDGET_BASE( filenameBase, $
                         ROW=1, $
                         FRAME=0, $
                         MAP=1, $
                         /ALIGN_RIGHT )

            sampleFilenameLabel = $
               WIDGET_LABEL( sampleFilenameBase, $
                             VALUE='Sample Radiance File' )

            sampleFilenameTextbox = $
               WIDGET_TEXT( sampleFilenameBase, $
                            UVALUE='sampleFilenameTextbox', $
                            UNAME='sampleFilenameTextbox', $
                            XSIZE=50, $
                            /EDITABLE )

            sampleFilenameButton = $
               WIDGET_BUTTON( sampleFilenameBase, $
                              UVALUE='sampleFilenameButton', $
                              VALUE='Browse' )

         downwellingFilenameBase = $
            WIDGET_BASE( filenameBase, $
                         ROW=1, $
                         FRAME=0, $
                         MAP=1, $
                         /ALIGN_RIGHT )

            downwellingFilenameLabel = $
               WIDGET_LABEL( downwellingFilenameBase, $
                             VALUE='Downwelling Radiance File' )

            downwellingFilenameTextbox = $
               WIDGET_TEXT( downwellingFilenameBase, $
                            UVALUE='downwellingFilenameTextbox', $
                            UNAME='downwellingFilenameTextbox', $
                            XSIZE=50, $
                            /EDITABLE )

            downwellingFilenameButton = $
               WIDGET_BUTTON( downwellingFilenameBase, $
                              UVALUE='downwellingFilenameButton', $
                              VALUE='Browse' )


      parametersBase = $
         WIDGET_BASE( mainBase, $
                      UNAME='parametersBase', $
                      /ALIGN_CENTER, $
                      COL=1, $
                      FRAME=1, $
                      MAP=1 )

         smoothnessIntervalBase = $
            WIDGET_BASE( parametersBase, $
                         /ALIGN_RIGHT, $
                         ROW=1, $
                         FRAME=0, $
                         MAP=1 )

            smoothnessIntervalLabel = $
               WIDGET_LABEL( smoothnessIntervalBase, $
                             VALUE='Smoothness evaluation wavelength interval (lower,upper [microns])' )

            smoothnessIntervalLowerTextbox = $
               WIDGET_TEXT( smoothnessIntervalBase, $
                            UVALUE='smoothnessIntervalLowerTextbox', $
                            UNAME='smoothnessIntervalLowerTextbox', $
                            XSIZE=6, $
                            VALUE='8.12', $
                            /EDITABLE )

            smoothnessIntervalUpperTextbox = $
               WIDGET_TEXT( smoothnessIntervalBase, $
                            UVALUE='smoothnessIntervalUpperTextbox', $
                            UNAME='smoothnessIntervalUpperTextbox', $
                            XSIZE=6, $
                            VALUE='8.6', $
                            /EDITABLE )

         temperatureIntervalBase = $
            WIDGET_BASE( parametersBase, $
                         /ALIGN_RIGHT, $
                         ROW=1, $
                         FRAME=0, $
                         MAP=1 )

            temperatureIntervalLabel = $
               WIDGET_LABEL( temperatureIntervalBase, $
                             VALUE='Temperature interval to search over (lower,upper [K])' )

            temperatureIntervalLowerTextbox = $
               WIDGET_TEXT( temperatureIntervalBase, $
                            UVALUE='temperatureIntervalLowerTextbox', $
                            UNAME='temperatureIntervalLowerTextbox', $
                            XSIZE=6, $
                            VALUE='280', $
                            /EDITABLE )

            temperatureIntervalUpperTextbox = $
               WIDGET_TEXT( temperatureIntervalBase, $
                            UVALUE='temperatureIntervalUpperTextbox', $
                            UNAME='temperatureIntervalUpperTextbox', $
                            XSIZE=6, $
                            VALUE='360', $
                            /EDITABLE )

         plotIntervalBase = $
            WIDGET_BASE( parametersBase, $
                         /ALIGN_RIGHT, $
                         ROW=1, $
                         FRAME=0, $
                         MAP=1 )

            plotIntervalLabel = $
               WIDGET_LABEL( plotIntervalBase, $
                             VALUE='Wavelength limits for final emissivity plot (lower,upper [microns])' )

            plotIntervalLowerTextbox = $
               WIDGET_TEXT( plotIntervalBase, $
                            UVALUE='plotIntervalLowerTextbox', $
                            UNAME='plotIntervalLowerTextbox', $
                            XSIZE=6, $
                            VALUE='8', $
                            /EDITABLE )

            plotIntervalUpperTextbox = $
               WIDGET_TEXT( plotIntervalBase, $
                            UVALUE='plotIntervalUpperTextbox', $
                            UNAME='plotIntervalUpperTextbox', $
                            XSIZE=6, $
                            VALUE='14', $
                            /EDITABLE )

      methodBase = $
         WIDGET_BASE( mainBase, $
                      /ALIGN_CENTER, $
                      ROW=1, $
                      FRAME=0, $
                      MAP=1 )

         methodButtonBase = $
            WIDGET_BASE( methodBase, $
                         /ALIGN_CENTER, $
                         ROW=1, $
                         /EXCLUSIVE, $
                         FRAME=0, $
                         MAP=1 )

            smoothnessButton = $
               WIDGET_BUTTON( methodButtonBase, $
                              UVALUE='smoothnessButton', $
                              VALUE='Spectral Smoothness', $
                              /NO_RELEASE )

            knownButton = $
               WIDGET_BUTTON( methodButtonBase, $
                              UVALUE='knownButton', $
                              VALUE='Known Temperature [K]', $
                              /NO_RELEASE )

            CASE state.method OF
               'smoothness': WIDGET_CONTROL, smoothnessButton, $
                                             SET_BUTTON=1
               'known': WIDGET_CONTROL, knownButton, $
                                        SET_BUTTON=1
            ENDCASE

         knownTemperatureTextbox = $
            WIDGET_TEXT( methodBase, $
                         UVALUE='knownTemperatureTextbox', $
                         UNAME='knownTemperatureTextbox', $
                         XSIZE=6, $
                         VALUE='300', $
                         SENSITIVE=0, $
                         /EDITABLE )

      plotBase = $
         WIDGET_TAB( mainBase, $
                     UVALUE='plotBase', $
                     UNAME='plotBase' )

         emissivityPlotBase = $
            WIDGET_BASE( plotBase, $
                         TITLE='Emissivity Plot', $
                         UVALUE='emissivityPlotBase', $
                         COLUMN=1, $
                         FRAME=0, $
                         MAP=1 )

            emissivityPlotArea = $
               WIDGET_DRAW( emissivityPlotBase, $
                            UNAME='emissivityPlotArea', $
                            XSIZE=state.plotWidth, $
                            YSIZE=state.plotHeight, $
                            RETAIN=2 )

         squaredSecondDerivativePlotBase = $
            WIDGET_BASE( plotBase, $
                         TITLE='Average Squared Second Derivative Plot', $
                         UVALUE='squaredSecondDerivativePlotBase', $
                         COLUMN=1, $
                         FRAME=0, $
                         MAP=1 )

            squaredSecondDerivativePlotArea = $
               WIDGET_DRAW( squaredSecondDerivativePlotBase, $
                            UNAME='squaredSecondDerivativePlotArea', $
                            XSIZE=state.plotWidth, $
                            YSIZE=state.plotHeight, $
                            RETAIN=2 )

         smoothnessPlotBase = $
            WIDGET_BASE( plotBase, $
                         TITLE='Smoothness Search', $
                         UVALUE='smoothnessPlotBase', $
                         COLUMN=1, $
                         FRAME=0, $
                         MAP=1 )

            smoothnessPlotArea = $
               WIDGET_DRAW( smoothnessPlotBase, $
                            UNAME='smoothnessPlotArea', $
                            XSIZE=state.plotWidth, $
                            YSIZE=state.plotHeight, $
                            RETAIN=2 )


      actionBase = $
         WIDGET_BASE( mainBase, $
                      /ALIGN_CENTER, $
                      ROW=1, $
                      FRAME=0, $
                      MAP=1 )

         saveButton = $
            WIDGET_BUTTON( actionBase, $
                           UVALUE='saveButton', $
                           UNAME='saveButton', $
                           SENSITIVE=0, $
                           VALUE='Save' )

         quitButton = $
            WIDGET_BUTTON( actionBase, $
                           UVALUE='quitButton', $
                           VALUE='Quit' )


   WIDGET_CONTROL, mainBase, $
                   /REALIZE

   XMANAGER, 'TES', mainBase

END



PRO TES
   TES_gui
END