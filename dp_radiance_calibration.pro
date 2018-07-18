;+
; :NAME:
;    DP_RADIANCE_CALIBRATION
;
; :PURPOSE:
;    This user-interactive procedure will produce calibrated radiance files from
;    the sample and downwelling measurement files created by the D&P Instruments
;    Model 102F MicroFT or the Model 202 TurboFT.  This calibration requires the
;    cold and warm blackbody files produced by the instrument also be provided.
;    Sample data is provided 
;       <a href="media/software/dp_radiance_calibration_data.zip">here</a>.
;
; :CATEGORY:
;    Radiometry.
;
; :CALLING SEQUENCE:
;    DP_RADIANCE_CALIBRATION
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
;    READ_DP_FILE (included)
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    June, 2012        Original code
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

PRO DP_RADIANCE_CALIBRATION_COMPUTE, state

   createASCII = 0
   createIndividual = 0
   createPlots = 0

   IF ( state.runMode EQ 2 ) THEN createASCII = 1
   IF ( state.runMode EQ 3 ) THEN createIndividual = 1
   IF ( state.runMode EQ 4 ) THEN createPlots = 1


   red   = [ 0, 1, 0, 0, 1, 1, 0, 1 ]
   green = [ 0, 0, 1, 0, 1, 0, 1, 1 ]
   blue  = [ 0, 0, 0, 1, 0, 1, 1, 1 ]

   DEVICE, DECOMPOSED=0
   TVLCT, 255*red, 255*green, 255*blue


   IF ( state.cbbFilename EQ '(none)' ) THEN BEGIN
      status = DIALOG_MESSAGE( 'Cold blackbody filename not supplied', /ERROR )
      RETURN
   ENDIF

   IF ( state.wbbFilename EQ '(none)' ) THEN BEGIN
      status = DIALOG_MESSAGE( 'Warm blackbody filename not supplied', /ERROR )
      RETURN
   ENDIF

   IF ( state.samFilename EQ '(none)' ) THEN BEGIN
      status = DIALOG_MESSAGE( 'Sample filename not supplied', /ERROR )
      RETURN
   ENDIF

   IF ( state.dwrFilename EQ '(none)' ) THEN BEGIN
      downwellingProvided = 0
   ENDIF ELSE BEGIN
      downwellingProvided = 1
   ENDELSE


   READ_DP_FILE, state.cbbFilename, $
                 state.instrumentModel, $
                 HEADER=cbbHeader, $
                 SPECTRUM=cbbSpectrum, $
                 INTERFEROGRAM=cbbInterferogram

   READ_DP_FILE, state.wbbFilename, $
                 state.instrumentModel, $
                 HEADER=wbbHeader, $
                 SPECTRUM=wbbSpectrum, $
                 INTERFEROGRAM=wbbInterferogram

   READ_DP_FILE, state.samFilename, $
                 state.instrumentModel, $
                 HEADER=sampleHeader, $
                 SPECTRUM=sampleSpectrum, $
                 INTERFEROGRAM=sampleInterferogram


   numberSpectrumPoints = sampleHeader.ZEROFILL * 2048

   IF ( (NOT createASCII) AND (NOT createIndividual) AND (NOT createPlots) ) THEN BEGIN

      IF ( state.diagnosticRaw ) THEN BEGIN
         PLOT, cbbSpectrum.Wavelength, $
               cbbSpectrum.Value, $
               XTITLE='Wavelength (microns)', $
               YTITLE='Raw Instrument Values', $
               XSTYLE=1, $
               XRANGE=[1.5,15], $
               YSTYLE=1, $
               YRANGE=[0,0.03], $
               /NODATA
         OPLOT, cbbSpectrum.Wavelength, $
                cbbSpectrum.Value, $
                COLOR=3
         OPLOT, wbbSpectrum.Wavelength, $
                wbbSpectrum.Value, $
                COLOR=1
         OPLOT, sampleSpectrum.Wavelength, $
                sampleSpectrum.Value, $
                COLOR=7
      ENDIF

   ENDIF

   IF ( downwellingProvided ) THEN BEGIN

      READ_DP_FILE, state.dwrFilename, $
                    state.instrumentModel, $
                    HEADER=downwellingHeader, $
                    SPECTRUM=downwellingSpectrum, $
                    INTERFEROGRAM=downwellingInterferogram

      IF ( (NOT createASCII) AND (NOT createIndividual) AND (NOT createPlots) ) THEN BEGIN
         IF ( state.diagnosticRaw ) THEN BEGIN
            OPLOT, downwellingSpectrum.Wavelength, $
                   downwellingSpectrum.Value, $
                   COLOR=6
         ENDIF
      ENDIF
   ENDIF

   IF ( (NOT createASCII) AND (NOT createIndividual) AND (NOT createPlots) ) THEN BEGIN
      IF ( state.diagnosticRaw ) THEN BEGIN
         status = DIALOG_MESSAGE( 'Click OK to continue execution', /INFORMATION )
      ENDIF
   ENDIF

   coldBlackbody = BB_RADIANCE( cbbHeader.CBBTemperature + 273.15, $
                                cbbSpectrum.Wavelength, $
                                UNITS=0 )
   warmBlackbody = BB_RADIANCE( wbbHeader.WBBTemperature + 273.15, $
                                wbbSpectrum.Wavelength, $
                                UNITS=0 )

   wbbSpectrum.Value[0] = 1
   wbbSpectrum.Value[2047] = 1
   calibrationSlope = (warmBlackbody - coldBlackbody) / $
                      (wbbSpectrum.Value - cbbSpectrum.Value)
   calibrationOffset = warmBlackbody - wbbSpectrum.Value * calibrationSlope

   cbbSpectrum.Value = calibrationSlope * cbbSpectrum.Value + calibrationOffset
   wbbSpectrum.Value = calibrationSlope * wbbSpectrum.Value + calibrationOffset
   sampleSpectrum.Value = calibrationSlope * sampleSpectrum.Value + calibrationOffset

   IF ( downwellingProvided ) THEN BEGIN
      downwellingSpectrum.Value = calibrationSlope * downwellingSpectrum.Value + calibrationOffset

      plateTemperature = downwellingHeader.SpareF[0]

      IF ( state.plateEmissivity LT 0 ) THEN BEGIN
         state.plateEmissivity = downwellingHeader.SpareF[1]
      ENDIF
      plateEmissivity = state.plateEmissivity

      plateEmission = plateEmissivity * BB_RADIANCE( plateTemperature + 273.15, $
                                                     downwellingSpectrum.Wavelength, $
                                                     UNITS=0 )

      downwellingSpectrum.Value = (downwellingSpectrum.Value - plateEmission) / $
                                  (1 - plateEmissivity)

   ENDIF


   individualWavelength = FLTARR(2048)
   individualSlope = FLTARR(2048)
   individualOffset = FLTARR(2048)
   FOR iWavelength = 0, 2047 DO BEGIN
      individualWavelength[iWavelength] = sampleSpectrum.Wavelength[iWavelength*sampleHeader.ZEROFILL]
      individualSlope[iWavelength] = calibrationSlope[iWavelength*sampleHeader.ZEROFILL]
      individualOffset[iWavelength] = calibrationOffset[iWavelength*sampleHeader.ZEROFILL]
   ENDFOR


   individualCBB = REPLICATE( { spectra:FLTARR(2048) }, cbbHeader.NumberOfCoAdds )

   FOR scan = 0, cbbHeader.NumberOfCoadds-1 DO BEGIN
      iMaximum = MAX( cbbInterferogram[scan].Value, iMaxIndex )
      iMinimum = MIN( cbbInterferogram[scan].Value, iMinIndex )
      IF ( ABS( iMinimum ) LT ABS( iMaximum ) ) THEN BEGIN
         iCenterBurst = iMaxIndex
      ENDIF ELSE BEGIN
         iCenterBurst = iMinIndex
      ENDELSE

      interferogramShift = cbbHeader.InterferogramSize/2 - iCenterBurst
      centeredInterferogram = SHIFT( cbbInterferogram[scan].Value, interferogramShift )
      centeredInterferogram = centeredInterferogram[cbbHeader.InterferogramSize/2-2048: $
                                                    cbbHeader.InterferogramSize/2+2047]

      windowFunction = HANNING( 4096 )
      spectra = FFT( centeredInterferogram * windowFunction )
      spectra = spectra / 3300  ;  WHAT MAKES UP THIS FACTOR ?????????
      individualCBB[scan].spectra = individualSlope * ABS( spectra[0:2047] ) + individualOffset

      IF ( (NOT createASCII) AND (NOT createIndividual) AND (NOT createPlots) ) THEN BEGIN
         IF ( state.diagnosticSpectra ) THEN BEGIN
            PLOT, cbbSpectrum.Wavelength, $
                  cbbSpectrum.Value, $
                  TITLE='Cold Blackbody', $
                  XTITLE='Wavelength (microns)', $
                  YTITLE='Radiance (W/m^2/sr/micron)', $
                  XSTYLE=1, $
                  XRANGE=[1.5,15], $
                  YSTYLE=1, $
                  YRANGE=[0,25], $
                  BACKGROUND=0, $
                  COLOR=7, $
                  /NODATA
            OPLOT, cbbSpectrum.Wavelength, $
                   cbbSpectrum.Value, $
                   COLOR=3
            OPLOT, individualWavelength, $
                   individualCBB[scan].spectra, $
                   COLOR=7
            WAIT, 0.5
         ENDIF

         IF ( state.diagnosticInterferogram ) THEN BEGIN
            PLOT, centeredInterferogram, $
                  TITLE='Cold Blackbody', $
                  XTITLE='Array Position', $
                  YTITLE='Interferogram', $
                  XSTYLE=1, $
                  XRANGE=[1800,2200], $
                  YSTYLE=1, $
                  YRANGE=[-10000,10000], $
                  BACKGROUND=0, $
                  COLOR=7, $
                  /NODATA
            OPLOT, centeredInterferogram, $
                   COLOR=3
            WAIT, 0.5
         ENDIF
      ENDIF

   ENDFOR

   IF ( (NOT createASCII) AND (NOT createIndividual) AND (NOT createPlots) ) THEN BEGIN
      IF ( state.diagnosticSpectra OR $
           state.diagnosticInterferogram ) THEN BEGIN
         status = DIALOG_MESSAGE( 'Click OK to continue execution', /INFORMATION )
      ENDIF
   ENDIF


   individualWBB = REPLICATE( { spectra:FLTARR(2048) }, wbbHeader.NumberOfCoAdds )

   FOR scan = 0, wbbHeader.NumberOfCoadds-1 DO BEGIN
      iMaximum = MAX( wbbInterferogram[scan].Value, iMaxIndex )
      iMinimum = MIN( wbbInterferogram[scan].Value, iMinIndex )
      IF ( ABS( iMinimum ) LT ABS( iMaximum ) ) THEN BEGIN
         iCenterBurst = iMaxIndex
      ENDIF ELSE BEGIN
         iCenterBurst = iMinIndex
      ENDELSE

      interferogramShift = wbbHeader.InterferogramSize/2 - iCenterBurst
      centeredInterferogram = SHIFT( wbbInterferogram[scan].Value, interferogramShift )
      centeredInterferogram = centeredInterferogram[wbbHeader.InterferogramSize/2-2048: $
                                                    wbbHeader.InterferogramSize/2+2047]

      windowFunction = HANNING( 4096 )
      spectra = FFT( centeredInterferogram * windowFunction )
      spectra = spectra / 3300  ;  WHAT MAKES UP THIS FACTOR ?????????
      individualWBB[scan].spectra = individualSlope * ABS( spectra[0:2047] ) + individualOffset

      IF ( (NOT createASCII) AND (NOT createIndividual) AND (NOT createPlots) ) THEN BEGIN
         IF ( state.diagnosticSpectra ) THEN BEGIN
            PLOT, wbbSpectrum.Wavelength, $
                  wbbSpectrum.Value, $
                  TITLE='Warm Blackbody', $
                  XTITLE='Wavelength (microns)', $
                  YTITLE='Radiance (W/m^2/sr/micron)', $
                  XSTYLE=1, $
                  XRANGE=[1.5,15], $
                  YSTYLE=1, $
                  YRANGE=[0,25], $
                  BACKGROUND=0, $
                  COLOR=7, $
                  /NODATA
            OPLOT, wbbSpectrum.Wavelength, $
                   wbbSpectrum.Value, $
                   COLOR=1
            OPLOT, individualWavelength, $
                   individualWBB[scan].spectra, $
                   COLOR=7
            WAIT, 0.5
         ENDIF

         IF ( state.diagnosticInterferogram ) THEN BEGIN
            PLOT, centeredInterferogram, $
                  TITLE='Warm Blackbody', $
                  XTITLE='Array Position', $
                  YTITLE='Interferogram', $
                  XSTYLE=1, $
                  XRANGE=[1800,2200], $
                  YSTYLE=1, $
                  YRANGE=[-10000,10000], $
                  BACKGROUND=0, $
                  COLOR=7, $
                  /NODATA
            OPLOT, centeredInterferogram, $
                   COLOR=1
            WAIT, 0.5
         ENDIF
      ENDIF

   ENDFOR

   IF ( (NOT createASCII) AND (NOT createIndividual) AND (NOT createPlots) ) THEN BEGIN
      IF ( state.diagnosticSpectra OR $
           state.diagnosticInterferogram ) THEN BEGIN
         status = DIALOG_MESSAGE( 'Click OK to continue execution', /INFORMATION )
      ENDIF
   ENDIF


   individualSample = REPLICATE( { spectra:FLTARR(2048) }, sampleHeader.NumberOfCoAdds )

   FOR scan = 0, sampleHeader.NumberOfCoadds-1 DO BEGIN
      iMaximum = MAX( sampleInterferogram[scan].Value, iMaxIndex )
      iMinimum = MIN( sampleInterferogram[scan].Value, iMinIndex )
      IF ( ABS( iMinimum ) LT ABS( iMaximum ) ) THEN BEGIN
         iCenterBurst = iMaxIndex
      ENDIF ELSE BEGIN
         iCenterBurst = iMinIndex
      ENDELSE

      interferogramShift = sampleHeader.InterferogramSize/2 - iCenterBurst
      centeredInterferogram = SHIFT( sampleInterferogram[scan].Value, interferogramShift )
      centeredInterferogram = centeredInterferogram[sampleHeader.InterferogramSize/2-2048: $
                                                    sampleHeader.InterferogramSize/2+2047]

      windowFunction = HANNING( 4096 )
      spectra = FFT( centeredInterferogram * windowFunction )
      spectra = spectra / 3300  ;  WHAT MAKES UP THIS FACTOR ?????????
      individualSample[scan].spectra = individualSlope * ABS( spectra[0:2047] ) + individualOffset

      IF ( (NOT createASCII) AND (NOT createIndividual) AND (NOT createPlots) ) THEN BEGIN
         IF ( state.diagnosticSpectra ) THEN BEGIN
            PLOT, sampleSpectrum.Wavelength, $
                  sampleSpectrum.Value, $
                  TITLE='Sample', $
                  XTITLE='Wavelength (microns)', $
                  YTITLE='Radiance (W/m^2/sr/micron)', $
                  XSTYLE=1, $
                  XRANGE=[1.5,15], $
                  YSTYLE=1, $
                  YRANGE=[0,25], $
                  BACKGROUND=0, $
                  COLOR=7, $
                  /NODATA
            OPLOT, sampleSpectrum.Wavelength, $
                   sampleSpectrum.Value, $
                   COLOR=7
            OPLOT, individualWavelength, $
                   individualSample[scan].spectra, $
                   COLOR=7
            WAIT, 0.5
         ENDIF

         IF ( state.diagnosticInterferogram ) THEN BEGIN
            PLOT, centeredInterferogram, $
                  TITLE='Sample', $
                  XTITLE='Array Position', $
                  YTITLE='Interferogram', $
                  XSTYLE=1, $
                  XRANGE=[1800,2200], $
                  YSTYLE=1, $
                  YRANGE=[-10000,10000], $
                  BACKGROUND=0, $
                  COLOR=7, $
                  /NODATA
            OPLOT, centeredInterferogram, $
                   COLOR=7
            WAIT, 0.5
         ENDIF
      ENDIF

   ENDFOR

   IF ( (NOT createASCII) AND (NOT createIndividual) AND (NOT createPlots) ) THEN BEGIN
      IF ( state.diagnosticSpectra OR $
           state.diagnosticInterferogram ) THEN BEGIN
         status = DIALOG_MESSAGE( 'Click OK to continue execution', /INFORMATION )
      ENDIF
   ENDIF


   IF ( downwellingProvided ) THEN BEGIN
      individualDownwelling = REPLICATE( { spectra:FLTARR(2048) }, downwellingHeader.NumberOfCoAdds )

      FOR scan = 0, downwellingHeader.NumberOfCoadds-1 DO BEGIN
         iMaximum = MAX( downwellingInterferogram[scan].Value, iMaxIndex )
         iMinimum = MIN( downwellingInterferogram[scan].Value, iMinIndex )
         IF ( ABS( iMinimum ) LT ABS( iMaximum ) ) THEN BEGIN
            iCenterBurst = iMaxIndex
         ENDIF ELSE BEGIN
            iCenterBurst = iMinIndex
         ENDELSE

         interferogramShift = downwellingHeader.InterferogramSize/2 - iCenterBurst
         centeredInterferogram = SHIFT( downwellingInterferogram[scan].Value, interferogramShift )
         centeredInterferogram = centeredInterferogram[downwellingHeader.InterferogramSize/2-2048: $
                                                       downwellingHeader.InterferogramSize/2+2047]

         windowFunction = HANNING( 4096 )
         spectra = FFT( centeredInterferogram * windowFunction )
         spectra = spectra / 3300  ;  WHAT MAKES UP THIS FACTOR ?????????
         individualDownwelling[scan].spectra = individualSlope * ABS( spectra[0:2047] ) + individualOffset

         IF ( (NOT createASCII) AND (NOT createIndividual) AND (NOT createPlots) ) THEN BEGIN
            IF ( state.diagnosticSpectra ) THEN BEGIN
               PLOT, downwellingSpectrum.Wavelength, $
                     downwellingSpectrum.Value, $
                     TITLE='Downwelling Radiance', $
                     XTITLE='Wavelength (microns)', $
                     YTITLE='Radiance (W/m^2/sr/micron)', $
                     XSTYLE=1, $
                     XRANGE=[1.5,15], $
                     YSTYLE=1, $
                     YRANGE=[0,25], $
                     BACKGROUND=0, $
                     COLOR=7, $
                     /NODATA
               OPLOT, downwellingSpectrum.Wavelength, $
                      downwellingSpectrum.Value, $
                      COLOR=6
               OPLOT, individualWavelength, $
                      individualDownwelling[scan].spectra, $
                      COLOR=7
               WAIT, 0.5
            ENDIF

            IF ( state.diagnosticInterferogram ) THEN BEGIN
               PLOT, centeredInterferogram, $
                     TITLE='Downwelling Radiance', $
                     XTITLE='Array Position', $
                     YTITLE='Interferogram', $
                     XSTYLE=1, $
                     XRANGE=[1800,2200], $
                     YSTYLE=1, $
                     YRANGE=[-10000,10000], $
                     BACKGROUND=0, $
                     COLOR=7, $
                     /NODATA
               OPLOT, centeredInterferogram, $
                      COLOR=6
               WAIT, 0.5
            ENDIF
         ENDIF

      ENDFOR

      IF ( (NOT createASCII) AND (NOT createIndividual) AND (NOT createPlots) ) THEN BEGIN
         IF ( state.diagnosticSpectra OR $
              state.diagnosticInterferogram ) THEN BEGIN
            status = DIALOG_MESSAGE( 'Click OK to continue execution', /INFORMATION )
         ENDIF
      ENDIF

   ENDIF


   IF ( (createASCII EQ 0) AND (createPlots EQ 0) AND (createIndividual EQ 0) ) THEN BEGIN
      WHILE ( !D.WINDOW GE 0 ) DO WDELETE, !D.WINDOW
      WINDOW, 0

      PLOT, sampleSpectrum.Wavelength, $
            sampleSpectrum.Value, $
            XTITLE='Wavelength (microns)', $
            YTITLE='Radiance (W/m^2/sr/micron)', $
            XSTYLE=1, $
            XRANGE=[1.5,15], $
            YSTYLE=1, $
            YRANGE=[0,25], $
            BACKGROUND=0, $
            COLOR=7, $
            /NODATA

      OPLOT, cbbSpectrum.Wavelength, $
             cbbSpectrum.Value, $
             COLOR=3

      OPLOT, wbbSpectrum.Wavelength, $
             wbbSpectrum.Value, $
             COLOR=1

      OPLOT, sampleSpectrum.Wavelength, $
             sampleSpectrum.Value, $
             COLOR=7

      IF ( downwellingProvided ) THEN BEGIN
         OPLOT, downwellingSpectrum.Wavelength, $
                downwellingSpectrum.Value, $
                COLOR=6
      ENDIF
   ENDIF

   IF ( createASCII ) THEN BEGIN
      lastCharacter = STRLEN( state.samFilename ) - 1
      outputSampleFilename = state.samFilename
      STRPUT, outputSampleFilename, 'c', lastCharacter[0]

      OPENW, sLun, outputSampleFilename, /GET_LUN

      dataSource = $
         (STR_SEP( state.samFilename[0], '\' )) $
            [N_ELEMENTS( STR_SEP( state.samFilename[0], '\' ) ) - 1]

      PRINTF, sLun, $
              'Wavelength (microns), Radiance (W/m^2/sr/micron) [', $
              dataSource, $
              ']'
      FOR I = 0, numberSpectrumPoints-1, sampleHeader.ZEROFILL DO BEGIN
         PRINTF, sLun, sampleSpectrum.Wavelength[I], $
                       ',', $
                       sampleSpectrum.Value[I]
      ENDFOR

      CLOSE, sLun
      FREE_LUN, sLun

      IF ( downwellingProvided ) THEN BEGIN
         lastCharacter = STRLEN( state.dwrFilename ) - 1
         outputDownwellingFilename = state.dwrFilename
         STRPUT, outputDownwellingFilename, 'c', lastCharacter[0]

         OPENW, dLun, outputDownwellingFilename, /GET_LUN

         dataSource = $
            (STR_SEP( state.dwrFilename[0], '\' )) $
               [N_ELEMENTS( STR_SEP( state.dwrFilename[0], '\' ) ) - 1]

         PRINTF, dLun, $
                 'Wavelength (microns), Radiance (W/m^2/sr/micron) [', $
                 dataSource, $
                 ']'
         FOR I = 0, numberSpectrumPoints-1, downwellingHeader.ZEROFILL DO BEGIN
            PRINTF, dLun, downwellingSpectrum.Wavelength[I], $
                          ',', $
                          downwellingSpectrum.Value[I]
         ENDFOR

         CLOSE, dLun
         FREE_LUN, dLun
      ENDIF

      status = DIALOG_MESSAGE( 'Calibrated ASCII files created', /INFORMATION )

   ENDIF

   IF ( createIndividual ) THEN BEGIN

      lastCharacter = STRLEN( state.samFilename ) - 1
      outputSampleFilename = state.samFilename
      STRPUT, outputSampleFilename, 'c', lastCharacter[0]
      extensionPosition = STRPOS( outputSampleFilename, '.' )
      extension = STRMID( outputSampleFilename, $
                          extensionPosition[0], $
                          (lastCharacter[0] - extensionPosition[0] + 1) )
      outputSampleFilename = STRMID( outputSampleFilename, $
                                       0, $
                                       extensionPosition[0] ) + $
                               '_INDIVIDUAL' + $
                               extension

      OPENW, sLun, outputSampleFilename, /GET_LUN

      dataSource = $
         (STR_SEP( state.samFilename[0], '\' )) $
            [N_ELEMENTS( STR_SEP( state.samFilename[0], '\' ) ) - 1]

      headerString = 'Wavelength (microns), Radiance (W/m^2/sr/micron) [' + $
                      STRING( dataSource, /PRINT ) + $
                      '] (Individual Scans)'
      PRINTF, sLun, headerString
      FOR I = 0, 2047 DO BEGIN
         outputString = STRING( sampleSpectrum.Wavelength[I*sampleHeader.ZEROFILL] )
         FOR scan = 0, sampleHeader.NumberOfCoadds-1 DO BEGIN
            outputString = outputString + $
                            ',' + $
                            STRING( individualSample[scan].spectra[I], /PRINT )
         ENDFOR
         PRINTF, sLun, outputString
      ENDFOR

      CLOSE, sLun
      FREE_LUN, sLun

      status = DIALOG_MESSAGE( 'Calibrated ASCII files for individual scans created', /INFORMATION )

   ENDIF

   IF ( createPlots ) THEN BEGIN
      lastCharacter = STRLEN( state.samFilename ) - 1
      outputSampleFilename = state.samFilename
      STRPUT, outputSampleFilename, 'c', lastCharacter[0]
      legend = $
         (STR_SEP( outputSampleFilename[0], '\' )) $
            [N_ELEMENTS( STR_SEP( outputSampleFilename[0], '\' ) ) - 1]
      WINDOW, 0
      PLOT,  sampleSpectrum.Wavelength, $
             sampleSpectrum.Value, $
             XSTYLE=1, $
             XRANGE=[3.0,5.0], $
             YSTYLE=1, $
             YRANGE=[0.0,25.0], $
             XTITLE='Wavelength (microns)', $
             YTITLE='Radiance (W/m^2/sr/micron)', $
             TITLE=legend[0], $
             BACKGROUND=7, $
             COLOR=0
      WRITE_PNG, $
         STRMID( state.samFilename, 0, $
                 (STRLEN( state.samFilename ))[0] -4 ) + $
                    '_sac_mwir.png', $
         TVRD()

      WINDOW, 1
      PLOT,  sampleSpectrum.Wavelength, $
             sampleSpectrum.Value, $
             XSTYLE=1, $
             XRANGE=[8.0,14.0], $
             YSTYLE=1, $
             YRANGE=[0.0,25.0], $
             XTITLE='Wavelength (microns)', $
             YTITLE='Radiance (W/m^2/sr/micron)', $
             TITLE=legend[0], $
             BACKGROUND=7, $
             COLOR=0
      WRITE_PNG, $
         STRMID( state.samFilename, 0, $
                 (STRLEN( state.samFilename ))[0] -4 ) + $
                    '_sac_lwir.png', $
         TVRD()


      IF ( downwellingProvided ) THEN BEGIN
         lastCharacter = STRLEN( state.dwrFilename ) - 1
         outputDownwellingFilename = state.dwrFilename
         STRPUT, outputDownwellingFilename, 'c', lastCharacter[0]
         legend = $
            (STR_SEP( outputDownwellingFilename[0], '\' )) $
               [N_ELEMENTS( STR_SEP( outputDownwellingFilename[0], '\' ) ) - 1]
         WINDOW, 2
         PLOT,  downwellingSpectrum.Wavelength, $
                downwellingSpectrum.Value, $
                XSTYLE=1, $
                XRANGE=[3.0, 5.0], $
                YSTYLE=1, $
                YRANGE=[0.0, 25.0], $
                XTITLE='Wavelength (microns)', $
                YTITLE='Radiance (W/m^2/sr/micron)', $
                TITLE=legend[0], $
                BACKGROUND=7, $
                COLOR=0
         WRITE_PNG, $
            STRMID( state.dwrFilename, 0, $
                    (STRLEN( state.dwrFilename ))[0] -4 ) + $
                       '_dwc_mwir.png', $
            TVRD()

         WINDOW, 3
         PLOT,  downwellingSpectrum.Wavelength, $
                downwellingSpectrum.Value, $
                XSTYLE=1, $
                XRANGE=[8.0, 14.0], $
                YSTYLE=1, $
                YRANGE=[0.0, 25.0], $
                XTITLE='Wavelength (microns)', $
                YTITLE='Radiance (W/m^2/sr/micron)', $
                TITLE=legend[0], $
                BACKGROUND=7, $
                COLOR=0
         WRITE_PNG, $
            STRMID( state.dwrFilename, 0, $
                    (STRLEN( state.dwrFilename ))[0] -4 ) + $
                       '_dwc_lwir.png', $
            TVRD()
      ENDIF

      status = DIALOG_MESSAGE( 'Calibrated MWIR/LWIR plots created', /INFORMATION )

   ENDIF


   RETURN

END



PRO DP_RADIANCE_CALIBRATION_EVENT, event

   WIDGET_CONTROL, event.ID, GET_UVALUE=widget

   WIDGET_CONTROL, event.TOP, GET_UVALUE=state

   SWITCH widget OF

      'cbbButton': $
         BEGIN
            filename = DIALOG_PICKFILE( FILTER='*.cbb,*.CBB', $
                                        /FIX_FILTER, $
                                        PATH=state.lastDirectory, $
                                        GET_PATH=lastDirectory )
            fileTextbox = WIDGET_INFO( event.TOP, $
                                       FIND_BY_UNAME='cbbTextbox' )
            WIDGET_CONTROL, fileTextbox, SET_VALUE=filename
            state.cbbFilename = filename
            state.lastDirectory = lastDirectory
            BREAK
         END

      'wbbButton': $
         BEGIN
            filename = DIALOG_PICKFILE( FILTER='*.wbb,*.WBB', $
                                        /FIX_FILTER, $
                                        PATH=state.lastDirectory, $
                                        GET_PATH=lastDirectory )
            fileTextbox = WIDGET_INFO( event.TOP, $
                                       FIND_BY_UNAME='wbbTextbox' )
            WIDGET_CONTROL, fileTextbox, SET_VALUE=filename
            state.wbbFilename = filename
            state.lastDirectory = lastDirectory
            BREAK
         END

      'samButton': $
         BEGIN
            filename = DIALOG_PICKFILE( FILTER='*.sam,*.SAM', $
                                        /FIX_FILTER, $
                                        PATH=state.lastDirectory, $
                                        GET_PATH=lastDirectory )
            fileTextbox = WIDGET_INFO( event.TOP, $
                                       FIND_BY_UNAME='samTextbox' )
            WIDGET_CONTROL, fileTextbox, SET_VALUE=filename
            state.samFilename = filename
            state.lastDirectory = lastDirectory
            BREAK
         END

      'dwrButton': $
         BEGIN
            filename = DIALOG_PICKFILE( FILTER='*.dwr,*.DWR,*.ref,*.REF', $
                                        /FIX_FILTER, $
                                        PATH=state.lastDirectory, $
                                        GET_PATH=lastDirectory )
            fileTextbox = WIDGET_INFO( event.TOP, $
                                       FIND_BY_UNAME='dwrTextbox' )
            WIDGET_CONTROL, fileTextbox, SET_VALUE=filename
            state.dwrFilename = filename
            state.lastDirectory = lastDirectory
            BREAK
         END

      'diagnosticRawButton': $
         BEGIN
            state.diagnosticRaw = event.SELECT
            BREAK
         END

      'diagnosticSpectraButton': $
         BEGIN
            state.diagnosticSpectra = event.SELECT
            BREAK
         END

      'diagnosticInterferogramButton': $
         BEGIN
            state.diagnosticInterferogram = event.SELECT
            BREAK
         END

      'instrument102': $
         BEGIN
            state.instrumentModel = '102'
            BREAK
         END

      'instrument202': $
         BEGIN
            state.instrumentModel = '202'
            BREAK
         END

      'mode1Button':
      'mode2Button':
      'mode3Button':
      'mode4Button': $
         BEGIN
            CASE widget OF
               'mode1Button': state.runMode = 1
               'mode2Button': state.runMode = 2
               'mode3Button': state.runMode = 3
               'mode4Button': state.runMode = 4
            ENDCASE

            plateTextbox = WIDGET_INFO( event.TOP, $
                                        FIND_BY_UNAME='plateTextbox' )
            WIDGET_CONTROL, plateTextbox, GET_VALUE=plateEmissivity
            IF ( STRLEN( plateEmissivity[0] ) GT 0 ) THEN BEGIN
               state.plateEmissivity = DOUBLE( plateEmissivity[0] )
            ENDIF

            DP_RADIANCE_CALIBRATION_COMPUTE, state

            BREAK
         END

      'cancelButton': $
         BEGIN
            WHILE ( !D.WINDOW GE 0 ) DO WDELETE, !D.WINDOW
            CLOSE, /ALL
            WIDGET_CONTROL, event.TOP, /DESTROY
            RETURN
         END

   ENDSWITCH

   WIDGET_CONTROL, event.TOP, SET_UVALUE=state

END



PRO DP_RADIANCE_CALIBRATION_GUI

   CD, CURRENT=cwd

   state = { cbbFilename:'(none)', $
             wbbFilename:'(none)', $
             samFilename:'(none)', $
             dwrFilename:'(none)', $
             lastDirectory:cwd, $
             plateEmissivity:-1D, $
             diagnosticRaw:0L, $
             diagnosticSpectra:0L, $
             diagnosticInterferogram:0L, $
             instrumentModel:'102', $
             runMode:0L }

   mainBase = $
      WIDGET_BASE( COLUMN=1, $
                   /ALIGN_CENTER, $
                   TLB_FRAME_ATTR=1, $
                   UVALUE=state, $
                   TITLE='D&P Instruments Radiance Calibration' )

      filenameBase = $
         WIDGET_BASE( mainBase, $
                      COLUMN=1, $
                      /ALIGN_CENTER, $
                      MAP=1, $
                      FRAME=1 )

         cbbBase = $
            WIDGET_BASE( filenameBase, $
                         ROW=1, $
                         /ALIGN_RIGHT, $
                         MAP=1, $
                         FRAME=0 )

            cbbLabel = $
               WIDGET_LABEL( cbbBase, $
                             VALUE='Cold Blackbody Filename' )
            cbbTextbox = $
               WIDGET_TEXT( cbbBase, $
                            UNAME='cbbTextbox', $
                            UVALUE='cbbTextbox', $
                            XSIZE=80, $
                            /EDITABLE )
            cbbButton = $
               WIDGET_BUTTON( cbbBase, $
                              UVALUE='cbbButton', $
                              VALUE='Browse' )

         wbbBase = $
            WIDGET_BASE( filenameBase, $
                         ROW=1, $
                         /ALIGN_RIGHT, $
                         MAP=1, $
                         FRAME=0 )

            wbbLabel = $
               WIDGET_LABEL( wbbBase, $
                             VALUE='Warm Blackbody Filename' )
            wbbTextbox = $
               WIDGET_TEXT( wbbBase, $
                            UNAME='wbbTextbox', $
                            UVALUE='wbbTextbox', $
                            XSIZE=80, $
                            /EDITABLE )
            wbbButton = $
               WIDGET_BUTTON( wbbBase, $
                              UVALUE='wbbButton', $
                              VALUE='Browse' )

         samBase = $
            WIDGET_BASE( filenameBase, $
                         ROW=1, $
                         /ALIGN_RIGHT, $
                         MAP=1, $
                         FRAME=0 )

            samLabel = $
               WIDGET_LABEL( samBase, $
                             VALUE='Sample Filename' )
            samTextbox = $
               WIDGET_TEXT( samBase, $
                            UNAME='samTextbox', $
                            UVALUE='samTextbox', $
                            XSIZE=80, $
                            /EDITABLE )
            samButton = $
               WIDGET_BUTTON( samBase, $
                              UVALUE='samButton', $
                              VALUE='Browse' )

         dwrBase = $
            WIDGET_BASE( filenameBase, $
                         ROW=1, $
                         /ALIGN_RIGHT, $
                         MAP=1, $
                         FRAME=0 )

            dwrLabel = $
               WIDGET_LABEL( dwrBase, $
                             VALUE='Downwelling/Reference Filename (OPTIONAL)' )
            dwrTextbox = $
               WIDGET_TEXT( dwrBase, $
                            UNAME='dwrTextbox', $
                            UVALUE='dwrTextbox', $
                            XSIZE=80, $
                            /EDITABLE )
            dwrButton = $
               WIDGET_BUTTON( dwrBase, $
                              UVALUE='dwrButton', $
                              VALUE='Browse' )

      plateBase = $
         WIDGET_BASE( mainBase, $
                      COLUMN=1, $
                      /ALIGN_CENTER, $
                      MAP=1, $
                      FRAME=0 )

         plateLabel = $
            WIDGET_LABEL( plateBase, $
                          VALUE='Downwelling Radiance Reflectance Plate' )

         plateParameters = $
            WIDGET_BASE( plateBase, $
                         ROW=1, $
                         /ALIGN_CENTER, $
                         MAP=1, $
                         FRAME=1 )

            plateTextbox = $
               WIDGET_TEXT( plateParameters, $
                            UNAME='plateTextbox', $
                            UVALUE='plateTextbox', $
                            XSIZE=10, $
                            /EDITABLE )
            plateLabel = $
               WIDGET_LABEL( plateParameters, $
                             VALUE='Plate Emissivity (OPTIONAL - Leave blank to use value stored in downwelling file)' )

      diagnosticBase = $
         WIDGET_BASE( mainBase, $
                      COLUMN=1, $
                      /ALIGN_CENTER, $
                      MAP=1, $
                      FRAME=0 )

         diagnosticLabel = $
            WIDGET_LABEL( diagnosticBase, $
                          VALUE='Diagnostic Plots' )

         diagnosticChoices = $
            WIDGET_BASE( diagnosticBase, $
                         ROW=1, $
                         /ALIGN_CENTER, $
                         /NONEXCLUSIVE, $
                         MAP=1, $
                         FRAME=1 )

            diagnosticRawButton = $
               WIDGET_BUTTON( diagnosticChoices, $
                              UVALUE='diagnosticRawButton', $
                              VALUE='Raw Instrument Values' )
            diagnosticSpectraButton = $
               WIDGET_BUTTON( diagnosticChoices, $
                              UVALUE='diagnosticSpectraButton', $
                              VALUE='Individual Spectra' )
            diagnosticInterferogramButton = $
               WIDGET_BUTTON( diagnosticChoices, $
                              UVALUE='diagnosticInterferogramButton', $
                              VALUE='Individual Interferograms' )

         IF ( state.diagnosticRaw ) THEN WIDGET_CONTROL, diagnosticRawButton, SET_BUTTON=1
         IF ( state.diagnosticSpectra ) THEN WIDGET_CONTROL, diagnosticSpectraButton, SET_BUTTON=1
         IF ( state.diagnosticInterferogram ) THEN WIDGET_CONTROL, diagnosticInterferogramButton, SET_BUTTON=1

      instrumentBase = $
         WIDGET_BASE( mainBase, $
                      COLUMN=1, $
                      /ALIGN_CENTER, $
                      MAP=1, $
                      FRAME=0 )

         instrumentLabel = $
            WIDGET_LABEL( instrumentBase, $
                          VALUE='Instrument Choices' )

         instrumentChoices = $
            WIDGET_BASE( instrumentBase, $
                         ROW=1, $
                         /ALIGN_CENTER, $
                         /EXCLUSIVE, $
                         MAP=1, $
                         FRAME=1 )

            instrument102 = $
               WIDGET_BUTTON( instrumentChoices, $
                              UVALUE='instrument102', $
                              VALUE='Model 102F (MicroFT)' )
            instrument202 = $
               WIDGET_BUTTON( instrumentChoices, $
                              UVALUE='instrument202', $
                              VALUE='Model 202 (TurboFT)' )

         CASE state.instrumentModel OF
            '102': WIDGET_CONTROL, instrument102, SET_BUTTON=1
            '202': WIDGET_CONTROL, instrument202, SET_BUTTON=1
         ENDCASE

      actionBase = $
         WIDGET_BASE( mainBase, $
                      ROW=1, $
                      /ALIGN_CENTER, $
                      MAP=1, $
                      FRAME=0 )

         mode1Button = $
            WIDGET_BUTTON( actionBase, $
                           UVALUE='mode1Button', $
                           VALUE='Calibrate/Preview' )

         mode2Button = $
            WIDGET_BUTTON( actionBase, $
                           UVALUE='mode2Button', $
                           VALUE='Calibrate/Create ASCII Files' )

         mode3Button = $
            WIDGET_BUTTON( actionBase, $
                           UVALUE='mode3Button', $
                           VALUE='Calibrate/Create Individual' )

         mode4Button = $
            WIDGET_BUTTON( actionBase, $
                           UVALUE='mode4Button', $
                           VALUE='Calibrate/Create Plot Images' )

         cancelButton = $
            WIDGET_BUTTON( actionBase, $
                           UVALUE='cancelButton', $
                           VALUE='Cancel' )


   WIDGET_CONTROL, mainBase, /REALIZE

   XMANAGER, 'DP_RADIANCE_CALIBRATION_GUI', $
             mainBase, $
             EVENT_HANDLER='DP_RADIANCE_CALIBRATION_EVENT'

END



PRO DP_RADIANCE_CALIBRATION

   DP_RADIANCE_CALIBRATION_GUI

END



PRO READ_DP_FILE, filename, device, HEADER=headerData, $
                                    SPECTRUM=spectrum, $
                                    INTERFEROGRAM=interferogram

   headerSize = 1224

   headerData = {                      $
      Label:'',                        $
      Version:0L,                      $
      Revision:0L,                     $
      Date:'',                         $
      FileFormat:0L,                   $
      FileType:'',                     $
      OriginalFileName:'',             $
      ReferenceFileName:'',            $
      RelatedFileNameA:'',             $
      RelatedFileNameB:'',             $
      RelatedFileNameC:'',             $
      Annotate:'',                     $
      InstrumentModel:'',              $
      InstrumentSerialNumber:'',       $
      SoftwareVersionNumber:'',        $
      CrystalMaterial:'',              $
      LaserWavelengthMicrons:0D,       $
      LaserNullDoubling:0L,            $
      Padding:0L,                      $
      DispersionConstantXc:0D,         $
      DispersionConstantXm:0D,         $
      DispersionConstantXb:0D,         $
      NumChan:0L,                      $
      InterferogramSize:0L,            $
      ScanDirection:0L,                $
      AcquireMode:0L,                  $
      Emissivity:0L,                   $
      Apodization:0L,                  $
      ZeroFill:0L,                     $
      RunTimeMath:0L,                  $
      FFTSize:0L,                      $
      NumberOfCoAdds:0L,               $
      SingleSided:0L,                  $
      ChanDisplay:0L,                  $
      AmbTemperature:0D,               $
      InstTemperature:0D,              $
      WBBTemperature:0D,               $
      CBBTemperature:0D,               $
      TemperatureDWR:0D,               $
      EmissivityDWR:0D,                $
      LaserTemperature:0D,             $
      SpareI:LONARR(10),               $
      SpareF:DBLARR(10),               $
      SpareNA:'',                      $
      SpareNB:'',                      $
      SpareNC:'',                      $
      SpareND:'',                      $
      SpareNE:'',                      $
      HeaderEnd:''                     $
   }


   OPENR, LUN, filename, /GET_LUN, ERROR=errorOccurred
   IF ( errorOccurred ) THEN BEGIN
      PRINTF, -2, !ERR_STRING
   ENDIF

   IF ( device EQ '202' ) THEN BEGIN
      id = BYTARR( 12 )
      READU, LUN, id
   ENDIF

   rawHeader = BYTARR( headerSize )
   READU, LUN, rawHeader


   byteCount=0

   dataSize=4
   headerData.Label = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.Version = LONG(rawHeader,byteCount)
   headerData.Version = SWAP_ENDIAN( headerData.Version, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.Revision = LONG(rawHeader,byteCount)
   headerData.Revision = SWAP_ENDIAN( headerData.Revision, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=28
   headerData.Date = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.FileFormat = LONG(rawHeader,byteCount)
   headerData.FileFormat = SWAP_ENDIAN( headerData.FileFormat, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.FileType = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=68
   headerData.OriginalFileName = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=68
   headerData.ReferenceFileName = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=68
   headerData.RelatedFileNameA = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=68
   headerData.RelatedFileNameB = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=68
   headerData.RelatedFileNameC = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=84
   headerData.Annotate = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=36
   headerData.InstrumentModel = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=36
   headerData.InstrumentSerialNumber = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=36
   headerData.SoftwareVersionNumber = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=36
   headerData.CrystalMaterial = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=8
   headerData.LaserWavelengthMicrons = DOUBLE(rawHeader,byteCount)
   headerData.LaserWavelengthMicrons = SWAP_ENDIAN( headerData.LaserWavelengthMicrons, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.LaserNullDoubling = LONG(rawHeader,byteCount)
   headerData.LaserNullDoubling = SWAP_ENDIAN( headerData.LaserNullDoubling, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.Padding = LONG(rawHeader,byteCount)
   headerData.Padding = SWAP_ENDIAN( headerData.Padding, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=8
   headerData.DispersionConstantXc = DOUBLE(rawHeader,byteCount)
   headerData.DispersionConstantXc = SWAP_ENDIAN( headerData.DispersionConstantXc, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=8
   headerData.DispersionConstantXm = DOUBLE(rawHeader,byteCount)
   headerData.DispersionConstantXm = SWAP_ENDIAN( headerData.DispersionConstantXm, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=8
   headerData.DispersionConstantXb = DOUBLE(rawHeader,byteCount)
   headerData.DispersionConstantXb = SWAP_ENDIAN( headerData.DispersionConstantXb, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.NumChan = LONG(rawHeader,byteCount)
   headerData.NumChan = SWAP_ENDIAN( headerData.NumChan, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.InterferogramSize = LONG(rawHeader,byteCount)
   headerData.InterferogramSize = SWAP_ENDIAN( headerData.InterferogramSize, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.ScanDirection = LONG(rawHeader,byteCount)
   headerData.ScanDirection = SWAP_ENDIAN( headerData.ScanDirection, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.AcquireMode = LONG(rawHeader,byteCount)
   headerData.AcquireMode = SWAP_ENDIAN( headerData.AcquireMode, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.Emissivity = LONG(rawHeader,byteCount)
   headerData.Emissivity = SWAP_ENDIAN( headerData.Emissivity, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.Apodization = LONG(rawHeader,byteCount)
   headerData.Apodization = SWAP_ENDIAN( headerData.Apodization, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.ZeroFill = LONG(rawHeader,byteCount)
   headerData.ZeroFill = SWAP_ENDIAN( headerData.ZeroFill, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.RunTimeMath = LONG(rawHeader,byteCount)
   headerData.RunTimeMath = SWAP_ENDIAN( headerData.RunTimeMath, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.FFTSize = LONG(rawHeader,byteCount)
   headerData.FFTSize = SWAP_ENDIAN( headerData.FFTSize, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.NumberOfCoAdds = LONG(rawHeader,byteCount)
   headerData.NumberOfCoAdds = SWAP_ENDIAN( headerData.NumberOfCoAdds, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.SingleSided = LONG(rawHeader,byteCount)
   headerData.SingleSided = SWAP_ENDIAN( headerData.SingleSided, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.ChanDisplay = LONG(rawHeader,byteCount)
   headerData.ChanDisplay = SWAP_ENDIAN( headerData.ChanDisplay, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=8
   headerData.AmbTemperature = DOUBLE(rawHeader,byteCount)
   headerData.AmbTemperature = SWAP_ENDIAN( headerData.AmbTemperature, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=8
   headerData.InstTemperature = DOUBLE(rawHeader,byteCount)
   headerData.InstTemperature = SWAP_ENDIAN( headerData.InstTemperature, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=8
   headerData.WBBTemperature = DOUBLE(rawHeader,byteCount)
   headerData.WBBTemperature = SWAP_ENDIAN( headerData.WBBTemperature, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=8
   headerData.CBBTemperature = DOUBLE(rawHeader,byteCount)
   headerData.CBBTemperature = SWAP_ENDIAN( headerData.CBBTemperature, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=8
   headerData.TemperatureDWR = DOUBLE(rawHeader,byteCount)
   headerData.TemperatureDWR = SWAP_ENDIAN( headerData.TemperatureDWR, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=8
   headerData.EmissivityDWR = DOUBLE(rawHeader,byteCount)
   headerData.EmissivityDWR = SWAP_ENDIAN( headerData.EmissivityDWR, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=8
   headerData.LaserTemperature = DOUBLE(rawHeader,byteCount)
   headerData.LaserTemperature = SWAP_ENDIAN( headerData.LaserTemperature, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=40
   headerData.SpareI = LONG(rawHeader,byteCount, 10)
   headerData.SpareI = SWAP_ENDIAN( headerData.SpareI, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=80
   headerData.SpareF = DOUBLE(rawHeader,byteCount, 10)
   headerData.SpareF = SWAP_ENDIAN( headerData.SpareF, /SWAP_IF_BIG_ENDIAN )
   byteCount = byteCount + dataSize

   dataSize=68
   headerData.SpareNA = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=68
   headerData.SpareNB = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=68
   headerData.SpareNC = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=68
   headerData.SpareND = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=68
   headerData.SpareNE = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   dataSize=4
   headerData.HeaderEnd = STRING(rawHeader[byteCount:byteCount+dataSize-1])
   byteCount = byteCount + dataSize

   spectrumSize = 2048*headerData.ZEROFILL

   interferogram = REPLICATE( { FREQUENCY:FLTARR(headerData.InterferogramSize), $
                                VALUE:INTARR(headerData.InterferogramSize) },   $
                              headerData.NumberOfCoAdds )
   iValue = INTARR(headerData.InterferogramSize)

   FOR I = 0, headerData.NumberOfCoAdds-1 DO BEGIN
      READU, LUN, iValue
      FOR J = 0, spectrumSize-1 DO BEGIN
         iValue[J] = SWAP_ENDIAN( iValue[J], /SWAP_IF_BIG_ENDIAN )
      ENDFOR
      interferogram[I].VALUE = iValue
   ENDFOR

   spectrum = { WAVELENGTH:FLTARR(spectrumSize), $
                VALUE:FLTARR(spectrumSize) }
   sValue = FLTARR(spectrumSize)

   READU, LUN, sValue
   FOR J = 0, spectrumSize-1 DO BEGIN
      sValue[J] = SWAP_ENDIAN( sValue[J], /SWAP_IF_BIG_ENDIAN )
   ENDFOR
   spectrum.VALUE = sValue

   CLOSE, LUN
   FREE_LUN, LUN

   shortestWavelength = 2.0 * headerData.LaserWavelengthMicrons
   largestWavenumber = 10000.0 / shortestWavelength

   deltaWavenumber = largestWavenumber / DOUBLE( spectrumSize )
   FOR I = 0, spectrumSize-1 DO BEGIN
      spectrum.WAVELENGTH[I] = largestWavenumber - (spectrumSize-I) * deltaWavenumber
      spectrum.WAVELENGTH[I] = spectrum.WAVELENGTH[I] + $
                               10.0^( headerData.DispersionConstantXm * $
                                      spectrum.WAVELENGTH[I] + $
                                      headerData.DispersionConstantXb )
      spectrum.WAVELENGTH[I] = 10000.0 / spectrum.WAVELENGTH[I]
   ENDFOR

   deltaWavenumber = largestWavenumber / (headerData.InterferogramSize/2)
   FOR I = 0, (headerData.InterferogramSize/2-1) DO BEGIN
      interferogram.FREQUENCY[I] = largestWavenumber - $
                                   ((headerData.InterferogramSize/2)-I) * deltaWavenumber
      interferogram.FREQUENCY[I] = interferogram.FREQUENCY[I] + $
                                   10.0^( headerData.DispersionConstantXm * $
                                          interferogram.FREQUENCY[I] + $
                                          headerData.DispersionConstantXb )
   ENDFOR
   FOR I = 0, (headerData.InterferogramSize/2-1) DO BEGIN
      interferogram.FREQUENCY[headerData.InterferogramSize-I-1] = interferogram.FREQUENCY[I]
   ENDFOR

   RETURN

END