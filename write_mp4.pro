;+
; :NAME:
;    WRITE_MP4
;
; :PURPOSE:
;    This procedure will create an MPEG4 video file from the provided
;    frames.  The frames can be either greyscale or 3-channel color.
;
; :CATEGORY:
;    Image Processing.
;
; :CALLING SEQUENCE:
;    WRITE_MP4, frames, filename, FPS=fps, VERBOSE=verbose
;
; :INPUTS:
;    frames
;       An [n,samples,lines] (greyscale) or [n,3,samples,lines] (color)
;       array containing the frames to be rendered in the video file.
;    filename
;       A string containing the full path and filename of the video
;       file to be created.
;
; :KEYWORD PARAMETRS:
;      FPS
;         The frame rate that the created video file should play
;         at [default=30].
;      VERBOSE
;         Set this keyword to receive feedback to the console window
;         during the rendering process.
;
; :ERROR CHECKING:
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
;    January, 2012     Original code
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

PRO WRITE_MP4, frames, filename, FPS=fps, VERBOSE=verbose

;+
; Set the frame rate to the default value of 30 fps if not specified
;-
   IF NOT KEYWORD_SET( fps ) THEN fps = 30

;+
; Determine the size of the provided frames array and check if the
; provided data is of valid dimensions for this procedure
;-
   dims = SIZE( frames, /DIMENSIONS )
   numberDims = SIZE( frames, /N_DIMENSIONS )
   CASE numberDims OF
      3: $
         BEGIN
            isColor = 0
            numberFrames = dims[0]
            numberSamples = dims[1]
            numberLines = dims[2]
         END
      4: $
         BEGIN
            isColor = 1
            numberFrames = dims[0]
            numberSamples = dims[2]
            numberLines = dims[3]
         END
      ELSE: $
         BEGIN
            MESSAGE, 'Provided frames must be of dimension [n,width,height] or [n,3,width,height]'
         END
   ENDCASE

;+
; Initialize the video object
;-
   IF KEYWORD_SET( verbose ) THEN PRINT, 'WRITE_MP4: Beginning to render ', filename
   oVid = IDLffVideoWrite( filename, FORMAT='mp4' )

;+
; Add a video stream to the video object
;-
   IF KEYWORD_SET( verbose ) THEN PRINT, 'WRITE_MP4: Initiating the video stream'
   vidStream = oVid.AddVideoStream( numberSamples, numberLines, fps )

;+
; Render the video by adding individual frames to the video stream
;-
   cumulativeTime = 0
   FOR frame = 0, numberFrames-1 DO BEGIN

;+
; Extract the current frame from the provided array (replicate the
; frame in three channels if the input is greyscale)
;-
      IF isColor THEN BEGIN
         oFrame = REFORM( frames[frame,*,*,*] )
      ENDIF ELSE BEGIN
         oFrame = [ frames[frame,*,*], frames[frame,*,*], frames[frame,*,*] ]
      ENDELSE

;+
; Put the current frame in the video stream of the video object
;-
      startTime = SYSTIME( /SECONDS )
      time = oVid.Put( vidStream, oFrame )
      endTime = SYSTIME( /SECONDS )
      IF KEYWORD_SET( verbose ) THEN BEGIN
         cumulativeTime = cumulativeTime + (endTime - startTime)
         averageFPS = cumulativeTime / (frame+1)
         PRINT, 'WRITE_MP4: Rendering frame ', $
                STRTRIM( frame+1, 2 ), $
                ' of ', $
                STRTRIM( numberFrames, 2 ), $
                ' (average rate = ', $
                STRTRIM( averageFPS, 2 ), $
                ' frames/sec)'
      ENDIF

   ENDFOR

;+
; Clean up after the rendering process and close the video file
;-
   IF KEYWORD_SET( verbose ) THEN PRINT, 'WRITE_MP4: Cleaning up after rendering'
   oVid.Cleanup

END