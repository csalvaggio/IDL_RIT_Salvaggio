;+
; :NAME:
;    INTERACTIVE_CROP
;
; :PURPOSE:
;    The INTERACTIVE_CROP function will allow the user to select a
;    spatial sub-region of the provided image that is to be cropped.
;    The function will create a window and display the image provided
;    for the user to interact with.
;
; :CATEGORY:
;    Image Processing.
;
; :PARAMETERS:
;    fullImage
;       Either a 1- or 3-band image that is to be interactively cropped.
;
; :KEYWORDS:
;    None
;
; :RETURN VALUE:
;    A 1- or 3-band extracted subimage will be returned to the calling
;    module.
;
; :MODIFICATION HISTORY:
;    Written by:       Carl Salvaggio
;    Original Code:    Oct 20, 2010
;-

FUNCTION INTERACTIVE_CROP, fullImage, MESSAGE=message

;+
; Determine the spatial dimensions and the number of bands for 
; the provided image
;-
   dimensions = SIZE( fullImage )

   CASE dimensions[0] OF
      2: BEGIN
            numberColumns = dimensions[1]
            numberRows = dimensions[2]
            numberBands = 1
         END
      3: BEGIN
            numberColumns = dimensions[2]
            numberRows = dimensions[3]
            numberBands = 3
         END
   ENDCASE

;+
; Create a window of the proper size to display the provided image
; in for user interaction (this window is temporary)
;-
   WINDOW, !D.WINDOW+1, XSIZE=numberColumns, YSIZE=numberRows

;+
; Display the provided image
;-
   CASE numberBands OF
      1: TV, fullImage
      3: TV, fullImage, TRUE=1
   ENDCASE

;+
; Determine the initial cursor (box) size based on the dimensions
; of the provided image; the cursor (box) should be half as wide and
; half as high as the image and be centered in the window
;-
   cursorWidth = numberColumns / 2
   cursorHeight = numberRows / 2
   llx = numberColumns / 4
   lly = numberRows / 4

;+
; Instantiate the cursor (box) in the window and display
; use instructions to the console if desired
;-
   IF NOT KEYWORD_SET( message ) THEN message = 0
   BOX_CURSOR, llx, lly, cursorWidth, cursorHeight, /INIT, MESSAGE=message

;+
; Once the cursor (box) limits and positions have been determined,
; delete the window created in this routine
;-
   WDELETE, !D.WINDOW

;+
; Return the subarray that represents the cropped image
;-
   CASE numberBands OF
      1: RETURN, fullImage[ llx:llx+cursorWidth, lly:lly+cursorHeight ]
      3: RETURN, fullImage[ *, llx:llx+cursorWidth, lly:lly+cursorHeight ]
   ENDCASE

END