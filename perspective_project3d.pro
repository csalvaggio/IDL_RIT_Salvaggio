;+
; :NAME:
;    PERSPECTIVE_PROJECT3D
;
; :PURPOSE:
;    This function will perform a perspective projection of a three-
;    dimensional point defined in homogenous coordinates onto a two-
;    dimensional "focal plane" as if being acquired by a camera
;    located in 3-space with a specified focal length, pixel size,
;    and "look at" position.
;
; :CATEGORY:
;    GENERAL.
;
; :CALLING SEQUENCE:
;    result = PERSPECTIVE_PROJECT3D( point, $
;                                    cameraPosition, $
;                                    focalLength, $
;                                    pixelSize, $
;                                    lookatPosition, $
;                                    OBSERVER_UP=observerUp )
;
; :INPUTS:
;    point
;       A vector containing the three-dimensional point to be projected in
;       homogenous coordinates [x,y,z,1].
;    cameraPosition
;       A vector containing the three-dimensional point at which the camera
;       is located in 3-space.  Ths point is defined in homogeneous coordinates.
;    focalLength
;       A scalar defining the focal length of camera's lens (while this value
;       serves as a scaling factor, keeping the units consistent with the pixel
;       size is required).
;    pixelSize
;       A scalar defining the pixel size of the camera (pixels are assumed to
;       be square) (while this value serves as a scaling factor, keeping the 
;       units consistent with the focal length is required).
;    lookatPosition
;       A vector containing the three-dimensional point at which the camera is
;       staring in 3-space.  This is only used to define the direction of gaze.
;       This point is defined in homogeneous coordinates.
;
; :KEYWORD PARAMETRS:
;      OBSERVER_UP
;         Set this keyword to a 4-element homogenous vector defining the up 
;         direction of the observer (as if the observer is holding the camera).
;         The default value is [0,0,1,1] indicating up is in the positive z-
;         direction.
;
; :ERROR CHECKING:
;    None.
;
; :SIDE EFFECTS:
;    None.
;
; :REQUIRES:
;    NORMALIZE
;    NORMH
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

FUNCTION PERSPECTIVE_PROJECT3D, point, $
                                cameraPosition, $
                                focalLength, $
                                pixelSize, $
                                lookatPosition, $
                                OBSERVER_UP=observerUp

;+
; The 'observeUp' vector orients the y-axis of the camera's coordinate system
; to point towards the top of the image, defined in the world coordinate
; system (DEFAULT=[0,0,1,1])
;-
   IF NOT KEYWORD_SET( observerUp ) THEN observerUp = [ 0D, 0D, 1D, 1D ]

;+
; Define the camera's coordinate system where the camera is looking down the
; z-axis, with the x-axis to the right and the y-axis pointing up
;-
   forwardVector = NORMALIZE( DOUBLE( lookatPosition ) - DOUBLE( cameraPosition ) )
   rightVector = NORMALIZE( CROSSP( observerUp, forwardVector ) )
   upVector = NORMALIZE( CROSSP( rightVector, forwardVector ) )

;+
; Define the rotation of the camera's coordinate system to align with the
; world coordinate system
;-
   rotationMatrix = [ [ rightVector[0],   rightVector[1],   rightVector[2]   ], $
                      [ upVector[0],      upvector[1],      upVector[2]      ], $
                      [ forwardVector[0], forwardVector[1], forwardVector[2] ] ]

;+
; Define the camera calibration matrix
;-
   f = DOUBLE( focalLength )
   p = DOUBLE( pixelSize )
   cameraMatrix = [ [ -f/p, 0D,   0D ], $
                    [ 0D,   -f/p, 0D ], $
                    [ 0D,   0D,   1D ] ]

;+
; Define the projection matrix
;-
   projectionMatrix = [ cameraMatrix##rotationMatrix, $
                        -cameraMatrix##rotationMatrix##TRANSPOSE( cameraPosition[0:2] ) ]

;+
; Perform the projective transformation
;-
   projectedPoint = NORMH( projectionMatrix ## TRANSPOSE( point ) )

   RETURN, TRANSPOSE( projectedPoint )

END