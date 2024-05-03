! raylib_camera.f90
!
! Additional camera routines for raylib 4.5, from `rcamera.h`.
!
! Author:  Philipp Engel
! Licence: ISC
MODULE raylib_camera
USE, INTRINSIC :: ISO_C_BINDING
USE :: raylib
IMPLICIT NONE(TYPE, EXTERNAL)
PRIVATE

PUBLIC :: camera_move_forward
PUBLIC :: camera_move_right
PUBLIC :: camera_move_to_target
PUBLIC :: camera_move_up
PUBLIC :: camera_pitch
PUBLIC :: camera_roll
PUBLIC :: camera_yaw
PUBLIC :: get_camera_forward
PUBLIC :: get_camera_projection_matrix
PUBLIC :: get_camera_right
PUBLIC :: get_camera_up
PUBLIC :: get_camera_view_matrix

INTERFACE
  ! void CameraMoveForward(Camera *camera, float distance, bool moveInWorldPlane)
        subroutine camera_move_forward(camera, distance, move_in_world_plane) bind(c, name='CameraMoveForward')
    IMPORT :: C_BOOL, C_FLOAT, camera3d_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(inout) :: camera
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: distance
    LOGICAL(kind=C_BOOL), INTENT(in), VALUE :: move_in_world_plane
  END SUBROUTINE camera_move_forward

  ! void CameraMoveRight(Camera *camera, float distance, bool moveInWorldPlane)
        subroutine camera_move_right(camera, distance, move_in_world_plane) bind(c, name='CameraMoveRight')
    IMPORT :: C_BOOL, C_FLOAT, camera3d_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(inout) :: camera
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: distance
    LOGICAL(kind=C_BOOL), INTENT(in), VALUE :: move_in_world_plane
  END SUBROUTINE camera_move_right

  ! void CameraMoveToTarget(Camera *camera, float delta)
        subroutine camera_move_to_target(camera, delta) bind(c, name='CameraMoveToTarget')
    IMPORT :: C_FLOAT, camera3d_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(inout) :: camera
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: delta
  END SUBROUTINE camera_move_to_target

  ! void CameraMoveUp(Camera *camera, float distance)
  SUBROUTINE camera_move_up(camera, distance) BIND(c, name='CameraMoveUp')
    IMPORT :: C_FLOAT, camera3d_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(inout) :: camera
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: distance
  END SUBROUTINE camera_move_up

  ! void CameraPitch(Camera *camera, float angle, bool lockView, bool rotateAroundTarget, bool rotateUp)
        subroutine camera_pitch(camera, angle, lock_view, rotate_around_target, rotate_up) bind(c, name='CameraPitch')
    IMPORT :: C_BOOL, C_FLOAT, camera3d_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(inout) :: camera
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: angle
    LOGICAL(kind=C_BOOL), INTENT(in), VALUE :: lock_view
    LOGICAL(kind=C_BOOL), INTENT(in), VALUE :: rotate_around_target
    LOGICAL(kind=C_BOOL), INTENT(in), VALUE :: rotate_up
  END SUBROUTINE camera_pitch

  ! void CameraRoll(Camera *camera, float angle)
  SUBROUTINE camera_roll(camera, angle) BIND(c, name='CameraRoll')
    IMPORT :: C_FLOAT, camera3d_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(inout) :: camera
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: angle
  END SUBROUTINE camera_roll

  ! void CameraYaw(Camera *camera, float angle, bool rotateAroundTarget)
        subroutine camera_yaw(camera, angle, rotate_around_target) bind(c, name='CameraYaw')
    IMPORT :: C_BOOL, C_FLOAT, camera3d_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(inout) :: camera
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: angle
    LOGICAL(kind=C_BOOL), INTENT(in), VALUE :: rotate_around_target
  END SUBROUTINE camera_yaw

  ! Vector3 GetCameraForward(Camera *camera)
  FUNCTION get_camera_forward(camera) BIND(c, name='GetCameraForward')
    IMPORT :: camera3d_, vector3_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(inout) :: camera
    TYPE(vector3_) :: get_camera_forward
  END FUNCTION get_camera_forward

  ! Matrix GetCameraProjectionMatrix(Camera* camera, float aspect)
        function get_camera_projection_matrix(camera, aspect) bind(c, name='GetCameraProjectionMatrix')
    IMPORT :: C_FLOAT, camera3d_, matrix_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(inout) :: camera
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: aspect
    TYPE(matrix_) :: get_camera_projection_matrix
  END FUNCTION get_camera_projection_matrix

  ! Vector3 GetCameraRight(Camera *camera)
  FUNCTION get_camera_right(camera) BIND(c, name='GetCameraRight')
    IMPORT :: camera3d_, vector3_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(inout) :: camera
    TYPE(vector3_) :: get_camera_right
  END FUNCTION get_camera_right

  ! Vector3 GetCameraUp(Camera *camera)
  FUNCTION get_camera_up(camera) BIND(c, name='GetCameraUp')
    IMPORT :: camera3d_, vector3_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(inout) :: camera
    TYPE(vector3_) :: get_camera_up
  END FUNCTION get_camera_up

  ! Matrix GetCameraViewMatrix(Camera *camera)
  FUNCTION get_camera_view_matrix(camera) BIND(c, name='GetCameraViewMatrix')
    IMPORT :: camera3d_, matrix_
    IMPLICIT NONE
    TYPE(camera3d_), INTENT(inout) :: camera
    TYPE(matrix_) :: get_camera_view_matrix
  END FUNCTION get_camera_view_matrix
END INTERFACE
END MODULE raylib_camera
