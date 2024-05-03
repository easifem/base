! raylib_math.f90
!
! Fortran 2018 interface bindings to `raymath.h`.
!
! Author:  Philipp Engel
! Licence: ISC
MODULE raylib_math
USE, INTRINSIC :: ISO_C_BINDING
USE :: raylib
IMPLICIT NONE(TYPE, EXTERNAL)
PRIVATE

! float3
TYPE, BIND(c), PUBLIC :: float3_
  REAL(kind=C_FLOAT) :: v(0:2) = 0.0
END TYPE float3_

! float16
TYPE, BIND(c), PUBLIC :: float16_
  REAL(kind=C_FLOAT) :: v(0:15) = 0.0
END TYPE float16_

PUBLIC :: clamp
PUBLIC :: float_equals
PUBLIC :: lerp
PUBLIC :: matrix_add
PUBLIC :: matrix_determinant
PUBLIC :: matrix_frustum
PUBLIC :: matrix_identity
PUBLIC :: matrix_invert
PUBLIC :: matrix_look_at
PUBLIC :: matrix_multiply
PUBLIC :: matrix_ortho
PUBLIC :: matrix_perspective
PUBLIC :: matrix_rotate
PUBLIC :: matrix_rotate_x
PUBLIC :: matrix_rotate_xyz
PUBLIC :: matrix_rotate_y
PUBLIC :: matrix_rotate_z
PUBLIC :: matrix_rotate_zyx
PUBLIC :: matrix_scale
PUBLIC :: matrix_subtract
PUBLIC :: matrix_to_float_v
PUBLIC :: matrix_trace
PUBLIC :: matrix_translate
PUBLIC :: matrix_transpose
PUBLIC :: normalize
PUBLIC :: quaternion_add
PUBLIC :: quaternion_add_value
PUBLIC :: quaternion_divide
PUBLIC :: quaternion_equals
PUBLIC :: quaternion_from_axis_angle
PUBLIC :: quaternion_from_euler
PUBLIC :: quaternion_from_matrix
PUBLIC :: quaternion_from_vector3_to_vector3
PUBLIC :: quaternion_identity
PUBLIC :: quaternion_invert
PUBLIC :: quaternion_length
PUBLIC :: quaternion_lerp
PUBLIC :: quaternion_multiply
PUBLIC :: quaternion_nlerp
PUBLIC :: quaternion_normalize
PUBLIC :: quaternion_scale
PUBLIC :: quaternion_slerp
PUBLIC :: quaternion_subtract
PUBLIC :: quaternion_subtract_value
PUBLIC :: quaternion_to_axis_angle
PUBLIC :: quaternion_to_euler
PUBLIC :: quaternion_to_matrix
PUBLIC :: quaternion_transform
PUBLIC :: remap
PUBLIC :: vector2_add
PUBLIC :: vector2_add_value
PUBLIC :: vector2_angle
PUBLIC :: vector2_clamp
PUBLIC :: vector2_clamp_value
PUBLIC :: vector2_distance
PUBLIC :: vector2_distance_sqr
PUBLIC :: vector2_divide
PUBLIC :: vector2_dot_product
PUBLIC :: vector2_equals
PUBLIC :: vector2_invert
PUBLIC :: vector2_length
PUBLIC :: vector2_length_sqr
PUBLIC :: vector2_lerp
PUBLIC :: vector2_line_angle
PUBLIC :: vector2_move_towards
PUBLIC :: vector2_multiply
PUBLIC :: vector2_negate
PUBLIC :: vector2_normalize
PUBLIC :: vector2_one
PUBLIC :: vector2_reflect
PUBLIC :: vector2_rotate
PUBLIC :: vector2_scale
PUBLIC :: vector2_subtract
PUBLIC :: vector2_subtract_value
PUBLIC :: vector2_transform
PUBLIC :: vector2_zero
PUBLIC :: vector3_add
PUBLIC :: vector3_add_value
PUBLIC :: vector3_angle
PUBLIC :: vector3_barycenter
PUBLIC :: vector3_clamp
PUBLIC :: vector3_clamp_value
PUBLIC :: vector3_cross_product
PUBLIC :: vector3_distance
PUBLIC :: vector3_distance_sqr
PUBLIC :: vector3_divide
PUBLIC :: vector3_dot_product
PUBLIC :: vector3_equals
PUBLIC :: vector3_invert
PUBLIC :: vector3_length
PUBLIC :: vector3_length_sqr
PUBLIC :: vector3_lerp
PUBLIC :: vector3_max
PUBLIC :: vector3_min
PUBLIC :: vector3_multiply
PUBLIC :: vector3_negate
PUBLIC :: vector3_normalize
PUBLIC :: vector3_one
PUBLIC :: vector3_ortho_normalize
PUBLIC :: vector3_perpendicular
PUBLIC :: vector3_reflect
PUBLIC :: vector3_refract
PUBLIC :: vector3_rotate_by_axis_angle
PUBLIC :: vector3_rotate_by_quaternion
PUBLIC :: vector3_scale
PUBLIC :: vector3_subtract
PUBLIC :: vector3_subtract_value
PUBLIC :: vector3_to_float_v
PUBLIC :: vector3_transform
PUBLIC :: vector3_unproject
PUBLIC :: vector3_zero
PUBLIC :: wrap

INTERFACE
  ! float Clamp(float value, float min, float max)
  FUNCTION clamp(VALUE, min, max) BIND(c, name='Clamp')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: VALUE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: min
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: max
    REAL(kind=C_FLOAT) :: clamp
  END FUNCTION clamp

  ! int FloatEquals(float x, float y)
  FUNCTION float_equals(x, y) BIND(c, name='FloatEquals')
    IMPORT :: C_FLOAT, C_INT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: x
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: y
    INTEGER(kind=C_INT) :: float_equals
  END FUNCTION float_equals

  ! float Lerp(float start, float end, float amount)
  FUNCTION lerp(start, END, amount) BIND(c, name='Lerp')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: END
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: amount
    REAL(kind=C_FLOAT) :: lerp
  END FUNCTION lerp

  ! Matrix MatrixAdd(Matrix left, Matrix right)
  FUNCTION matrix_add(left, right) BIND(c, name='MatrixAdd')
    IMPORT :: matrix_
    IMPLICIT NONE
    TYPE(matrix_), INTENT(in), VALUE :: left
    TYPE(matrix_), INTENT(in), VALUE :: right
    TYPE(matrix_) :: matrix_add
  END FUNCTION matrix_add

  ! float MatrixDeterminant(Matrix mat)
  FUNCTION matrix_determinant(mat) BIND(c, name='MatrixDeterminant')
    IMPORT :: C_FLOAT, matrix_
    IMPLICIT NONE
    TYPE(matrix_), INTENT(in), VALUE :: mat
    REAL(kind=C_FLOAT) :: matrix_determinant
  END FUNCTION matrix_determinant

  ! Matrix MatrixFrustum(double left, double right, double bottom, double top, double near, double far)
        function matrix_frustum(left, right, bottom, top, near, far) bind(c, name='MatrixFrustum')
    IMPORT :: C_DOUBLE, matrix_
    IMPLICIT NONE
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: left
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: right
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: bottom
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: top
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: near
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: far
    TYPE(matrix_) :: matrix_frustum
  END FUNCTION matrix_frustum

  ! Matrix MatrixIdentity(void)
  FUNCTION matrix_identity() BIND(c, name='MatrixIdentity')
    IMPORT :: matrix_
    IMPLICIT NONE
    TYPE(matrix_) :: matrix_identity
  END FUNCTION matrix_identity

  ! Matrix MatrixInvert(Matrix mat)
  FUNCTION matrix_invert(mat) BIND(c, name='MatrixInvert')
    IMPORT :: matrix_
    IMPLICIT NONE
    TYPE(matrix_), INTENT(in), VALUE :: mat
    TYPE(matrix_) :: matrix_invert
  END FUNCTION matrix_invert

  ! Matrix MatrixLookAt(Vector3 eye, Vector3 target, Vector3 up)
  FUNCTION matrix_look_at(eye, TARGET, up) BIND(c, name='MatrixLookAt')
    IMPORT :: matrix_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: eye
    TYPE(vector3_), INTENT(in), VALUE :: TARGET
    TYPE(vector3_), INTENT(in), VALUE :: up
    TYPE(matrix_) :: matrix_look_at
  END FUNCTION matrix_look_at

  ! Matrix MatrixMultiply(Matrix left, Matrix right)
  FUNCTION matrix_multiply(left, right) BIND(c, name='MatrixMultiply')
    IMPORT :: matrix_
    IMPLICIT NONE
    TYPE(matrix_), INTENT(in), VALUE :: left
    TYPE(matrix_), INTENT(in), VALUE :: right
    TYPE(matrix_) :: matrix_multiply
  END FUNCTION matrix_multiply

  ! Matrix MatrixOrtho(double left, double right, double bottom, double top, double near, double far)
        function matrix_ortho(left, right, bottom, top, near, far) bind(c, name='MatrixOrtho')
    IMPORT :: C_DOUBLE, matrix_
    IMPLICIT NONE
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: left
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: right
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: bottom
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: top
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: near
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: far
    TYPE(matrix_) :: matrix_ortho
  END FUNCTION matrix_ortho

  ! Matrix MatrixPerspective(double fovy, double aspect, double near, double far)
        function matrix_perspective(fovy, aspect, near, far) bind(c, name='MatrixPerspective')
    IMPORT :: C_DOUBLE, matrix_
    IMPLICIT NONE
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: fovy
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: aspect
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: near
    REAL(kind=C_DOUBLE), INTENT(in), VALUE :: far
    TYPE(matrix_) :: matrix_perspective
  END FUNCTION matrix_perspective

  ! Matrix MatrixRotate(Vector3 axis, float angle)
  FUNCTION matrix_rotate(axis, angle) BIND(c, name='MatrixRotate')
    IMPORT :: C_FLOAT, matrix_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: axis
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: angle
    TYPE(matrix_) :: matrix_rotate
  END FUNCTION matrix_rotate

  ! Matrix MatrixRotateX(float angle)
  FUNCTION matrix_rotate_x(angle) BIND(c, name='MatrixRotateX')
    IMPORT :: C_FLOAT, matrix_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: angle
    TYPE(matrix_) :: matrix_rotate_x
  END FUNCTION matrix_rotate_x

  ! Matrix MatrixRotateXYZ(Vector3 angle)
  FUNCTION matrix_rotate_xyz(angle) BIND(c, name='MatrixRotateXYZ')
    IMPORT :: matrix_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: angle
    TYPE(matrix_) :: matrix_rotate_xyz
  END FUNCTION matrix_rotate_xyz

  ! Matrix MatrixRotateY(float angle)
  FUNCTION matrix_rotate_y(angle) BIND(c, name='MatrixRotateY')
    IMPORT :: C_FLOAT, matrix_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: angle
    TYPE(matrix_) :: matrix_rotate_y
  END FUNCTION matrix_rotate_y

  ! Matrix MatrixRotateZ(float angle)
  FUNCTION matrix_rotate_z(angle) BIND(c, name='MatrixRotateZ')
    IMPORT :: C_FLOAT, matrix_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: angle
    TYPE(matrix_) :: matrix_rotate_z
  END FUNCTION matrix_rotate_z

  ! Matrix MatrixRotateZYX(Vector3 angle)
  FUNCTION matrix_rotate_zyx(angle) BIND(c, name='MatrixRotateZYX')
    IMPORT :: matrix_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: angle
    TYPE(matrix_) :: matrix_rotate_zyx
  END FUNCTION matrix_rotate_zyx

  ! Matrix MatrixScale(float x, float y, float z)
  FUNCTION matrix_scale(x, y, z) BIND(c, name='MatrixScale')
    IMPORT :: C_FLOAT, matrix_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: x
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: z
    TYPE(matrix_) :: matrix_scale
  END FUNCTION matrix_scale

  ! Matrix MatrixSubtract(Matrix left, Matrix right)
  FUNCTION matrix_subtract(left, right) BIND(c, name='MatrixSubtract')
    IMPORT :: matrix_
    IMPLICIT NONE
    TYPE(matrix_), INTENT(in), VALUE :: left
    TYPE(matrix_), INTENT(in), VALUE :: right
    TYPE(matrix_) :: matrix_subtract
  END FUNCTION matrix_subtract

  ! float16 MatrixToFloatV(Matrix mat)
  FUNCTION matrix_to_float_v(mat) BIND(c, name='MatrixToFloatV')
    IMPORT :: float16_, matrix_
    IMPLICIT NONE
    TYPE(matrix_), INTENT(in), VALUE :: mat
    TYPE(float16_) :: matrix_to_float_v
  END FUNCTION matrix_to_float_v

  ! float MatrixTrace(Matrix mat)
  FUNCTION matrix_trace(mat) BIND(c, name='MatrixTrace')
    IMPORT :: C_FLOAT, matrix_
    IMPLICIT NONE
    TYPE(matrix_), INTENT(in), VALUE :: mat
    REAL(kind=C_FLOAT) :: matrix_trace
  END FUNCTION matrix_trace

  ! Matrix MatrixTranslate(float x, float y, float z)
  FUNCTION matrix_translate(x, y, z) BIND(c, name='MatrixTranslate')
    IMPORT :: C_FLOAT, matrix_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: x
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: y
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: z
    TYPE(matrix_) :: matrix_translate
  END FUNCTION matrix_translate

  ! Matrix MatrixTranspose(Matrix mat)
  FUNCTION matrix_transpose(mat) BIND(c, name='MatrixTranspose')
    IMPORT :: matrix_
    IMPLICIT NONE
    TYPE(matrix_), INTENT(in), VALUE :: mat
    TYPE(matrix_) :: matrix_transpose
  END FUNCTION matrix_transpose

  ! float Normalize(float value, float start, float end)
  FUNCTION normalize(VALUE, start, END) BIND(c, name='Normalize')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: VALUE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: start
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: END
    REAL(kind=C_FLOAT) :: normalize
  END FUNCTION normalize

  ! Quaternion QuaternionAdd(Quaternion q1, Quaternion q2)
  FUNCTION quaternion_add(q1, q2) BIND(c, name='QuaternionAdd')
    IMPORT :: quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q1
    TYPE(quaternion_), INTENT(in), VALUE :: q2
    TYPE(quaternion_) :: quaternion_add
  END FUNCTION quaternion_add

  ! Quaternion QuaternionAddValue(Quaternion q, float add)
  FUNCTION quaternion_add_value(q, add) BIND(c, name='QuaternionAddValue')
    IMPORT :: C_FLOAT, quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: add
    TYPE(quaternion_) :: quaternion_add_value
  END FUNCTION quaternion_add_value

  ! Quaternion QuaternionDivide(Quaternion q1, Quaternion q2)
  FUNCTION quaternion_divide(q1, q2) BIND(c, name='QuaternionDivide')
    IMPORT :: quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q1
    TYPE(quaternion_), INTENT(in), VALUE :: q2
    TYPE(quaternion_) :: quaternion_divide
  END FUNCTION quaternion_divide

  ! int QuaternionEquals(Quaternion p, Quaternion q)
  FUNCTION quaternion_equals(p, q) BIND(c, name='QuaternionEquals')
    IMPORT :: C_INT, quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: p
    TYPE(quaternion_), INTENT(in), VALUE :: q
    INTEGER(kind=C_INT) :: quaternion_equals
  END FUNCTION quaternion_equals

  ! Quaternion QuaternionFromAxisAngle(Vector3 axis, float angle)
        function quaternion_from_axis_angle(axis, angle) bind(c, name='QuaternionFromAxisAngle')
    IMPORT :: C_FLOAT, quaternion_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: axis
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: angle
    TYPE(quaternion_) :: quaternion_from_axis_angle
  END FUNCTION quaternion_from_axis_angle

  ! Quaternion QuaternionFromEuler(float pitch, float yaw, float roll)
        function quaternion_from_euler(pitch, yaw, roll) bind(c, name='QuaternionFromEuler')
    IMPORT :: C_FLOAT, quaternion_
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: pitch
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: yaw
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: roll
    TYPE(quaternion_) :: quaternion_from_euler
  END FUNCTION quaternion_from_euler

  ! Quaternion QuaternionFromMatrix(Matrix mat)
  FUNCTION quaternion_from_matrix(mat) BIND(c, name='QuaternionFromMatrix')
    IMPORT :: matrix_, quaternion_
    IMPLICIT NONE
    TYPE(matrix_), INTENT(in), VALUE :: mat
    TYPE(quaternion_) :: quaternion_from_matrix
  END FUNCTION quaternion_from_matrix

  ! Quaternion QuaternionFromVector3ToVector3(Vector3 from, Vector3 to)
        function quaternion_from_vector3_to_vector3(from, to) bind(c, name='QuaternionFromVector3ToVector3')
    IMPORT :: quaternion_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: from
    TYPE(vector3_), INTENT(in), VALUE :: to
    TYPE(quaternion_) :: quaternion_from_vector3_to_vector3
  END FUNCTION quaternion_from_vector3_to_vector3

  ! Quaternion QuaternionIdentity(void)
  FUNCTION quaternion_identity() BIND(c, name='QuaternionIdentity')
    IMPORT :: quaternion_
    IMPLICIT NONE
    TYPE(quaternion_) :: quaternion_identity
  END FUNCTION quaternion_identity

  ! Quaternion QuaternionInvert(Quaternion q)
  FUNCTION quaternion_invert(q) BIND(c, name='QuaternionInvert')
    IMPORT :: quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q
    TYPE(quaternion_) :: quaternion_invert
  END FUNCTION quaternion_invert

  ! float QuaternionLength(Quaternion q)
  FUNCTION quaternion_length(q) BIND(c, name='QuaternionLength')
    IMPORT :: C_FLOAT, quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q
    REAL(kind=C_FLOAT) :: quaternion_length
  END FUNCTION quaternion_length

  ! Quaternion QuaternionLerp(Quaternion q1, Quaternion q2, float amount)
  FUNCTION quaternion_lerp(q1, q2, amount) BIND(c, name='QuaternionLerp')
    IMPORT :: C_FLOAT, quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q1
    TYPE(quaternion_), INTENT(in), VALUE :: q2
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: amount
    TYPE(quaternion_) :: quaternion_lerp
  END FUNCTION quaternion_lerp

  ! Quaternion QuaternionMultiply(Quaternion q1, Quaternion q2)
  FUNCTION quaternion_multiply(q1, q2) BIND(c, name='QuaternionMultiply')
    IMPORT :: quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q1
    TYPE(quaternion_), INTENT(in), VALUE :: q2
    TYPE(quaternion_) :: quaternion_multiply
  END FUNCTION quaternion_multiply

  ! Quaternion QuaternionNlerp(Quaternion q1, Quaternion q2, float amount)
  FUNCTION quaternion_nlerp(q1, q2, amount) BIND(c, name='QuaternionNlerp')
    IMPORT :: C_FLOAT, quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q1
    TYPE(quaternion_), INTENT(in), VALUE :: q2
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: amount
    TYPE(quaternion_) :: quaternion_nlerp
  END FUNCTION quaternion_nlerp

  ! Quaternion QuaternionNormalize(Quaternion q)
  FUNCTION quaternion_normalize(q) BIND(c, name='QuaternionNormalize')
    IMPORT :: quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q
    TYPE(quaternion_) :: quaternion_normalize
  END FUNCTION quaternion_normalize

  ! Quaternion QuaternionScale(Quaternion q, float mul)
  FUNCTION quaternion_scale(q, mul) BIND(c, name='QuaternionScale')
    IMPORT :: C_FLOAT, quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: mul
    TYPE(quaternion_) :: quaternion_scale
  END FUNCTION quaternion_scale

  ! Quaternion QuaternionSlerp(Quaternion q1, Quaternion q2, float amount)
  FUNCTION quaternion_slerp(q1, q2, amount) BIND(c, name='QuaternionSlerp')
    IMPORT :: C_FLOAT, quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q1
    TYPE(quaternion_), INTENT(in), VALUE :: q2
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: amount
    TYPE(quaternion_) :: quaternion_slerp
  END FUNCTION quaternion_slerp

  ! Quaternion QuaternionSubtract(Quaternion q1, Quaternion q2)
  FUNCTION quaternion_subtract(q1, q2) BIND(c, name='QuaternionSubtract')
    IMPORT :: quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q1
    TYPE(quaternion_), INTENT(in), VALUE :: q2
    TYPE(quaternion_) :: quaternion_subtract
  END FUNCTION quaternion_subtract

  ! Quaternion QuaternionSubtractValue(Quaternion q, float sub)
        function quaternion_subtract_value(q, sub) bind(c, name='QuaternionSubtractValue')
    IMPORT :: C_FLOAT, quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: sub
    TYPE(quaternion_) :: quaternion_subtract_value
  END FUNCTION quaternion_subtract_value

  ! void QuaternionToAxisAngle(Quaternion q, Vector3 *outAxis, float *outAngle)
        subroutine quaternion_to_axis_angle(q, out_axis, out_angle) bind(c, name='QuaternionToAxisAngle')
    IMPORT :: C_FLOAT, quaternion_, vector3_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q
    TYPE(vector3_), INTENT(inout) :: out_axis(*)
    REAL(kind=C_FLOAT), INTENT(out) :: out_angle
  END SUBROUTINE quaternion_to_axis_angle

  ! Vector3 QuaternionToEuler(Quaternion q)
  FUNCTION quaternion_to_euler(q) BIND(c, name='QuaternionToEuler')
    IMPORT :: quaternion_, vector3_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q
    TYPE(vector3_) :: quaternion_to_euler
  END FUNCTION quaternion_to_euler

  ! Matrix QuaternionToMatrix(Quaternion q)
  FUNCTION quaternion_to_matrix(q) BIND(c, name='QuaternionToMatrix')
    IMPORT :: matrix_, quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q
    TYPE(matrix_) :: quaternion_to_matrix
  END FUNCTION quaternion_to_matrix

  ! Quaternion QuaternionTransform(Quaternion q, Matrix mat)
  FUNCTION quaternion_transform(q, mat) BIND(c, name='QuaternionTransform')
    IMPORT :: matrix_, quaternion_
    IMPLICIT NONE
    TYPE(quaternion_), INTENT(in), VALUE :: q
    TYPE(matrix_), INTENT(in), VALUE :: mat
    TYPE(quaternion_) :: quaternion_transform
  END FUNCTION quaternion_transform

  ! float Remap(float value, float inputStart, float inputEnd, float outputStart, float outputEnd)
        function remap(value, input_start, input_end, output_start, output_end) bind(c, name='Remap')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: VALUE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: input_start
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: input_end
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: output_start
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: output_end
    REAL(kind=C_FLOAT) :: remap
  END FUNCTION remap

  ! Vector2 Vector2Add(Vector2 v1, Vector2 v2)
  FUNCTION vector2_add(v1, v2) BIND(c, name='Vector2Add')
    IMPORT :: vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v1
    TYPE(vector2_), INTENT(in), VALUE :: v2
    TYPE(vector2_) :: vector2_add
  END FUNCTION vector2_add

  ! Vector2 Vector2AddValue(Vector2 v, float add)
  FUNCTION vector2_add_value(v, add) BIND(c, name='Vector2AddValue')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: add
    TYPE(vector2_) :: vector2_add_value
  END FUNCTION vector2_add_value

  ! float Vector2Angle(Vector2 v1, Vector2 v2)
  FUNCTION vector2_angle(v1, v2) BIND(c, name='Vector2Angle')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v1
    TYPE(vector2_), INTENT(in), VALUE :: v2
    REAL(kind=C_FLOAT) :: vector2_angle
  END FUNCTION vector2_angle

  ! Vector2 Vector2Clamp(Vector2 v, Vector2 min, Vector2 max)
  FUNCTION vector2_clamp(v, min, max) BIND(c, name='Vector2Clamp')
    IMPORT :: vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    TYPE(vector2_), INTENT(in), VALUE :: min
    TYPE(vector2_), INTENT(in), VALUE :: max
    TYPE(vector2_) :: vector2_clamp
  END FUNCTION vector2_clamp

  ! Vector2 Vector2ClampValue(Vector2 v, float min, float max)
  FUNCTION vector2_clamp_value(v, min, max) BIND(c, name='Vector2ClampValue')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: min
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: max
    TYPE(vector2_) :: vector2_clamp_value
  END FUNCTION vector2_clamp_value

  ! float Vector2Distance(Vector2 v1, Vector2 v2)
  FUNCTION vector2_distance(v1, v2) BIND(c, name='Vector2Distance')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v1
    TYPE(vector2_), INTENT(in), VALUE :: v2
    REAL(kind=C_FLOAT) :: vector2_distance
  END FUNCTION vector2_distance

  ! float Vector2DistanceSqr(Vector2 v1, Vector2 v2)
  FUNCTION vector2_distance_sqr(v1, v2) BIND(c, name='Vector2DistanceSqr')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v1
    TYPE(vector2_), INTENT(in), VALUE :: v2
    REAL(kind=C_FLOAT) :: vector2_distance_sqr
  END FUNCTION vector2_distance_sqr

  ! Vector2 Vector2Divide(Vector2 v1, Vector2 v2)
  FUNCTION vector2_divide(v1, v2) BIND(c, name='Vector2Divide')
    IMPORT :: vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v1
    TYPE(vector2_), INTENT(in), VALUE :: v2
    TYPE(vector2_) :: vector2_divide
  END FUNCTION vector2_divide

  ! float Vector2DotProduct(Vector2 v1, Vector2 v2)
  FUNCTION vector2_dot_product(v1, v2) BIND(c, name='Vector2DotProduct')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v1
    TYPE(vector2_), INTENT(in), VALUE :: v2
    REAL(kind=C_FLOAT) :: vector2_dot_product
  END FUNCTION vector2_dot_product

  ! int Vector2Equals(Vector2 p, Vector2 q)
  FUNCTION vector2_equals(p, q) BIND(c, name='Vector2Equals')
    IMPORT :: C_INT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: p
    TYPE(vector2_), INTENT(in), VALUE :: q
    INTEGER(kind=C_INT) :: vector2_equals
  END FUNCTION vector2_equals

  ! Vector2 Vector2Invert(Vector2 v)
  FUNCTION vector2_invert(v) BIND(c, name='Vector2Invert')
    IMPORT :: vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    TYPE(vector2_) :: vector2_invert
  END FUNCTION vector2_invert

  ! float Vector2Length(Vector2 v)
  FUNCTION vector2_length(v) BIND(c, name='Vector2Length')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    REAL(kind=C_FLOAT) :: vector2_length
  END FUNCTION vector2_length

  ! float Vector2LengthSqr(Vector2 v)
  FUNCTION vector2_length_sqr(v) BIND(c, name='Vector2LengthSqr')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    REAL(kind=C_FLOAT) :: vector2_length_sqr
  END FUNCTION vector2_length_sqr

  ! Vector2 Vector2Lerp(Vector2 v1, Vector2 v2, float amount)
  FUNCTION vector2_lerp(v1, v2, amount) BIND(c, name='Vector2Lerp')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v1
    TYPE(vector2_), INTENT(in), VALUE :: v2
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: amount
    TYPE(vector2_) :: vector2_lerp
  END FUNCTION vector2_lerp

  ! float Vector2LineAngle(Vector2 start, Vector2 end)
  FUNCTION vector2_line_angle(start, END) BIND(c, name='Vector2LineAngle')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: start
    TYPE(vector2_), INTENT(in), VALUE :: END
    REAL(kind=C_FLOAT) :: vector2_line_angle
  END FUNCTION vector2_line_angle

  ! Vector2 Vector2MoveTowards(Vector2 v, Vector2 target, float maxDistance)
        function vector2_move_towards(v, target, max_distance) bind(c, name='Vector2MoveTowards')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    TYPE(vector2_), INTENT(in), VALUE :: TARGET
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: max_distance
    TYPE(vector2_) :: vector2_move_towards
  END FUNCTION vector2_move_towards

  ! Vector2 Vector2Multiply(Vector2 v1, Vector2 v2)
  FUNCTION vector2_multiply(v1, v2) BIND(c, name='Vector2Multiply')
    IMPORT :: vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v1
    TYPE(vector2_), INTENT(in), VALUE :: v2
    TYPE(vector2_) :: vector2_multiply
  END FUNCTION vector2_multiply

  ! Vector2 Vector2Negate(Vector2 v)
  FUNCTION vector2_negate(v) BIND(c, name='Vector2Negate')
    IMPORT :: vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    TYPE(vector2_) :: vector2_negate
  END FUNCTION vector2_negate

  ! Vector2 Vector2Normalize(Vector2 v)
  FUNCTION vector2_normalize(v) BIND(c, name='Vector2Normalize')
    IMPORT :: vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    TYPE(vector2_) :: vector2_normalize
  END FUNCTION vector2_normalize

  ! Vector2 Vector2One(void)
  FUNCTION vector2_one() BIND(c, name='Vector2One')
    IMPORT :: vector2_
    IMPLICIT NONE
    TYPE(vector2_) :: vector2_one
  END FUNCTION vector2_one

  ! Vector2 Vector2Reflect(Vector2 v, Vector2 normal)
  FUNCTION vector2_reflect(v, normal) BIND(c, name='Vector2Reflect')
    IMPORT :: vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    TYPE(vector2_), INTENT(in), VALUE :: normal
    TYPE(vector2_) :: vector2_reflect
  END FUNCTION vector2_reflect

  ! Vector2 Vector2Rotate(Vector2 v, float angle)
  FUNCTION vector2_rotate(v, angle) BIND(c, name='Vector2Rotate')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: angle
    TYPE(vector2_) :: vector2_rotate
  END FUNCTION vector2_rotate

  ! Vector2 Vector2Scale(Vector2 v, float scale)
  FUNCTION vector2_scale(v, scale) BIND(c, name='Vector2Scale')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scale
    TYPE(vector2_) :: vector2_scale
  END FUNCTION vector2_scale

  ! Vector2 Vector2Subtract(Vector2 v1, Vector2 v2)
  FUNCTION vector2_subtract(v1, v2) BIND(c, name='Vector2Subtract')
    IMPORT :: vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v1
    TYPE(vector2_), INTENT(in), VALUE :: v2
    TYPE(vector2_) :: vector2_subtract
  END FUNCTION vector2_subtract

  ! Vector2 Vector2SubtractValue(Vector2 v, float sub)
  FUNCTION vector2_subtract_value(v, sub) BIND(c, name='Vector2SubtractValue')
    IMPORT :: C_FLOAT, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: sub
    TYPE(vector2_) :: vector2_subtract_value
  END FUNCTION vector2_subtract_value

  ! Vector2 Vector2Transform(Vector2 v, Matrix mat)
  FUNCTION vector2_transform(v, mat) BIND(c, name='Vector2Transform')
    IMPORT :: matrix_, vector2_
    IMPLICIT NONE
    TYPE(vector2_), INTENT(in), VALUE :: v
    TYPE(matrix_), INTENT(in), VALUE :: mat
    TYPE(vector2_) :: vector2_transform
  END FUNCTION vector2_transform

  ! Vector2 Vector2Zero(void)
  FUNCTION vector2_zero() BIND(c, name='Vector2Zero')
    IMPORT :: vector2_
    IMPLICIT NONE
    TYPE(vector2_) :: vector2_zero
  END FUNCTION vector2_zero

  ! Vector3 Vector3Add(Vector3 v1, Vector3 v2)
  FUNCTION vector3_add(v1, v2) BIND(c, name='Vector3Add')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v1
    TYPE(vector3_), INTENT(in), VALUE :: v2
    TYPE(vector3_) :: vector3_add
  END FUNCTION vector3_add

  ! Vector3 Vector3AddValue(Vector3 v, float add)
  FUNCTION vector3_add_value(v, add) BIND(c, name='Vector3AddValue')
    IMPORT :: C_FLOAT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: add
    TYPE(vector3_) :: vector3_add_value
  END FUNCTION vector3_add_value

  ! float Vector3Angle(Vector3 v1, Vector3 v2)
  FUNCTION vector3_angle(v1, v2) BIND(c, name='Vector3Angle')
    IMPORT :: C_FLOAT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v1
    TYPE(vector3_), INTENT(in), VALUE :: v2
    REAL(kind=C_FLOAT) :: vector3_angle
  END FUNCTION vector3_angle

  ! Vector3 Vector3Barycenter(Vector3 p, Vector3 a, Vector3 b, Vector3 c)
  FUNCTION vector3_barycenter(p, a, b, c) BIND(c, name='Vector3Barycenter')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: p
    TYPE(vector3_), INTENT(in), VALUE :: a
    TYPE(vector3_), INTENT(in), VALUE :: b
    TYPE(vector3_), INTENT(in), VALUE :: c
    TYPE(vector3_) :: vector3_barycenter
  END FUNCTION vector3_barycenter

  ! Vector3 Vector3Clamp(Vector3 v, Vector3 min, Vector3 max)
  FUNCTION vector3_clamp(v, min, max) BIND(c, name='Vector3Clamp')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    TYPE(vector3_), INTENT(in), VALUE :: min
    TYPE(vector3_), INTENT(in), VALUE :: max
    TYPE(vector3_) :: vector3_clamp
  END FUNCTION vector3_clamp

  ! Vector3 Vector3ClampValue(Vector3 v, float min, float max)
  FUNCTION vector3_clamp_value(v, min, max) BIND(c, name='Vector3ClampValue')
    IMPORT :: C_FLOAT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: min
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: max
    TYPE(vector3_) :: vector3_clamp_value
  END FUNCTION vector3_clamp_value

  ! Vector3 Vector3CrossProduct(Vector3 v1, Vector3 v2)
  FUNCTION vector3_cross_product(v1, v2) BIND(c, name='Vector3CrossProduct')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v1
    TYPE(vector3_), INTENT(in), VALUE :: v2
    TYPE(vector3_) :: vector3_cross_product
  END FUNCTION vector3_cross_product

  ! float Vector3Distance(Vector3 v1, Vector3 v2)
  FUNCTION vector3_distance(v1, v2) BIND(c, name='Vector3Distance')
    IMPORT :: C_FLOAT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v1
    TYPE(vector3_), INTENT(in), VALUE :: v2
    REAL(kind=C_FLOAT) :: vector3_distance
  END FUNCTION vector3_distance

  ! float Vector3DistanceSqr(Vector3 v1, Vector3 v2)
  FUNCTION vector3_distance_sqr(v1, v2) BIND(c, name='Vector3DistanceSqr')
    IMPORT :: C_FLOAT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v1
    TYPE(vector3_), INTENT(in), VALUE :: v2
    REAL(kind=C_FLOAT) :: vector3_distance_sqr
  END FUNCTION vector3_distance_sqr

  ! Vector3 Vector3Divide(Vector3 v1, Vector3 v2)
  FUNCTION vector3_divide(v1, v2) BIND(c, name='Vector3Divide')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v1
    TYPE(vector3_), INTENT(in), VALUE :: v2
    TYPE(vector3_) :: vector3_divide
  END FUNCTION vector3_divide

  ! float Vector3DotProduct(Vector3 v1, Vector3 v2)
  FUNCTION vector3_dot_product(v1, v2) BIND(c, name='Vector3DotProduct')
    IMPORT :: C_FLOAT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v1
    TYPE(vector3_), INTENT(in), VALUE :: v2
    REAL(kind=C_FLOAT) :: vector3_dot_product
  END FUNCTION vector3_dot_product

  ! int Vector3Equals(Vector3 p, Vector3 q)
  FUNCTION vector3_equals(p, q) BIND(c, name='Vector3Equals')
    IMPORT :: C_INT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: p
    TYPE(vector3_), INTENT(in), VALUE :: q
    INTEGER(kind=C_INT) :: vector3_equals
  END FUNCTION vector3_equals

  ! Vector3 Vector3Invert(Vector3 v)
  FUNCTION vector3_invert(v) BIND(c, name='Vector3Invert')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    TYPE(vector3_) :: vector3_invert
  END FUNCTION vector3_invert

  ! float Vector3Length(const Vector3 v)
  FUNCTION vector3_length(v) BIND(c, name='Vector3Length')
    IMPORT :: C_FLOAT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    REAL(kind=C_FLOAT) :: vector3_length
  END FUNCTION vector3_length

  ! float Vector3LengthSqr(const Vector3 v)
  FUNCTION vector3_length_sqr(v) BIND(c, name='Vector3LengthSqr')
    IMPORT :: C_FLOAT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    REAL(kind=C_FLOAT) :: vector3_length_sqr
  END FUNCTION vector3_length_sqr

  ! Vector3 Vector3Lerp(Vector3 v1, Vector3 v2, float amount)
  FUNCTION vector3_lerp(v1, v2, amount) BIND(c, name='Vector3Lerp')
    IMPORT :: C_FLOAT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v1
    TYPE(vector3_), INTENT(in), VALUE :: v2
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: amount
    TYPE(vector3_) :: vector3_lerp
  END FUNCTION vector3_lerp

  ! Vector3 Vector3Max(Vector3 v1, Vector3 v2)
  FUNCTION vector3_max(v1, v2) BIND(c, name='Vector3Max')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v1
    TYPE(vector3_), INTENT(in), VALUE :: v2
    TYPE(vector3_) :: vector3_max
  END FUNCTION vector3_max

  ! Vector3 Vector3Min(Vector3 v1, Vector3 v2)
  FUNCTION vector3_min(v1, v2) BIND(c, name='Vector3Min')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v1
    TYPE(vector3_), INTENT(in), VALUE :: v2
    TYPE(vector3_) :: vector3_min
  END FUNCTION vector3_min

  ! Vector3 Vector3Multiply(Vector3 v1, Vector3 v2)
  FUNCTION vector3_multiply(v1, v2) BIND(c, name='Vector3Multiply')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v1
    TYPE(vector3_), INTENT(in), VALUE :: v2
    TYPE(vector3_) :: vector3_multiply
  END FUNCTION vector3_multiply

  ! Vector3 Vector3Negate(Vector3 v)
  FUNCTION vector3_negate(v) BIND(c, name='Vector3Negate')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    TYPE(vector3_) :: vector3_negate
  END FUNCTION vector3_negate

  ! Vector3 Vector3Normalize(Vector3 v)
  FUNCTION vector3_normalize(v) BIND(c, name='Vector3Normalize')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    TYPE(vector3_) :: vector3_normalize
  END FUNCTION vector3_normalize

  ! Vector3 Vector3One(void)
  FUNCTION vector3_one() BIND(c, name='Vector3One')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_) :: vector3_one
  END FUNCTION vector3_one

  ! void Vector3OrthoNormalize(Vector3 *v1, Vector3 *v2)
        subroutine vector3_ortho_normalize(v1, v2) bind(c, name='Vector3OrthoNormalize')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(inout) :: v1(*)
    TYPE(vector3_), INTENT(inout) :: v2(*)
  END SUBROUTINE vector3_ortho_normalize

  ! Vector3 Vector3Perpendicular(Vector3 v)
  FUNCTION vector3_perpendicular(v) BIND(c, name='Vector3Perpendicular')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    TYPE(vector3_) :: vector3_perpendicular
  END FUNCTION vector3_perpendicular

  ! Vector3 Vector3Reflect(Vector3 v, Vector3 normal)
  FUNCTION vector3_reflect(v, normal) BIND(c, name='Vector3Reflect')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    TYPE(vector3_), INTENT(in), VALUE :: normal
    TYPE(vector3_) :: vector3_reflect
  END FUNCTION vector3_reflect

  ! Vector3 Vector3Refract(Vector3 v, Vector3 n, float r)
  FUNCTION vector3_refract(v, n, r) BIND(c, name='Vector3Refract')
    IMPORT :: C_FLOAT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    TYPE(vector3_), INTENT(in), VALUE :: n
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: r
    TYPE(vector3_) :: vector3_refract
  END FUNCTION vector3_refract

  ! Vector3 Vector3RotateByAxisAngle(Vector3 v, Vector3 axis, float angle)
        function vector3_rotate_by_axis_angle(v, axis, angle) bind(c, name='Vector3RotateByAxisAngle')
    IMPORT :: C_FLOAT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    TYPE(vector3_), INTENT(in), VALUE :: axis
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: angle
    TYPE(vector3_) :: vector3_rotate_by_axis_angle
  END FUNCTION vector3_rotate_by_axis_angle

  ! Vector3 Vector3RotateByQuaternion(Vector3 v, Quaternion q)
        function vector3_rotate_by_quaternion(v, q) bind(c, name='Vector3RotateByQuaternion')
    IMPORT :: quaternion_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    TYPE(quaternion_), INTENT(in), VALUE :: q
    TYPE(vector3_) :: vector3_rotate_by_quaternion
  END FUNCTION vector3_rotate_by_quaternion

  ! Vector3 Vector3Scale(Vector3 v, float scalar)
  FUNCTION vector3_scale(v, scalar) BIND(c, name='Vector3Scale')
    IMPORT :: C_FLOAT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: scalar
    TYPE(vector3_) :: vector3_scale
  END FUNCTION vector3_scale

  ! Vector3 Vector3Subtract(Vector3 v1, Vector3 v2)
  FUNCTION vector3_subtract(v1, v2) BIND(c, name='Vector3Subtract')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v1
    TYPE(vector3_), INTENT(in), VALUE :: v2
    TYPE(vector3_) :: vector3_subtract
  END FUNCTION vector3_subtract

  ! Vector3 Vector3SubtractValue(Vector3 v, float sub)
  FUNCTION vector3_subtract_value(v, sub) BIND(c, name='Vector3SubtractValue')
    IMPORT :: C_FLOAT, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: sub
    TYPE(vector3_) :: vector3_subtract_value
  END FUNCTION vector3_subtract_value

  ! float3 Vector3ToFloatV(Vector3 v)
  FUNCTION vector3_to_float_v(v) BIND(c, name='Vector3ToFloatV')
    IMPORT :: float3_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    TYPE(float3_) :: vector3_to_float_v
  END FUNCTION vector3_to_float_v

  ! Vector3 Vector3Transform(Vector3 v, Matrix mat)
  FUNCTION vector3_transform(v, mat) BIND(c, name='Vector3Transform')
    IMPORT :: matrix_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: v
    TYPE(matrix_), INTENT(in), VALUE :: mat
    TYPE(vector3_) :: vector3_transform
  END FUNCTION vector3_transform

  ! Vector3 Vector3Unproject(Vector3 source, Matrix projection, Matrix view)
        function vector3_unproject(source, projection, view) bind(c, name='Vector3Unproject')
    IMPORT :: matrix_, vector3_
    IMPLICIT NONE
    TYPE(vector3_), INTENT(in), VALUE :: source
    TYPE(matrix_), INTENT(in), VALUE :: projection
    TYPE(matrix_), INTENT(in), VALUE :: view
    TYPE(vector3_) :: vector3_unproject
  END FUNCTION vector3_unproject

  ! Vector3 Vector3Zero(void)
  FUNCTION vector3_zero() BIND(c, name='Vector3Zero')
    IMPORT :: vector3_
    IMPLICIT NONE
    TYPE(vector3_) :: vector3_zero
  END FUNCTION vector3_zero

  ! float Wrap(float value, float min, float max)
  FUNCTION wrap(VALUE, min, max) BIND(c, name='Wrap')
    IMPORT :: C_FLOAT
    IMPLICIT NONE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: VALUE
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: min
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: max
    REAL(kind=C_FLOAT) :: wrap
  END FUNCTION wrap
END INTERFACE
END MODULE raylib_math
