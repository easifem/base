! raylib.f90
!
! A collection of auto-generated Fortran 2018 interface bindings to
! raylib 5.1.
!
! Author:  Philipp Engel
! Licence: ISC

MODULE RaylibCheckMethods
USE, INTRINSIC :: ISO_C_BINDING
USE RaylibTypes
USE RaylibEnums
IMPLICIT NONE
PRIVATE

PUBLIC :: CheckCollisionSpheres
PUBLIC :: CheckCollisionRecs
PUBLIC :: CheckCollisionPointTriangle
PUBLIC :: CheckCollisionPointRec
PUBLIC :: CheckCollisionPointPoly
PUBLIC :: CheckCollisionPointLine
PUBLIC :: CheckCollisionPointCircle
PUBLIC :: CheckCollisionLines
PUBLIC :: CheckCollisionCircles
PUBLIC :: CheckCollisionCircleRec
PUBLIC :: CheckCollisionBoxes
PUBLIC :: CheckCollisionBoxSphere

INTERFACE
  ! bool CheckCollisionBoxSphere(BoundingBox box, Vector3 center, float radius)
  FUNCTION CheckCollisionBoxSphere(box, center, radius) BIND(c, &
                                               name='CheckCollisionBoxSphere')
    IMPORT :: bounding_box_type, C_BOOL, C_FLOAT, vector3_type
    IMPLICIT NONE
    TYPE(bounding_box_type), INTENT(in), VALUE :: box
    TYPE(vector3_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    LOGICAL(kind=C_BOOL) :: CheckCollisionBoxSphere
  END FUNCTION CheckCollisionBoxSphere

  ! bool CheckCollisionBoxes(BoundingBox box1, BoundingBox box2)
  FUNCTION CheckCollisionBoxes(box1, box2) BIND(c, name='CheckCollisionBoxes')
    IMPORT :: bounding_box_type, C_BOOL
    IMPLICIT NONE
    TYPE(bounding_box_type), INTENT(in), VALUE :: box1
    TYPE(bounding_box_type), INTENT(in), VALUE :: box2
    LOGICAL(kind=C_BOOL) :: CheckCollisionBoxes
  END FUNCTION CheckCollisionBoxes

  ! bool CheckCollisionCircleRec(Vector2 center, float radius, Rectangle rec)
  FUNCTION CheckCollisionCircleRec(center, radius, rec) BIND(c, &
                                               name='CheckCollisionCircleRec')
    IMPORT :: C_BOOL, C_FLOAT, rectangle_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    LOGICAL(kind=C_BOOL) :: CheckCollisionCircleRec
  END FUNCTION CheckCollisionCircleRec

  ! bool CheckCollisionCircles(Vector2 center1, float radius1, Vector2 center2, float radius2)
        function CheckCollisionCircles(center1, radius1, center2, radius2) bind(c, name='CheckCollisionCircles')
    IMPORT :: C_BOOL, C_FLOAT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: center1
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius1
    TYPE(vector2_type), INTENT(in), VALUE :: center2
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius2
    LOGICAL(kind=C_BOOL) :: CheckCollisionCircles
  END FUNCTION CheckCollisionCircles

  ! bool CheckCollisionLines(Vector2 startPos1, Vector2 endPos1, Vector2 startPos2, Vector2 endPos2, Vector2 *collisionPoint)
        function CheckCollisionLines(start_pos1, end_pos1, start_pos2, end_pos2, collision_point) &
    BIND(c, name='CheckCollisionLines')
    IMPORT :: C_BOOL, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: start_pos1
    TYPE(vector2_type), INTENT(in), VALUE :: end_pos1
    TYPE(vector2_type), INTENT(in), VALUE :: start_pos2
    TYPE(vector2_type), INTENT(in), VALUE :: end_pos2
    TYPE(vector2_type), INTENT(out) :: collision_point
    LOGICAL(kind=C_BOOL) :: CheckCollisionLines
  END FUNCTION CheckCollisionLines

  ! bool CheckCollisionPointCircle(Vector2 point, Vector2 center, float radius)
        function CheckCollisionPointCircle(point, center, radius) bind(c, name='CheckCollisionPointCircle')
    IMPORT :: C_BOOL, C_FLOAT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: point
    TYPE(vector2_type), INTENT(in), VALUE :: center
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius
    LOGICAL(kind=C_BOOL) :: CheckCollisionPointCircle
  END FUNCTION CheckCollisionPointCircle

  ! bool CheckCollisionPointLine(Vector2 point, Vector2 p1, Vector2 p2, int threshold)
        function CheckCollisionPointLine(point, p1, p2, threshold) bind(c, name='CheckCollisionPointLine')
    IMPORT :: C_BOOL, C_INT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: point
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: p2
    INTEGER(kind=C_INT), INTENT(in), VALUE :: threshold
    LOGICAL(kind=C_BOOL) :: CheckCollisionPointLine
  END FUNCTION CheckCollisionPointLine

  ! bool CheckCollisionPointPoly(Vector2 point, Vector2 *points, int pointCount)
        function CheckCollisionPointPoly(point, points, point_count) bind(c, name='CheckCollisionPointPoly')
    IMPORT :: C_BOOL, C_INT, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: point
    TYPE(vector2_type), INTENT(in) :: points(*)
    INTEGER(kind=C_INT), INTENT(in), VALUE :: point_count
    LOGICAL(kind=C_BOOL) :: CheckCollisionPointPoly
  END FUNCTION CheckCollisionPointPoly

  ! bool CheckCollisionPointRec(Vector2 point, Rectangle rec)
        function CheckCollisionPointRec(point, rec) bind(c, name='CheckCollisionPointRec')
    IMPORT :: C_BOOL, rectangle_type, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: point
    TYPE(rectangle_type), INTENT(in), VALUE :: rec
    LOGICAL(kind=C_BOOL) :: CheckCollisionPointRec
  END FUNCTION CheckCollisionPointRec

  ! bool CheckCollisionPointTriangle(Vector2 point, Vector2 p1, Vector2 p2, Vector2 p3)
        function CheckCollisionPointTriangle(point, p1, p2, p3) bind(c, name='CheckCollisionPointTriangle')
    IMPORT :: C_BOOL, vector2_type
    IMPLICIT NONE
    TYPE(vector2_type), INTENT(in), VALUE :: point
    TYPE(vector2_type), INTENT(in), VALUE :: p1
    TYPE(vector2_type), INTENT(in), VALUE :: p2
    TYPE(vector2_type), INTENT(in), VALUE :: p3
    LOGICAL(kind=C_BOOL) :: CheckCollisionPointTriangle
  END FUNCTION CheckCollisionPointTriangle

  ! bool CheckCollisionRecs(Rectangle rec1, Rectangle rec2)
  FUNCTION CheckCollisionRecs(rec1, rec2) BIND(c, name='CheckCollisionRecs')
    IMPORT :: C_BOOL, rectangle_type
    IMPLICIT NONE
    TYPE(rectangle_type), INTENT(in), VALUE :: rec1
    TYPE(rectangle_type), INTENT(in), VALUE :: rec2
    LOGICAL(kind=C_BOOL) :: CheckCollisionRecs
  END FUNCTION CheckCollisionRecs

  ! bool CheckCollisionSpheres(Vector3 center1, float radius1, Vector3 center2, float radius2)
        function CheckCollisionSpheres(center1, radius1, center2, radius2) bind(c, name='CheckCollisionSpheres')
    IMPORT :: C_BOOL, C_FLOAT, vector3_type
    IMPLICIT NONE
    TYPE(vector3_type), INTENT(in), VALUE :: center1
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius1
    TYPE(vector3_type), INTENT(in), VALUE :: center2
    REAL(kind=C_FLOAT), INTENT(in), VALUE :: radius2
    LOGICAL(kind=C_BOOL) :: CheckCollisionSpheres
  END FUNCTION CheckCollisionSpheres
END INTERFACE

END MODULE RaylibCheckMethods
