!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceTriangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReferenceTriangle_Method, ONLY: FaceShapeMetaData_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: face(3), sorted(3), orientation(2), local(3)

face = [3_I4B, 1_I4B, 2_I4B]
CALL FaceShapeMetaData_Triangle(face, sorted, orientation, local)
CALL OK(ALL(sorted == [1_I4B, 2_I4B, 3_I4B]), "Canonical vertex order")
CALL OK(ALL(orientation == [1_I4B, 1_I4B]), &
  "Rotation and orientation code")
END PROGRAM main
