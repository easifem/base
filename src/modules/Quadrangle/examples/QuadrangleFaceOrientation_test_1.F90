!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceQuadrangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReferenceQuadrangle_Method, ONLY: FaceShapeMetaData_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: face(4),canonical(4),orientation(3),cyclic(4)

face = [1_I4B,2_I4B,3_I4B,4_I4B]
CALL FaceShapeMetaData_Quadrangle(face,canonical,orientation,cyclic)
CALL OK(ALL(canonical == face), "Canonical cyclic face ordering")
CALL OK(ALL(orientation == [1_I4B,1_I4B,1_I4B]), &
  "Identity orientation has three positive signs")
END PROGRAM main
