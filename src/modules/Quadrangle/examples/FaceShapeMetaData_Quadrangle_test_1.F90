!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing FaceShapeMetaData_Quadrangle for a face that does not
! start at its smallest vertex

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReferenceQuadrangle_Method, ONLY: FaceShapeMetaData_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: face [7, 3, 9, 5] starts at 3 and runs in reverse
! Rotated: [3, 9, 5, 7]. Canonical: [3, 7, 5, 9].
BLOCK
  INTEGER(I4B) :: face(4), sorted(4), orient(3), local(4)

  face = [7, 3, 9, 5]
  CALL FaceShapeMetaData_Quadrangle(face=face, sorted_face=sorted, &
                                    faceOrient=orient, localFaces=local)
  CALL OK(ALL(local .EQ. [3, 9, 5, 7]), "rotated to the smallest vertex")
  CALL OK(ALL(sorted .EQ. [3, 7, 5, 9]), "second and fourth are sorted")
  CALL OK(orient(3) .EQ. -1, "reversed face: orient(3) = -1")
  CALL OK(ALL(orient(1:2) .EQ. 1), "orient(1:2) = 1")
  CALL OK(ALL(face .EQ. [7, 3, 9, 5]), "face is not changed")
END BLOCK

! Test 2: face [4, 8, 6, 2] starts at 2 and keeps its direction
! Rotated: [2, 4, 8, 6]. Canonical: [2, 4, 8, 6].
BLOCK
  INTEGER(I4B) :: face(4), sorted(4), orient(3)

  face = [4, 8, 6, 2]
  CALL FaceShapeMetaData_Quadrangle(face=face, sorted_face=sorted, &
                                    faceOrient=orient)
  CALL OK(ALL(sorted .EQ. [2, 4, 8, 6]), "already canonical")
  CALL OK(ALL(orient .EQ. [1, 1, 1]), "same direction: orient = 1")
END BLOCK

END PROGRAM main
