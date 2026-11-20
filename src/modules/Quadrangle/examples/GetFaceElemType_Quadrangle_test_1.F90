!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing GetFaceElemType_Quadrangle for all faces and for one
! face

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Line3
USE GlobalData, ONLY: Line4
USE GlobalData, ONLY: Quadrangle16
USE GlobalData, ONLY: Quadrangle9
USE ReferenceQuadrangle_Method, ONLY: GetFaceElemType_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: all four faces of a 9-node quadrangle are 3-node lines
BLOCK
  INTEGER(I4B) :: faceElemType(4), tFaceNodes(4)

  CALL GetFaceElemType_Quadrangle(elemType=Quadrangle9, &
                                  faceElemType=faceElemType, &
                                  tFaceNodes=tFaceNodes)
  CALL OK(ALL(faceElemType .EQ. Line3), "Quadrangle9: faces are Line3")
  CALL OK(ALL(tFaceNodes .EQ. 3), "Quadrangle9: 3 nodes per face")
END BLOCK

! Test 2: without elemType the quadrangle is linear
BLOCK
  INTEGER(I4B) :: faceElemType(4), tFaceNodes(4)

  CALL GetFaceElemType_Quadrangle(faceElemType=faceElemType, &
                                  tFaceNodes=tFaceNodes)
  CALL OK(ALL(faceElemType .EQ. Line2), "default: faces are Line2")
  CALL OK(ALL(tFaceNodes .EQ. 2), "default: 2 nodes per face")
END BLOCK

! Test 3: one face of a 16-node quadrangle is a 4-node line
BLOCK
  INTEGER(I4B) :: faceElemType, tFaceNodes

  CALL GetFaceElemType_Quadrangle(elemType=Quadrangle16, &
                                  localFaceNumber=3_I4B, &
                                  faceElemType=faceElemType, &
                                  tFaceNodes=tFaceNodes)
  CALL OK(faceElemType .EQ. Line4, "Quadrangle16: face 3 is Line4")
  CALL OK(tFaceNodes .EQ. 4, "Quadrangle16: face 3 has 4 nodes")
END BLOCK

END PROGRAM main
