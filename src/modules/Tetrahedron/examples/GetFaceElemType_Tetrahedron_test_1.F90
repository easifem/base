!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing GetFaceElemType_Tetrahedron, both overloads

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Tetrahedron10
USE GlobalData, ONLY: Tetrahedron20
USE GlobalData, ONLY: Triangle3
USE GlobalData, ONLY: Triangle6
USE GlobalData, ONLY: Triangle10
USE ReferenceTetrahedron_Method, ONLY: GetFaceElemType_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: all faces, elemType omitted, so Tetrahedron4 is used
BLOCK
  INTEGER(I4B) :: faceType(4), tFaceNodes(4)
  CALL GetFaceElemType_Tetrahedron(faceElemType=faceType, &
                                   tFaceNodes=tFaceNodes)
  CALL OK(ALL(faceType .EQ. Triangle3), &
          "default elemType gives Triangle3 faces")
  CALL OK(ALL(tFaceNodes .EQ. 3), "default elemType gives 3 face nodes")
END BLOCK

! Test 2: all faces of a quadratic tetrahedron
BLOCK
  INTEGER(I4B) :: faceType(4), tFaceNodes(4)
  CALL GetFaceElemType_Tetrahedron(elemType=Tetrahedron10, &
                                   faceElemType=faceType, &
                                   tFaceNodes=tFaceNodes)
  CALL OK(ALL(faceType .EQ. Triangle6), "Tetrahedron10 has Triangle6 faces")
  CALL OK(ALL(tFaceNodes .EQ. 6), "Tetrahedron10 has 6 nodes per face")
END BLOCK

! Test 3: one local face of a cubic tetrahedron
BLOCK
  INTEGER(I4B) :: faceType, tFaceNodes
  CALL GetFaceElemType_Tetrahedron(elemType=Tetrahedron20, &
                                   localFaceNumber=3_I4B, &
                                   faceElemType=faceType, &
                                   tFaceNodes=tFaceNodes)
  CALL OK(faceType .EQ. Triangle10, "Tetrahedron20 face 3 is Triangle10")
  CALL OK(tFaceNodes .EQ. 10, "Tetrahedron20 face 3 has 10 nodes")
END BLOCK

END PROGRAM main
