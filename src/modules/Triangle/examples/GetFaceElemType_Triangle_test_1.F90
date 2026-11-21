!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing GetFaceElemType_Triangle (both overloads)

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Line3
USE GlobalData, ONLY: Line4
USE GlobalData, ONLY: Line5
USE GlobalData, ONLY: Line6
USE GlobalData, ONLY: Triangle10
USE GlobalData, ONLY: Triangle12
USE GlobalData, ONLY: Triangle15
USE GlobalData, ONLY: Triangle15b
USE GlobalData, ONLY: Triangle21
USE GlobalData, ONLY: Triangle3
USE GlobalData, ONLY: Triangle6
USE ReferenceTriangle_Method, ONLY: GetFaceElemType_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: all faces of a complete element (overload 1)
BLOCK
  INTEGER(I4B) :: faceElemType(3), tFaceNodes(3)

  CALL GetFaceElemType_Triangle(elemType=Triangle6, &
                                faceElemType=faceElemType, &
                                tFaceNodes=tFaceNodes)
  CALL OK(ALL(faceElemType .EQ. Line3), "Triangle6: faces are Line3")
  CALL OK(ALL(tFaceNodes .EQ. 3), "Triangle6: three nodes per face")

  CALL GetFaceElemType_Triangle(elemType=Triangle10, &
                                faceElemType=faceElemType, &
                                tFaceNodes=tFaceNodes)
  CALL OK(ALL(faceElemType .EQ. Line4), "Triangle10: faces are Line4")

  CALL GetFaceElemType_Triangle(elemType=Triangle15, &
                                faceElemType=faceElemType, &
                                tFaceNodes=tFaceNodes)
  CALL OK(ALL(faceElemType .EQ. Line5), "Triangle15: faces are Line5")

  CALL GetFaceElemType_Triangle(elemType=Triangle21, &
                                faceElemType=faceElemType, &
                                tFaceNodes=tFaceNodes)
  CALL OK(ALL(faceElemType .EQ. Line6), "Triangle21: faces are Line6")
END BLOCK

! Test 2: default element type is the linear triangle (overload 1)
BLOCK
  INTEGER(I4B) :: faceElemType(3), tFaceNodes(3)

  CALL GetFaceElemType_Triangle(faceElemType=faceElemType, &
                                tFaceNodes=tFaceNodes)
  CALL OK(ALL(faceElemType .EQ. Line2), "default: faces are Line2")
  CALL OK(ALL(tFaceNodes .EQ. 2), "default: two nodes per face")
END BLOCK

! Test 3: incomplete (serendipity) elements (overload 1)
! Triangle12 is a quartic element, so its faces have 5 nodes.
! Triangle15b is a quintic element, so its faces have 6 nodes.
BLOCK
  INTEGER(I4B) :: faceElemType(3), tFaceNodes(3)

  faceElemType = -1
  tFaceNodes = -1
  CALL GetFaceElemType_Triangle(elemType=Triangle12, &
                                faceElemType=faceElemType, &
                                tFaceNodes=tFaceNodes)
  CALL OK(ALL(faceElemType .EQ. Line5) .AND. ALL(tFaceNodes .EQ. 5), &
          "Triangle12: faces are Line5")

  faceElemType = -1
  tFaceNodes = -1
  CALL GetFaceElemType_Triangle(elemType=Triangle15b, &
                                faceElemType=faceElemType, &
                                tFaceNodes=tFaceNodes)
  CALL OK(ALL(faceElemType .EQ. Line6) .AND. ALL(tFaceNodes .EQ. 6), &
          "Triangle15b: faces are Line6")
END BLOCK

! Test 4: one face (overload 2)
BLOCK
  INTEGER(I4B) :: faceElemType, tFaceNodes, ii
  LOGICAL :: isok

  isok = .TRUE.
  DO ii = 1, 3
    CALL GetFaceElemType_Triangle(elemType=Triangle3, &
                                  localFaceNumber=ii, &
                                  faceElemType=faceElemType, &
                                  tFaceNodes=tFaceNodes)
    isok = isok .AND. (faceElemType .EQ. Line2) .AND. (tFaceNodes .EQ. 2)
  END DO
  CALL OK(isok, "Triangle3: every face is Line2")

  CALL GetFaceElemType_Triangle(elemType=Triangle6, localFaceNumber=2_I4B, &
                                faceElemType=faceElemType, &
                                tFaceNodes=tFaceNodes)
  CALL OK(faceElemType .EQ. Line3 .AND. tFaceNodes .EQ. 3, &
          "Triangle6: face 2 is Line3")
END BLOCK

END PROGRAM main
