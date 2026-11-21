!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Face element type of a line, for all faces and for one face.

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Line3
USE GlobalData, ONLY: Point1
USE ReferenceLine_Method, ONLY: GetFaceElemType_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: all faces; elemType is optional
BLOCK
  INTEGER(I4B) :: faceTypes(2), faceNodes(2)
  CALL GetFaceElemType_Line(faceElemType=faceTypes, tFaceNodes=faceNodes)
  CALL OK(ALL(faceTypes == Point1), "Both faces are Point1")
  CALL OK(ALL(faceNodes == 1), "Each face has one node")
END BLOCK

! Test 2: one face of a linear line
BLOCK
  INTEGER(I4B) :: faceType, faceNodes
  CALL GetFaceElemType_Line(elemType=Line2, localFaceNumber=1_I4B, &
                            faceElemType=faceType, tFaceNodes=faceNodes)
  CALL OK(faceType == Point1, "Face 1 of Line2 is Point1")
  CALL OK(faceNodes == 1, "Face 1 of Line2 has one node")
END BLOCK

! Test 3: one face of a quadratic line; faces are still points
BLOCK
  INTEGER(I4B) :: faceType, faceNodes
  CALL GetFaceElemType_Line(elemType=Line3, localFaceNumber=2_I4B, &
                            faceElemType=faceType, tFaceNodes=faceNodes)
  CALL OK(faceType == Point1, "Face 2 of Line3 is Point1")
  CALL OK(faceNodes == 1, "Face 2 of Line3 has one node")
END BLOCK

END PROGRAM main
