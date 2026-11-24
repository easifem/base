!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing that both FacetElements_Quadrangle overloads give the
! same edge topology for a 9-node quadrangle

PROGRAM main
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: ReferenceQuadrangle_
USE GlobalData, ONLY: Equidistance
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: Quadrangle9
USE ReferenceQuadrangle_Method, ONLY: FacetElements_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: HighorderElement_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(ReferenceQuadrangle_) :: ref
TYPE(ReferenceElement_) :: high, fromElem(4), fromType(4)
INTEGER(I4B) :: ii

ref = ReferenceQuadrangle(nsd=2_I4B)
CALL HighorderElement_Quadrangle(refelem=ref, order=2_I4B, obj=high, &
                                 ipType=Equidistance)
CALL FacetElements_Quadrangle(refelem=high, ans=fromElem)
CALL FacetElements_Quadrangle(elemType=Quadrangle9, nsd=2_I4B, &
                              ans=fromType)

! Test 1: the last topology entry of each edge lists all three nodes
BLOCK
  CALL OK(ALL(fromType(1)%topology(4)%nptrs .EQ. [1, 2, 5]), &
          "elemType: edge 1 cell has nodes 1, 2, 5")
  CALL OK(ALL(fromType(4)%topology(4)%nptrs .EQ. [4, 1, 8]), &
          "elemType: edge 4 cell has nodes 4, 1, 8")
END BLOCK

! Test 2: both overloads agree on every edge
BLOCK
  LOGICAL(LGT) :: same

  same = .TRUE.
  DO ii = 1, 4
    IF (SIZE(fromType(ii)%topology(4)%nptrs) .NE. &
        SIZE(fromElem(ii)%topology(4)%nptrs)) THEN
      same = .FALSE.
    ELSE IF (ANY(fromType(ii)%topology(4)%nptrs .NE. &
                 fromElem(ii)%topology(4)%nptrs)) THEN
      same = .FALSE.
    END IF
  END DO
  CALL OK(same, "refelem and elemType overloads give the same edges")
END BLOCK

END PROGRAM main
