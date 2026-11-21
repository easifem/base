!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing HighorderElement_Quadrangle for a cubic element: name,
! entity counts, edge topology and interpolation point type

PROGRAM main
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: ReferenceQuadrangle_
USE GlobalData, ONLY: GaussLegendreLobatto
USE GlobalData, ONLY: I4B
USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: HighorderElement_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: QuadrangleName
USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(ReferenceQuadrangle_) :: ref
TYPE(ReferenceElement_) :: high

ref = ReferenceQuadrangle(nsd=2_I4B)
CALL HighorderElement_Quadrangle(refelem=ref, order=3_I4B, obj=high, &
                                 ipType=GaussLegendreLobatto)

! Test 1: name, order and entity counts
BLOCK
  CALL OK(high%name .EQ. QuadrangleName(order=3_I4B), &
          "order=3: element name")
  CALL OK(high%order .EQ. 3, "order=3: element order")
  CALL OK(ALL(high%entityCounts .EQ. [16, 4, 1, 0]), &
          "order=3: entity counts")
  CALL OK(SIZE(high%xij, 2) .EQ. 16, "order=3: 16 nodes")
END BLOCK

! Test 2: edge topology agrees with GetEdgeConnectivity_Quadrangle
BLOCK
  INTEGER(I4B) :: con(4, 4), ii

  CALL GetEdgeConnectivity_Quadrangle(con=con, opt=2_I4B, order=3_I4B)
  DO ii = 1, 4
    CALL OK(ALL(high%topology(16 + ii)%nptrs .EQ. con(:, ii)), &
            "order=3: edge topology agrees with edge connectivity")
  END DO
END BLOCK

! Test 3: the element stores its interpolation point type
BLOCK
  CALL OK(high%interpolationPointType .EQ. GaussLegendreLobatto, &
          "order=3: interpolation point type is stored")
END BLOCK

END PROGRAM main
