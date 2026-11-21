!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing InterpolationPoint_Quadrangle for anisotropic orders and
! for a mapped element

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Equidistance
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE QuadrangleInterpolationUtility, ONLY: InterpolationPoint_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

! Test 1: p=3, q=2, VEFC layout, all nodes are distinct
BLOCK
  REAL(DFP), ALLOCATABLE :: xij(:, :)

  xij = InterpolationPoint_Quadrangle(p=3_I4B, q=2_I4B, &
                                      ipType1=Equidistance, &
                                      ipType2=Equidistance, layout="VEFC")
  CALL OK(ALL(SHAPE(xij) .EQ. [2, 12]), "p=3, q=2: shape")
  CALL OK(AllDistinct(xij), "p=3, q=2: all nodes are distinct")
  CALL OK(MAXVAL(ABS(xij(:, 1:4) - RESHAPE([-1.0_DFP, -1.0_DFP, &
          1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP], &
          [2, 4]))) .LT. tol, "p=3, q=2: first four nodes are vertices")
END BLOCK

! Test 2: p=2, q=3, VEFC layout, all nodes are distinct
BLOCK
  REAL(DFP), ALLOCATABLE :: xij(:, :)

  xij = InterpolationPoint_Quadrangle(p=2_I4B, q=3_I4B, &
                                      ipType1=Equidistance, &
                                      ipType2=Equidistance, layout="VEFC")
  CALL OK(ALL(SHAPE(xij) .EQ. [2, 12]), "p=2, q=3: shape")
  CALL OK(AllDistinct(xij), "p=2, q=3: all nodes are distinct")
END BLOCK

! Test 3: nodes mapped to the unit square
BLOCK
  REAL(DFP), ALLOCATABLE :: xij(:, :), ref(:, :)
  REAL(DFP) :: corner(2, 4)

  corner(:, 1) = [0.0_DFP, 0.0_DFP]
  corner(:, 2) = [1.0_DFP, 0.0_DFP]
  corner(:, 3) = [1.0_DFP, 1.0_DFP]
  corner(:, 4) = [0.0_DFP, 1.0_DFP]

  ref = InterpolationPoint_Quadrangle(order=2_I4B, ipType=Equidistance, &
                                      layout="VEFC")
  xij = InterpolationPoint_Quadrangle(order=2_I4B, ipType=Equidistance, &
                                      layout="VEFC", xij=corner)
  CALL OK(MAXVAL(ABS(xij - 0.5_DFP * (ref + 1.0_DFP))) .LT. tol, &
          "unit square: x = (xi + 1) / 2")
END BLOCK

CONTAINS

FUNCTION AllDistinct(x) RESULT(ans)
  REAL(DFP), INTENT(IN) :: x(:, :)
  LOGICAL(LGT) :: ans
  INTEGER(I4B) :: ii, jj

  ans = .TRUE.
  DO ii = 1, SIZE(x, 2)
    DO jj = ii + 1, SIZE(x, 2)
      IF (MAXVAL(ABS(x(:, ii) - x(:, jj))) .LT. tol) ans = .FALSE.
    END DO
  END DO
END FUNCTION AllDistinct

END PROGRAM main
