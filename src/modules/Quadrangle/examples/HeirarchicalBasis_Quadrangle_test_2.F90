!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing HeirarchicalBasis_Quadrangle for anisotropic orders,
! vertex values, and edge and face orientation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: HeirarchicalBasis_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B), PARAMETER :: pb = 2, qb = 3, pe3 = 2, pe4 = 2, qe1 = 3, &
                           qe2 = 3
! Number of modes: 4 vertex, 1 bottom, 2 right, 1 top, 2 left, 2 cell
INTEGER(I4B), PARAMETER :: nb = 12
REAL(DFP) :: xij(2, 3)

xij(:, 1) = [0.2_DFP, -0.4_DFP]
xij(:, 2) = [-0.3_DFP, 0.5_DFP]
xij(:, 3) = [0.7_DFP, 0.9_DFP]

! Test 1: entity orders, p and q orders, and default orientation agree
BLOCK
  REAL(DFP) :: v1(3, nb), v2(3, nb)
  REAL(DFP), ALLOCATABLE :: v3(:, :)

  v1 = HeirarchicalBasis_Quadrangle(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
                                    qe1=qe1, qe2=qe2, xij=xij)
  v2 = HeirarchicalBasis_Quadrangle(p=pb, q=qb, xij=xij)
  v3 = HeirarchicalBasis_Quadrangle(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
                                    qe1=qe1, qe2=qe2, xij=xij, &
                                    pe3Orient=1_I4B, pe4Orient=1_I4B, &
                                    qe1Orient=1_I4B, qe2Orient=1_I4B, &
                                    faceOrient=[1_I4B, 1_I4B, 1_I4B])
  CALL OK(MAXVAL(ABS(v1 - v2)) .LT. tol, "entity orders equal p, q form")
  CALL OK(ALL(SHAPE(v3) .EQ. [3, nb]), "oriented form: shape")
  CALL OK(MAXVAL(ABS(v1 - v3)) .LT. tol, &
          "positive orientation equals default form")
END BLOCK

! Test 2: at the vertices only the vertex modes are nonzero
BLOCK
  REAL(DFP) :: v(4, nb), vert(2, 4), eye(4, nb)
  INTEGER(I4B) :: ii

  vert(:, 1) = [-1.0_DFP, -1.0_DFP]
  vert(:, 2) = [1.0_DFP, -1.0_DFP]
  vert(:, 3) = [1.0_DFP, 1.0_DFP]
  vert(:, 4) = [-1.0_DFP, 1.0_DFP]
  v = HeirarchicalBasis_Quadrangle(p=pb, q=qb, xij=vert)
  eye = 0.0_DFP
  DO ii = 1, 4
    eye(ii, ii) = 1.0_DFP
  END DO
  CALL OK(MAXVAL(ABS(v - eye)) .LT. tol, "vertex values are a delta")
END BLOCK

! Test 3: flipping the left edge changes the sign of its odd mode
! Left edge modes are columns 9 (k=2) and 10 (k=3)
BLOCK
  REAL(DFP), ALLOCATABLE :: vp(:, :), vm(:, :)

  vp = HeirarchicalBasis_Quadrangle(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
                                    qe1=qe1, qe2=qe2, xij=xij, &
                                    pe3Orient=1_I4B, pe4Orient=1_I4B, &
                                    qe1Orient=1_I4B, qe2Orient=1_I4B, &
                                    faceOrient=[1_I4B, 1_I4B, 1_I4B])
  vm = HeirarchicalBasis_Quadrangle(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
                                    qe1=qe1, qe2=qe2, xij=xij, &
                                    pe3Orient=1_I4B, pe4Orient=1_I4B, &
                                    qe1Orient=-1_I4B, qe2Orient=1_I4B, &
                                    faceOrient=[1_I4B, 1_I4B, 1_I4B])
  CALL OK(MAXVAL(ABS(vm(:, 9) - vp(:, 9))) .LT. tol, &
          "left edge flip: even mode unchanged")
  CALL OK(MAXVAL(ABS(vm(:, 10) + vp(:, 10))) .LT. tol, &
          "left edge flip: odd mode changes sign")
END BLOCK

! Test 4: faceOrient(2) = -1 changes the sign of odd y cell modes
! Cell modes are columns 11 (k2=2) and 12 (k2=3)
BLOCK
  REAL(DFP), ALLOCATABLE :: vp(:, :), vm(:, :)

  vp = HeirarchicalBasis_Quadrangle(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
                                    qe1=qe1, qe2=qe2, xij=xij, &
                                    pe3Orient=1_I4B, pe4Orient=1_I4B, &
                                    qe1Orient=1_I4B, qe2Orient=1_I4B, &
                                    faceOrient=[1_I4B, 1_I4B, 1_I4B])
  vm = HeirarchicalBasis_Quadrangle(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
                                    qe1=qe1, qe2=qe2, xij=xij, &
                                    pe3Orient=1_I4B, pe4Orient=1_I4B, &
                                    qe1Orient=1_I4B, qe2Orient=1_I4B, &
                                    faceOrient=[1_I4B, -1_I4B, 1_I4B])
  CALL OK(MAXVAL(ABS(vm(:, 11) - vp(:, 11))) .LT. tol, &
          "face flip in y: even cell mode unchanged")
  CALL OK(MAXVAL(ABS(vm(:, 12) + vp(:, 12))) .LT. tol, &
          "face flip in y: odd cell mode changes sign")
END BLOCK

END PROGRAM main
