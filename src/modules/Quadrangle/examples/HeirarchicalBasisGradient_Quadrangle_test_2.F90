!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing HeirarchicalBasisGradient_Quadrangle against central
! finite differences of HeirarchicalBasis_Quadrangle

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: HeirarchicalBasis_Quadrangle
USE QuadrangleInterpolationUtility, &
  ONLY: HeirarchicalBasisGradient_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: h = 1.0E-6_DFP, tol = 1.0E-7_DFP
INTEGER(I4B), PARAMETER :: pb = 3, qb = 2, pe3 = 3, pe4 = 2, qe1 = 2, &
                           qe2 = 3
! 4 vertex, 2 bottom, 2 right, 1 top, 1 left, 2 cell modes
INTEGER(I4B), PARAMETER :: nb = 12
REAL(DFP) :: xij(2, 2)

xij(:, 1) = [0.2_DFP, -0.4_DFP]
xij(:, 2) = [-0.3_DFP, 0.5_DFP]

! Test 1: entity orders with default orientation
BLOCK
  REAL(DFP) :: g(2, nb, 2), fp(2, nb), fm(2, nb), xh(2, 2), d(2, nb)
  INTEGER(I4B) :: idim

  g = HeirarchicalBasisGradient_Quadrangle(pb=pb, qb=qb, pe3=pe3, &
                                           pe4=pe4, qe1=qe1, qe2=qe2, &
                                           xij=xij)
  DO idim = 1, 2
    xh = xij
    xh(idim, :) = xij(idim, :) + h
    fp = HeirarchicalBasis_Quadrangle(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
                                      qe1=qe1, qe2=qe2, xij=xh)
    xh(idim, :) = xij(idim, :) - h
    fm = HeirarchicalBasis_Quadrangle(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
                                      qe1=qe1, qe2=qe2, xij=xh)
    d = (fp - fm) / (2.0_DFP * h)
    CALL OK(MAXVAL(ABS(g(:, :, idim) - d)) .LT. tol, &
            "default orientation: gradient agrees with finite differences")
  END DO
END BLOCK

! Test 2: flipped edges and face
BLOCK
  REAL(DFP), ALLOCATABLE :: g(:, :, :), fp(:, :), fm(:, :)
  REAL(DFP) :: xh(2, 2)
  INTEGER(I4B) :: idim
  INTEGER(I4B), PARAMETER :: fo(3) = [-1, -1, 1]

  g = HeirarchicalBasisGradient_Quadrangle(pb=pb, qb=qb, pe3=pe3, &
                                           pe4=pe4, qe1=qe1, qe2=qe2, &
                                           xij=xij, qe1Orient=1_I4B, &
                                           qe2Orient=-1_I4B, &
                                           pe3Orient=-1_I4B, &
                                           pe4Orient=1_I4B, faceOrient=fo)
  CALL OK(ALL(SHAPE(g) .EQ. [2, nb, 2]), "oriented gradient: shape")
  DO idim = 1, 2
    xh = xij
    xh(idim, :) = xij(idim, :) + h
    fp = HeirarchicalBasis_Quadrangle(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
                                      qe1=qe1, qe2=qe2, xij=xh, &
                                      pe3Orient=-1_I4B, pe4Orient=1_I4B, &
                                      qe1Orient=1_I4B, qe2Orient=-1_I4B, &
                                      faceOrient=fo)
    xh(idim, :) = xij(idim, :) - h
    fm = HeirarchicalBasis_Quadrangle(pb=pb, qb=qb, pe3=pe3, pe4=pe4, &
                                      qe1=qe1, qe2=qe2, xij=xh, &
                                      pe3Orient=-1_I4B, pe4Orient=1_I4B, &
                                      qe1Orient=1_I4B, qe2Orient=-1_I4B, &
                                      faceOrient=fo)
    CALL OK(MAXVAL(ABS(g(:, :, idim) - (fp - fm) / (2.0_DFP * h))) &
            .LT. tol, &
            "flipped orientation: gradient agrees with finite differences")
  END DO
END BLOCK

END PROGRAM main
