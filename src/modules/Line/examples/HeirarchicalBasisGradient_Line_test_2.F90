!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Gradient of the hierarchical line basis with orientation and on
! the unit line.
!
! On the biunit line L2' = 3x/SQRT(6) and L3' = 2.5 (3x**2 - 1)/SQRT(10).
! The gradient must be the derivative of the values returned by
! HeirarchicalBasis_Line_ with the same orient.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line
USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line_
USE LineInterpolationUtility, ONLY: HeirarchicalBasis_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xb(1, 3)

xb(1, :) = [-0.5_DFP, 0.0_DFP, 0.5_DFP]

! Test 1: orient = 1 matches the analytic gradients
BLOCK
  REAL(DFP), ALLOCATABLE :: g(:, :, :)
  g = HeirarchicalBasisGradient_Line(order=3_I4B, xij=xb, &
                                     refLine="BIUNIT", orient=1_I4B)
  CALL OK(ALL(SHAPE(g) == [3, 4, 1]), "Shape is 3 by 4 by 1")
  CALL OK(MAXVAL(ABS(g(:, 3, 1) - 3.0_DFP * xb(1, :) / SQRT(6.0_DFP))) &
          < tol, "Gradient of L2")
  CALL OK(MAXVAL(ABS(g(:, 4, 1) - 2.5_DFP * (3.0_DFP * xb(1, :)**2 - &
                                              1.0_DFP) / SQRT(10.0_DFP))) &
          < tol, "Gradient of L3")
END BLOCK

! Test 2: orient = -1; gradient equals finite difference of the values
BLOCK
  REAL(DFP), PARAMETER :: h = 1.0E-6_DFP
  REAL(DFP) :: g(3, 4, 1), vp(3, 4), vm(3, 4), xp(1, 3), xm(1, 3)
  INTEGER(I4B) :: dim1, dim2, dim3, nrow, ncol
  xp = xb + h
  xm = xb - h
  CALL HeirarchicalBasisGradient_Line_(order=3_I4B, xij=xb, &
                                       refLine="BIUNIT", orient=-1_I4B, &
                                       ans=g, dim1=dim1, dim2=dim2, &
                                       dim3=dim3)
  CALL HeirarchicalBasis_Line_(order=3_I4B, xij=xp, refLine="BIUNIT", &
                               orient=-1_I4B, ans=vp, nrow=nrow, ncol=ncol)
  CALL HeirarchicalBasis_Line_(order=3_I4B, xij=xm, refLine="BIUNIT", &
                               orient=-1_I4B, ans=vm, nrow=nrow, ncol=ncol)
  CALL OK(dim1 == 3 .AND. dim2 == 4 .AND. dim3 == 1, &
          "Subroutine dimensions")
  CALL OK(MAXVAL(ABS(g(:, :, 1) - (vp - vm) / (2.0_DFP * h))) < 1.0E-6_DFP, &
          "orient -1: gradient is the derivative of the values")
END BLOCK

! Test 3: unit line; chain rule gives a factor 2
BLOCK
  REAL(DFP) :: xu(1, 3), g(3, 3, 1)
  xu(1, :) = [0.25_DFP, 0.5_DFP, 0.75_DFP]
  g = HeirarchicalBasisGradient_Line(order=2_I4B, xij=xu, refLine="UNIT")
  CALL OK(MAXVAL(ABS(g(:, 3, 1) - 6.0_DFP * xb(1, :) / SQRT(6.0_DFP))) &
          < tol, "UNIT: gradient of L2 scaled by 2")
END BLOCK

END PROGRAM main
