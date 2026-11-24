!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Lagrange coefficients of a quadratic line from nodes, from the
! Vandermonde matrix, and from its LU factors.
!
! Nodes are -1, 1, 0 (vertex-first order). In the monomial basis
! [1, x, x**2] the Lagrange polynomials are
! l1 = (x**2 - x)/2, l2 = (x**2 + x)/2, l3 = 1 - x**2.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Lapack_Method, ONLY: GetLU
USE LineInterpolationUtility, ONLY: LagrangeCoeff_Line
USE LineInterpolationUtility, ONLY: LagrangeCoeff_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: nodes(1, 3), v(3, 3), c1(3), c2(3), c3(3)
INTEGER(I4B) :: ii

nodes(1, :) = [-1.0_DFP, 1.0_DFP, 0.0_DFP]
! Vandermonde matrix, v(i, j) = x_i**(j-1)
DO ii = 1, 3
  v(ii, :) = [1.0_DFP, nodes(1, ii), nodes(1, ii)**2]
END DO
c1 = [0.0_DFP, -0.5_DFP, 0.5_DFP]
c2 = [0.0_DFP, 0.5_DFP, 0.5_DFP]
c3 = [1.0_DFP, 0.0_DFP, -1.0_DFP]

! Test 1: coefficients of one basis function from the nodes
BLOCK
  REAL(DFP) :: c(3)
  c = LagrangeCoeff_Line(order=2_I4B, i=1_I4B, xij=nodes)
  CALL OK(MAXVAL(ABS(c - c1)) < tol, "From nodes: l1")
END BLOCK

! Test 2: coefficients of one basis function from the Vandermonde matrix
BLOCK
  REAL(DFP) :: c(3)
  c = LagrangeCoeff_Line(order=2_I4B, i=2_I4B, v=v, isVandermonde=.TRUE.)
  CALL OK(MAXVAL(ABS(c - c2)) < tol, "From Vandermonde: l2")
END BLOCK

! Test 3: coefficients of one basis function from the LU factors
BLOCK
  REAL(DFP) :: c(3), vlu(3, 3)
  INTEGER(I4B) :: ipiv(3), info
  vlu = v
  CALL GetLU(A=vlu, IPIV=ipiv, info=info)
  c = LagrangeCoeff_Line(order=2_I4B, i=3_I4B, v=vlu, ipiv=ipiv)
  CALL OK(MAXVAL(ABS(c - c3)) < tol, "From LU factors: l3")
END BLOCK

! Test 4: all coefficients from the nodes; V times C is the identity
BLOCK
  REAL(DFP) :: c(3, 3), r(3, 3)
  c = LagrangeCoeff_Line(order=2_I4B, xij=nodes)
  r = MATMUL(v, c)
  DO ii = 1, 3
    r(ii, ii) = r(ii, ii) - 1.0_DFP
  END DO
  CALL OK(MAXVAL(ABS(r)) < tol, "All coefficients: V C = I")
END BLOCK

! Test 5: subroutine forms write the same coefficients
BLOCK
  REAL(DFP) :: c(4), vlu(3, 3), cm(4, 4)
  INTEGER(I4B) :: ipiv(3), info, tsize, nrow, ncol
  LOGICAL(LGT) :: isok

  CALL LagrangeCoeff_Line_(order=2_I4B, i=1_I4B, xij=nodes, ans=c, &
                           tsize=tsize)
  isok = tsize == 3 .AND. MAXVAL(ABS(c(1:3) - c1)) < tol
  CALL OK(isok, "Subroutine from nodes: l1")

  CALL LagrangeCoeff_Line_(order=2_I4B, i=2_I4B, v=v, &
                           isVandermonde=.TRUE., ans=c, tsize=tsize)
  isok = tsize == 3 .AND. MAXVAL(ABS(c(1:3) - c2)) < tol
  CALL OK(isok, "Subroutine from Vandermonde: l2")

  vlu = v
  CALL GetLU(A=vlu, IPIV=ipiv, info=info)
  CALL LagrangeCoeff_Line_(order=2_I4B, i=3_I4B, v=vlu, ipiv=ipiv, &
                           ans=c, tsize=tsize)
  isok = tsize == 3 .AND. MAXVAL(ABS(c(1:3) - c3)) < tol
  CALL OK(isok, "Subroutine from LU factors: l3")

  CALL LagrangeCoeff_Line_(order=2_I4B, xij=nodes, ans=cm, nrow=nrow, &
                           ncol=ncol)
  isok = nrow == 3 .AND. ncol == 3
  CALL OK(isok, "Subroutine all coefficients: 3 by 3")
  isok = MAXVAL(ABS(cm(1:3, 1) - c1)) < tol .AND. &
         MAXVAL(ABS(cm(1:3, 2) - c2)) < tol .AND. &
         MAXVAL(ABS(cm(1:3, 3) - c3)) < tol
  CALL OK(isok, "Subroutine all coefficients: columns are l1, l2, l3")
END BLOCK

END PROGRAM main
