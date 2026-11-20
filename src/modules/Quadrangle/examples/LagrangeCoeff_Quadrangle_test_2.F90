!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing LagrangeCoeff_Quadrangle from nodes, from a Vandermonde
! matrix, from its LU factors, and for anisotropic orders

PROGRAM main
USE BaseType, ONLY: elem => TypeElemNameOpt
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GE_LUMethods, ONLY: GetLU
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde
USE QuadrangleInterpolationUtility, ONLY: LagrangeCoeff_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: LagrangeCoeff_Quadrangle_
USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: nodes(2, 4)
REAL(DFP), ALLOCATABLE :: v(:, :)

nodes = RefCoord_Quadrangle("BIUNIT")
v = LagrangeVandermonde(xij=nodes, order=1_I4B, elemType=elem%Quadrangle)

! Test 1: coefficients of basis function 2 from the nodes
BLOCK
  REAL(DFP) :: c(4), e(4)

  c = LagrangeCoeff_Quadrangle(order=1_I4B, i=2_I4B, xij=nodes)
  e = 0.0_DFP
  e(2) = 1.0_DFP
  CALL OK(MAXVAL(ABS(MATMUL(v, c) - e)) .LT. tol, &
          "from nodes: V c = e_2")
END BLOCK

! Test 2: coefficients from the Vandermonde matrix
BLOCK
  REAL(DFP) :: c(4), e(4)

  c = LagrangeCoeff_Quadrangle(order=1_I4B, i=3_I4B, v=v, &
                               isVandermonde=.TRUE.)
  e = 0.0_DFP
  e(3) = 1.0_DFP
  CALL OK(MAXVAL(ABS(MATMUL(v, c) - e)) .LT. tol, &
          "from Vandermonde: V c = e_3")
END BLOCK

! Test 3: coefficients from the LU factors of the Vandermonde matrix
BLOCK
  REAL(DFP) :: c(4), e(4), vlu(4, 4)
  INTEGER(I4B) :: ipiv(4)

  vlu = v
  CALL GetLU(A=vlu, IPIV=ipiv)
  c = LagrangeCoeff_Quadrangle(order=1_I4B, i=4_I4B, v=vlu, ipiv=ipiv)
  e = 0.0_DFP
  e(4) = 1.0_DFP
  CALL OK(MAXVAL(ABS(MATMUL(v, c) - e)) .LT. tol, &
          "from LU factors: V c = e_4")
END BLOCK

! Test 4: subroutine form returns the size of the coefficient vector
BLOCK
  REAL(DFP) :: c(4), e(4)
  INTEGER(I4B) :: tsize

  CALL LagrangeCoeff_Quadrangle_(order=1_I4B, i=1_I4B, xij=nodes, ans=c, &
                                 tsize=tsize)
  e = 0.0_DFP
  e(1) = 1.0_DFP
  CALL OK(tsize .EQ. 4, "subroutine form: tsize = 4")
  CALL OK(MAXVAL(ABS(MATMUL(v, c) - e)) .LT. tol, &
          "subroutine form: V c = e_1")
END BLOCK

! Test 5: anisotropic orders p=2, q=1 with monomials in both directions
BLOCK
  REAL(DFP) :: pts(2, 6), t(6, 6), c(6, 6), eye(6, 6)
  INTEGER(I4B) :: ii, k1, k2

  pts(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP, -1.0_DFP, 0.0_DFP, 1.0_DFP]
  pts(2, :) = [-1.0_DFP, -1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP]

  ! t(ii, k2*3+k1+1) = x^k1 * y^k2, the x exponent varies fastest
  DO ii = 1, 6
    DO k2 = 0, 1
      DO k1 = 0, 2
        t(ii, k2 * 3 + k1 + 1) = pts(1, ii)**k1 * pts(2, ii)**k2
      END DO
    END DO
  END DO

  c = LagrangeCoeff_Quadrangle(p=2_I4B, q=1_I4B, xij=pts, &
                               basisType1=poly%monomial, &
                               basisType2=poly%monomial)
  eye = 0.0_DFP
  DO ii = 1, 6
    eye(ii, ii) = 1.0_DFP
  END DO
  CALL OK(MAXVAL(ABS(MATMUL(t, c) - eye)) .LT. tol, &
          "p=2, q=1: T C = I")
END BLOCK

END PROGRAM main
