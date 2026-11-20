!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing HeirarchicalBasis_Triangle with uniform and mixed
! edge orders

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: CellBasis_Triangle
USE TriangleInterpolationUtility, ONLY: EdgeBasis_Triangle
USE TriangleInterpolationUtility, ONLY: GetHierarchicalDOF_Triangle
USE TriangleInterpolationUtility, ONLY: HeirarchicalBasis_Triangle
USE TriangleInterpolationUtility, ONLY: VertexBasis_Triangle
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: x(2, 3)

! three points inside the BIUNIT triangle
x(:, 1) = [-0.6_DFP, -0.4_DFP]
x(:, 2) = [0.0_DFP, -0.6_DFP]
x(:, 3) = [-0.8_DFP, 0.2_DFP]

! Test 1: overload with one order equals overload with pe1=pe2=pe3=order
BLOCK
  REAL(DFP) :: a(3, 15), b(3, 15)

  a = HeirarchicalBasis_Triangle(order=4_I4B, xij=x, refTriangle="BIUNIT")
  b = HeirarchicalBasis_Triangle(order=4_I4B, pe1=4_I4B, pe2=4_I4B, &
                                 pe3=4_I4B, xij=x, refTriangle="BIUNIT")
  CALL OK(MAXVAL(ABS(a - b)) .LT. tol, "order 4: both overloads agree")
END BLOCK

! Test 2: vertex functions are the barycentric coordinates
BLOCK
  REAL(DFP) :: vert(2, 3), a(3, 3)
  INTEGER(I4B) :: ii

  vert(:, 1) = [-1.0_DFP, -1.0_DFP]
  vert(:, 2) = [1.0_DFP, -1.0_DFP]
  vert(:, 3) = [-1.0_DFP, 1.0_DFP]

  a = HeirarchicalBasis_Triangle(order=1_I4B, pe1=1_I4B, pe2=1_I4B, &
                                 pe3=1_I4B, xij=vert, refTriangle="BIUNIT")
  DO ii = 1, 3
    a(ii, ii) = a(ii, ii) - 1.0_DFP
  END DO
  CALL OK(MAXVAL(ABS(a)) .LT. tol, "vertex functions at vertices")
END BLOCK

! Test 3: mixed edge orders, columns are vertex, edge, then cell modes
BLOCK
  INTEGER(I4B), PARAMETER :: order = 4, pe1 = 2, pe2 = 3, pe3 = 4
  INTEGER(I4B) :: nv, ne, nc
  REAL(DFP) :: a(3, 15)

  nv = GetHierarchicalDOF_Triangle(order, pe1, pe2, pe3, "V")
  ne = GetHierarchicalDOF_Triangle(order, pe1, pe2, pe3, "E")
  nc = GetHierarchicalDOF_Triangle(order, pe1, pe2, pe3, "C")
  CALL OK(nv + ne + nc .EQ. &
          GetHierarchicalDOF_Triangle(order, pe1, pe2, pe3, "H"), &
          "mixed orders: total count")

  a(:, 1:nv + ne + nc) = HeirarchicalBasis_Triangle(order=order, &
                         pe1=pe1, pe2=pe2, pe3=pe3, xij=x, &
                         refTriangle="BIUNIT")

  CALL OK(MAXVAL(ABS(a(:, 1:nv) - VertexBasis_Triangle(xij=x, &
          refTriangle="BIUNIT"))) .LT. tol, "mixed orders: vertex part")

  CALL OK(MAXVAL(ABS(a(:, nv + 1:nv + ne) - EdgeBasis_Triangle(pe1=pe1, &
          pe2=pe2, pe3=pe3, xij=x, refTriangle="BIUNIT"))) .LT. tol, &
          "mixed orders: edge part")

  CALL OK(MAXVAL(ABS(a(:, nv + ne + 1:nv + ne + nc) - &
          CellBasis_Triangle(order=order, xij=x, refTriangle="BIUNIT"))) &
          .LT. tol, "mixed orders: cell part")
END BLOCK

END PROGRAM main
