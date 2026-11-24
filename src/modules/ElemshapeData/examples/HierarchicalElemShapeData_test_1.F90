!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Hierarchical shape functions at quadrature points

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ReferenceLine_
USE BaseType, ONLY: TypeH1
USE BaseType, ONLY: TypeHierarchyInterpolation
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE ElemShapeData_Method, ONLY: HierarchicalElemShapeData
USE ElemShapeData_Method, ONLY: Initiate
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Quadrangle4
USE LineInterpolationUtility, ONLY: HeirarchicalBasis_Line_
USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line_
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE ReferenceLine_Method, ONLY: ReferenceLine
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])

! Test 1: line, order 1, N = (1 -+ xi) / 2
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  TYPE(QuadraturePoint_) :: quad
  REAL(DFP) :: xi(2)

  CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                   nips=[2], quadratureType=qpopt%GaussLegendre)
  CALL HierarchicalElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                                 elemType=Line2, refelemCoord=lineCoord, &
                                 domainName="BIUNIT", cellOrder=[1_I4B], &
                                 cellOrient=[1_I4B])
  xi = quad%points(1, 1:2)

  CALL OK(elemsd%nns .EQ. 2 .AND. elemsd%nips .EQ. 2, "line p1: nns, nips")
  CALL OK(elemsd%nsd .EQ. 1 .AND. elemsd%xidim .EQ. 1, &
          "line p1: nsd, xidim")
  CALL OK(ALL(ABS(elemsd%N(1, :) - 0.5_DFP * (1.0_DFP - xi)) .LT. tol), &
          "line p1: N1 = (1 - xi) / 2")
  CALL OK(ALL(ABS(elemsd%N(2, :) - 0.5_DFP * (1.0_DFP + xi)) .LT. tol), &
          "line p1: N2 = (1 + xi) / 2")
  CALL OK(ALL(ABS(elemsd%dNdXi(1, 1, :) + 0.5_DFP) .LT. tol), &
          "line p1: dN1/dxi = -1/2")
  CALL OK(ALL(ABS(elemsd%dNdXi(2, 1, :) - 0.5_DFP) .LT. tol), &
          "line p1: dN2/dxi = 1/2")
  CALL OK(ABS(SUM(elemsd%ws) - 2.0_DFP) .LT. tol, "line p1: sum of ws = 2")
END BLOCK

! Test 2: line, order 2, same values as the line basis routine
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  TYPE(QuadraturePoint_) :: quad
  REAL(DFP) :: basis(3, 3), grad(3, 3, 1)
  INTEGER(I4B) :: nrow, ncol, dim1, dim2, dim3

  CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                   nips=[3], quadratureType=qpopt%GaussLegendre)
  CALL HierarchicalElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                                 elemType=Line2, refelemCoord=lineCoord, &
                                 domainName="BIUNIT", cellOrder=[2_I4B], &
                                 cellOrient=[1_I4B])
  CALL HeirarchicalBasis_Line_(order=2, xij=quad%points(1:1, 1:3), &
                               refLine="BIUNIT", orient=1, ans=basis, &
                               nrow=nrow, ncol=ncol)
  CALL HeirarchicalBasisGradient_Line_(order=2, &
                                       xij=quad%points(1:1, 1:3), &
                                       refLine="BIUNIT", orient=1, &
                                       ans=grad, dim1=dim1, dim2=dim2, &
                                       dim3=dim3)

  CALL OK(elemsd%nns .EQ. 3 .AND. elemsd%nips .EQ. 3, "line p2: nns, nips")
  CALL OK(ALL(ABS(elemsd%N - TRANSPOSE(basis)) .LT. tol), &
          "line p2: N(a, i) = phi_a(xi_i)")
  CALL OK(ALL(ABS(elemsd%dNdXi(:, 1, :) - TRANSPOSE(grad(:, :, 1))) &
              .LT. tol), "line p2: dNdXi(a, 1, i) = phi_a'(xi_i)")
  CALL OK(ALL(ABS(SUM(elemsd%N(1:2, :), dim=1) - 1.0_DFP) .LT. tol), &
          "line p2: N1 + N2 = 1")
END BLOCK

! Test 3: line, order 3, cellOrient = -1 flips the odd bubble only
BLOCK
  TYPE(ElemShapeData_) :: elemsd1, elemsd2
  TYPE(QuadraturePoint_) :: quad
  REAL(DFP) :: basis(4, 4), grad(4, 4, 1)
  INTEGER(I4B) :: nrow, ncol, dim1, dim2, dim3

  CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                   nips=[4], quadratureType=qpopt%GaussLegendre)
  CALL HierarchicalElemShapeData(obj=elemsd1, quad=quad, nsd=1, xidim=1, &
                                 elemType=Line2, refelemCoord=lineCoord, &
                                 domainName="BIUNIT", cellOrder=[3_I4B], &
                                 cellOrient=[1_I4B])
  CALL HierarchicalElemShapeData(obj=elemsd2, quad=quad, nsd=1, xidim=1, &
                                 elemType=Line2, refelemCoord=lineCoord, &
                                 domainName="BIUNIT", cellOrder=[3_I4B], &
                                 cellOrient=[-1_I4B])
  CALL HeirarchicalBasis_Line_(order=3, xij=quad%points(1:1, 1:4), &
                               refLine="BIUNIT", orient=-1, ans=basis, &
                               nrow=nrow, ncol=ncol)
  CALL HeirarchicalBasisGradient_Line_(order=3, &
                                       xij=quad%points(1:1, 1:4), &
                                       refLine="BIUNIT", orient=-1, &
                                       ans=grad, dim1=dim1, dim2=dim2, &
                                       dim3=dim3)

  CALL OK(elemsd2%nns .EQ. 4, "line p3: nns = 4")
  CALL OK(ALL(ABS(elemsd2%N - TRANSPOSE(basis)) .LT. tol), &
          "line p3, orient -1: same N as the line basis routine")
  CALL OK(ALL(ABS(elemsd2%dNdXi(:, 1, :) - TRANSPOSE(grad(:, :, 1))) &
              .LT. tol), "line p3, orient -1: same dN/dxi")
  CALL OK(ALL(ABS(elemsd2%N(1:3, :) - elemsd1%N(1:3, :)) .LT. tol), &
          "line p3, orient -1: vertex and degree-2 modes unchanged")
  CALL OK(ALL(ABS(elemsd2%N(4, :) + elemsd1%N(4, :)) .LT. tol), &
          "line p3, orient -1: degree-3 mode changes sign")
END BLOCK

! Test 4: quadrangle, order 1, bilinear vertex functions
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  TYPE(QuadraturePoint_) :: quad
  INTEGER(I4B) :: faceOrder(3, 4), faceOrient(3, 4), ii

  faceOrder = 1
  faceOrient = 1
  CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                   nips=[2], quadratureType=qpopt%GaussLegendre)
  CALL HierarchicalElemShapeData(obj=elemsd, quad=quad, nsd=2, xidim=2, &
                         elemType=Quadrangle4, refelemCoord=quadCoord, &
                       domainName="BIUNIT", cellOrder=[1_I4B, 1_I4B], &
                                 faceOrder=faceOrder, &
                                 cellOrient=[1_I4B, 1_I4B, 1_I4B], &
                                 faceOrient=faceOrient)

  CALL OK(elemsd%nns .EQ. 4 .AND. elemsd%nips .EQ. 4, &
          "quadrangle p1: nns = 4, nips = 4")
  CALL OK(ALL(ABS(SUM(elemsd%N, dim=1) - 1.0_DFP) .LT. tol), &
          "quadrangle p1: sum of N = 1")
  DO ii = 1, 2
    CALL OK(ALL(ABS(SUM(elemsd%dNdXi(:, ii, :), dim=1)) .LT. tol), &
            "quadrangle p1: sum of dN/dxi = 0")
  END DO
  CALL OK(ABS(SUM(elemsd%ws) - 4.0_DFP) .LT. tol, &
          "quadrangle p1: sum of ws = 4")
  ! N1 = (1 - xi)(1 - eta) / 4 at the first point
  CALL OK(ABS(elemsd%N(1, 1) - 0.25_DFP * (1.0_DFP - quad%points(1, 1)) &
              * (1.0_DFP - quad%points(2, 1))) .LT. tol, &
          "quadrangle p1: N1 at the first point")
END BLOCK

! Test 5: quadrangle, order 2, 4 vertex + 4 edge + 1 cell functions
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  TYPE(QuadraturePoint_) :: quad
  INTEGER(I4B) :: faceOrder(3, 4), faceOrient(3, 4)

  faceOrder = 2
  faceOrient = 1
  CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                   nips=[3], quadratureType=qpopt%GaussLegendre)
  CALL HierarchicalElemShapeData(obj=elemsd, quad=quad, nsd=2, xidim=2, &
                         elemType=Quadrangle4, refelemCoord=quadCoord, &
                       domainName="BIUNIT", cellOrder=[2_I4B, 2_I4B], &
                                 faceOrder=faceOrder, &
                                 cellOrient=[1_I4B, 1_I4B, 1_I4B], &
                                 faceOrient=faceOrient)

  CALL OK(elemsd%nns .EQ. 9 .AND. elemsd%nips .EQ. 9, &
          "quadrangle p2: nns = 9, nips = 9")
  CALL OK(ALL(ABS(SUM(elemsd%N(1:4, :), dim=1) - 1.0_DFP) .LT. tol), &
          "quadrangle p2: vertex functions sum to 1")
  CALL OK(ALL(ABS(SUM(elemsd%dNdXi(1:4, 1, :), dim=1)) .LT. tol), &
          "quadrangle p2: vertex dN/dxi sum to 0")
END BLOCK

! Test 6: from a reference element, and with the Initiate generic
BLOCK
  TYPE(ElemShapeData_) :: elemsd1, elemsd2, elemsd3
  TYPE(QuadraturePoint_) :: quad
  TYPE(ReferenceLine_) :: refelem

  refelem = ReferenceLine(nsd=1)
  CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                   nips=[3], quadratureType=qpopt%GaussLegendre)
  CALL HierarchicalElemShapeData(obj=elemsd1, quad=quad, nsd=1, xidim=1, &
                                 elemType=Line2, refelemCoord=lineCoord, &
                                 domainName="BIUNIT", cellOrder=[2_I4B], &
                                 cellOrient=[1_I4B])
  CALL HierarchicalElemShapeData(obj=elemsd2, quad=quad, refelem=refelem, &
                                 cellOrder=[2_I4B], cellOrient=[1_I4B])
  CALL Initiate(obj=elemsd3, quad=quad, refelem=refelem, &
                baseContinuity=TypeH1, &
                baseInterpolation=TypeHierarchyInterpolation, &
                cellOrder=[2_I4B], cellOrient=[1_I4B])

  CALL OK(elemsd2%nns .EQ. elemsd1%nns, "refelem: same nns")
  CALL OK(ALL(ABS(elemsd2%N - elemsd1%N) .LT. tol), "refelem: same N")
  CALL OK(ALL(ABS(elemsd2%dNdXi - elemsd1%dNdXi) .LT. tol), &
          "refelem: same dN/dxi")
  CALL OK(ALL(ABS(elemsd3%N - elemsd1%N) .LT. tol), &
          "Initiate H1 hierarchy: same N")
  CALL OK(ALL(ABS(elemsd3%dNdXi - elemsd1%dNdXi) .LT. tol), &
          "Initiate H1 hierarchy: same dN/dxi")
END BLOCK

END PROGRAM main
