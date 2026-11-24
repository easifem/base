!> author: OpenAI
! date: 2026-11-21
! summary: Coordinate interpolation clips to the shared node count

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE ElemShapeData_Method, ONLY: SetBarycentricCoord
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: coords(2, 4) = RESHAPE( &
                        [-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, &
                         1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
REAL(DFP) :: extended(2, 8), expected(2, 4)

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=coords, &
                           domainName="BIUNIT", order=1)
extended(:, 1:4) = coords
extended(:, 5:8) = 1000.0_DFP
expected = MATMUL(coords, cell%N)

! Test 1: extra coordinates must not index beyond N
BLOCK
  CALL SetBarycentricCoord(obj=cell, val=extended, N=cell%N)
  CALL OK(ALL(ABS(cell%coord - expected) .LT. tol), &
          "extra coordinates: use only rows available in N")
  CALL SetBarycentricCoord(obj=cell, val=coords, N=cell%N)
  CALL OK(ALL(ABS(cell%coord - expected) .LT. tol), &
          "matching nodes: unchanged interpolation")
END BLOCK

! Test 2: both sides can limit the shared node count
BLOCK
  REAL(DFP) :: fewer(2, 2), empty(2, 0)

  fewer = coords(:, 1:2)
  CALL SetBarycentricCoord(obj=cell, val=fewer, N=cell%N)
  CALL OK(ALL(ABS(cell%coord - MATMUL(fewer, cell%N(1:2, :))) &
              .LT. tol), "fewer coordinates: use all available nodes")
  CALL SetBarycentricCoord(obj=cell, val=extended, N=cell%N(1:2, :))
  CALL OK(ALL(ABS(cell%coord - MATMUL(fewer, cell%N(1:2, :))) &
              .LT. tol), "fewer shape rows: ignore remaining coordinates")
  CALL SetBarycentricCoord(obj=cell, val=empty, N=cell%N)
  CALL OK(ALL(cell%coord .EQ. 0.0_DFP), "zero nodes: zero coordinates")
END BLOCK

! Test 3: Set uses the same physical geometry despite extra coordinates
BLOCK
  REAL(DFP) :: jacobian(2, 2, 4)

  CALL Set(obj=cell, val=coords, N=cell%N, dNdXi=cell%dNdXi)
  jacobian = cell%jacobian
  CALL Set(obj=cell, val=extended, N=cell%N, dNdXi=cell%dNdXi)
  CALL OK(ALL(ABS(cell%coord - expected) .LT. tol), &
          "Set: coordinate interpolation ignores extra nodes")
  CALL OK(ALL(ABS(cell%jacobian - jacobian) .LT. tol), &
          "Set: Jacobian still uses the original geometry")
END BLOCK

! Test 4: time interpolation delegates to the clipped spatial routine
BLOCK
  TYPE(STElemShapeData_) :: st
  REAL(DFP) :: spaceTime(2, 8, 2)

  CALL ALLOCATE (st, nsd=2, xidim=2, nns=4, nips=4, nnt=2)
  st%N = cell%N
  spaceTime(:, :, 1) = extended
  spaceTime(:, :, 2) = 2.0_DFP * extended
  CALL SetBarycentricCoord(obj=st, val=spaceTime, N=st%N, &
                           T=[0.25_DFP, 0.75_DFP])
  CALL OK(ALL(ABS(st%coord - 1.75_DFP * expected) .LT. tol), &
          "space-time: ignore extra spatial nodes after time interpolation")
END BLOCK
END PROGRAM main
