!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Matrix interpolation of constant and quadrature FEVariables

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: GetInterpolation
USE ElemShapeData_Method, ONLY: GetInterpolation_
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
TYPE(ElemShapeData_) :: elemsd, timeElemsd
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
TYPE(QuadraturePoint_) :: quad
REAL(DFP) :: c(2, 2), q(2, 2, 2), qst(2, 2, 2, 2)
INTEGER(I4B) :: ips, ipt

! linear line in space and in time, 2 Gauss points each
CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)
timeElemsd = elemsd
CALL MakeST(elemsd, timeElemsd, stelemsd)

! constant matrix c, and quadrature values q(:, :, ips)
! and qst(:, :, ips, ipt) with distinct entries
c = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])
DO ips = 1, 2
  q(:, :, ips) = c + 10.0_DFP * ips
  DO ipt = 1, 2
    qst(:, :, ips, ipt) = c + 10.0_DFP * ips + 100.0_DFP * ipt
  END DO
END DO

! Test 1: constant matrix FEVariable, same matrix at every point
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  REAL(DFP) :: ans2(3, 3, 3)
  INTEGER(I4B) :: dim1, dim2, dim3

  CALL GetInterpolation(obj=elemsd, ans=ans, &
                        val=QuadratureVariable(c, Matrix, Constant))
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 2, 2]), "constant: ans(nrow, ncol, nips)")
  CALL OK(ALL(ABS(ans(:, :, 1) - c) .LT. tol) .AND. &
          ALL(ABS(ans(:, :, 2) - c) .LT. tol), "constant: c at each point")

  ans2 = 1.0_DFP
  CALL GetInterpolation_(obj=elemsd, ans=ans2, &
                         val=QuadratureVariable(c, Matrix, Constant), &
                         dim1=dim1, dim2=dim2, dim3=dim3, scale=2.0_DFP, &
                         addContribution=.TRUE.)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 2, 2]), "constant: sizes")
  CALL OK(ALL(ABS(ans2(1:2, 1:2, 2) - (1.0_DFP + 2.0_DFP * c)) .LT. tol), &
          "constant: add 2 c")
END BLOCK

! Test 2: quadrature space matrix FEVariable, values are copied
BLOCK
  REAL(DFP) :: ans(3, 3, 3)
  INTEGER(I4B) :: dim1, dim2, dim3

  CALL GetInterpolation_(obj=elemsd, ans=ans, &
                         val=QuadratureVariable(q, Matrix, Space), &
                         dim1=dim1, dim2=dim2, dim3=dim3)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 2, 2]), "quadrature space: sizes")
  CALL OK(ALL(ABS(ans(1:2, 1:2, 1:2) - q) .LT. tol), &
          "quadrature space: ans = q")
END BLOCK

! Test 3: quadrature space-time matrix FEVariable at all time points
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)

  CALL GetInterpolation(obj=stelemsd, ans=ans, &
                        val=QuadratureVariable(qst, Matrix, SpaceTime))
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 2, 2, 2]), &
          "quadrature space-time: ans(nrow, ncol, nips, nipt)")
  CALL OK(ALL(ABS(ans - qst) .LT. tol), "quadrature space-time: ans = qst")
END BLOCK

! Test 4: quadrature space-time matrix FEVariable, timeIndx selects time
BLOCK
  REAL(DFP) :: ans(3, 3, 3)
  INTEGER(I4B) :: dim1, dim2, dim3

  CALL GetInterpolation_(obj=stelemsd(2), ans=ans, &
                         val=QuadratureVariable(qst, Matrix, SpaceTime), &
                         dim1=dim1, dim2=dim2, dim3=dim3, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=2)
  CALL OK(ALL(ABS(ans(1:2, 1:2, 1:2) - qst(:, :, :, 2)) .LT. tol), &
          "quadrature space-time: timeIndx = 2")
END BLOCK

! Test 5: one quadrature point, constant, quadrature and space-time nodal
BLOCK
  REAL(DFP) :: ans(3, 3), mst(2, 2, 2, 2), ref(2, 2)
  INTEGER(I4B) :: nrow, ncol, ii, aa

  CALL GetInterpolation_(obj=elemsd, ans=ans, &
                         val=QuadratureVariable(c, Matrix, Constant), &
                         nrow=nrow, ncol=ncol, scale=1.0_DFP, &
                         addContribution=.FALSE., spaceIndx=2, timeIndx=1)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "one point constant: nrow, ncol")
  CALL OK(ALL(ABS(ans(1:2, 1:2) - c) .LT. tol), "one point constant: c")

  CALL GetInterpolation_(obj=elemsd, ans=ans, &
                         val=QuadratureVariable(q, Matrix, Space), &
                         nrow=nrow, ncol=ncol, scale=1.0_DFP, &
                         addContribution=.FALSE., spaceIndx=2, timeIndx=1)
  CALL OK(ALL(ABS(ans(1:2, 1:2) - q(:, :, 2)) .LT. tol), &
          "one point quadrature: q(:, :, 2)")

  CALL GetInterpolation_(obj=stelemsd(1), ans=ans, &
                         val=QuadratureVariable(qst, Matrix, SpaceTime), &
                         nrow=nrow, ncol=ncol, scale=1.0_DFP, &
                         addContribution=.FALSE., spaceIndx=1, timeIndx=2)
  CALL OK(ALL(ABS(ans(1:2, 1:2) - qst(:, :, 1, 2)) .LT. tol), &
          "one point quadrature space-time: qst(:, :, 1, 2)")

  ! space-time nodal: ref = sum_a T(a) sum_I N(I, 1) mst(:, :, I, a)
  mst(:, :, :, 1) = q
  mst(:, :, :, 2) = 2.0_DFP * q
  ref = 0.0_DFP
  DO aa = 1, 2
    DO ii = 1, 2
      ref = ref + stelemsd(2)%T(aa) * elemsd%N(ii, 1) * mst(:, :, ii, aa)
    END DO
  END DO
  CALL GetInterpolation_(obj=stelemsd(2), ans=ans, &
                         val=NodalVariable(mst, Matrix, SpaceTime), &
                         nrow=nrow, ncol=ncol, scale=1.0_DFP, &
                         addContribution=.FALSE., spaceIndx=1, timeIndx=2)
  CALL OK(ALL(ABS(ans(1:2, 1:2) - ref) .LT. tol), &
          "one point nodal space-time: sum T N mst")
END BLOCK

CONTAINS

!> Space-time shape data: time data from timeElemsd and the same space
! data from elemsd at every time point
SUBROUTINE MakeST(sd, timeSd, stsd)
  TYPE(ElemShapeData_), INTENT(IN) :: sd
  !! space shape data
  TYPE(ElemShapeData_), INTENT(IN) :: timeSd
  !! time shape data
  TYPE(STElemShapeData_), ALLOCATABLE, INTENT(INOUT) :: stsd(:)
  !! space-time shape data, one per time point
  INTEGER(I4B) :: kk

  CALL Initiate(stsd, timeSd)
  DO kk = 1, SIZE(stsd)
    stsd(kk)%nsd = sd%nsd
    stsd(kk)%xidim = sd%xidim
    stsd(kk)%nns = sd%nns
    stsd(kk)%nips = sd%nips
    stsd(kk)%N = sd%N
    stsd(kk)%dNdXi = sd%dNdXi
    stsd(kk)%ws = sd%ws
  END DO
END SUBROUTINE MakeST

END PROGRAM main
