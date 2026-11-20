!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Interpolation of constant and quadrature FEVariables

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetInterpolation
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: Interpolation
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
REAL(DFP) :: xi(2)

! linear line in space and in time, 2 Gauss points each
CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)
timeElemsd = elemsd
xi = quad%points(1, 1:2)
CALL MakeST(elemsd, timeElemsd, stelemsd)

! Test 1: a constant scalar is repeated at every quadrature point
BLOCK
  TYPE(FEVariable_) :: ans

  CALL GetInterpolation(obj=elemsd, ans=ans, &
                        val=NodalVariable(2.5_DFP, Scalar, Constant))
  CALL OK(ans%defineOn .EQ. fevaropt%quadrature, "constant: quadrature")
  CALL OK(ans%varType .EQ. fevaropt%space, "constant: space")
  CALL OK(ALL(ans%s(1:1) .EQ. [2]), "constant: shape [nips]")
  CALL OK(ALL(ABS(ans%val(1:2) - 2.5_DFP) .LT. tol), &
          "constant: u = 2.5 at each point")
END BLOCK

! Test 2: a constant vector is repeated at every quadrature point
BLOCK
  TYPE(FEVariable_) :: ans

  ans = Interpolation(obj=elemsd, &
                      val=NodalVariable([1.0_DFP, 3.0_DFP], Vector, &
                                        Constant))
  CALL OK(ALL(ans%s(1:2) .EQ. [2, 2]), "constant vector: [ncomp, nips]")
  CALL OK(ALL(ABS(ans%val(1:4) - [1.0_DFP, 3.0_DFP, 1.0_DFP, 3.0_DFP]) &
              .LT. tol), "constant vector: v = (1, 3) at each point")
END BLOCK

! Test 3: a quadrature scalar is copied
BLOCK
  TYPE(FEVariable_) :: ans

  CALL GetInterpolation(obj=elemsd, ans=ans, &
                        val=QuadratureVariable([1.0_DFP, 5.0_DFP], &
                                               Scalar, Space))
  CALL OK(ALL(ABS(ans%val(1:2) - [1.0_DFP, 5.0_DFP]) .LT. tol), &
          "quadrature: values are copied")
END BLOCK

! Test 4: a space variable on space-time data is the same at each time
BLOCK
  TYPE(FEVariable_) :: ans
  INTEGER(I4B) :: ipt

  CALL GetInterpolation(obj=stelemsd, ans=ans, &
                        val=NodalVariable([2.0_DFP, 4.0_DFP], Scalar, Space))
  CALL OK(ans%varType .EQ. fevaropt%spacetime, "space val: spacetime ans")
  CALL OK(ALL(ans%s(1:2) .EQ. [2, 2]), "space val: shape [nips, nipt]")
  DO ipt = 1, 2
    CALL OK(ALL(ABS(ans%val(2 * ipt - 1:2 * ipt) - (3.0_DFP + xi)) &
                .LT. tol), "space val: u = 3 + xi at each time point")
  END DO
END BLOCK

CONTAINS

!> Space-time shape data: time data from timeElemsd and the same space
! data from elemsd at every time point
SUBROUTINE MakeST(elemsd, timeElemsd, stelemsd)
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd
  !! space shape data
  TYPE(ElemShapeData_), INTENT(IN) :: timeElemsd
  !! time shape data
  TYPE(STElemShapeData_), ALLOCATABLE, INTENT(INOUT) :: stelemsd(:)
  !! space-time shape data, one per time point
  INTEGER(I4B) :: ipt

  CALL Initiate(stelemsd, timeElemsd)
  DO ipt = 1, SIZE(stelemsd)
    stelemsd(ipt)%nsd = elemsd%nsd
    stelemsd(ipt)%xidim = elemsd%xidim
    stelemsd(ipt)%nns = elemsd%nns
    stelemsd(ipt)%nips = elemsd%nips
    stelemsd(ipt)%N = elemsd%N
    stelemsd(ipt)%dNdXi = elemsd%dNdXi
    stelemsd(ipt)%ws = elemsd%ws
  END DO
END SUBROUTINE MakeST

END PROGRAM main
