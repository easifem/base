!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Copy ElemShapeData_ and initiate STElemShapeData_ in time

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE ElemShapeData_Method, ONLY: ASSIGNMENT(=)
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
TYPE(ElemShapeData_) :: elemsd
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)

! Test 1: Initiate(obj1, obj2) and obj1 = obj2 copy the data
BLOCK
  TYPE(ElemShapeData_) :: obj1, obj2

  CALL Initiate(obj1, elemsd)
  CALL OK(obj1%nns .EQ. 2 .AND. obj1%nips .EQ. 2, "Initiate: sizes")
  CALL OK(ALL(ABS(obj1%N - elemsd%N) .LT. tol), "Initiate: N")
  CALL OK(ALL(ABS(obj1%dNdXi - elemsd%dNdXi) .LT. tol), "Initiate: dNdXi")
  CALL OK(ALL(ABS(obj1%ws - elemsd%ws) .LT. tol), "Initiate: ws")

  obj2 = elemsd
  CALL OK(ALL(ABS(obj2%N - elemsd%N) .LT. tol), "assignment: N")
END BLOCK

! Test 2: STElemShapeData_ at each time point from a time shape data
BLOCK
  TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
  INTEGER(I4B) :: ip

  CALL Initiate(stelemsd, elemsd)
  CALL OK(SIZE(stelemsd) .EQ. 2, "one STElemShapeData_ per time point")
  DO ip = 1, 2
    CALL OK(stelemsd(ip)%nnt .EQ. 2, "nnt = nns of time shape data")
    CALL OK(ALL(ABS(stelemsd(ip)%T - elemsd%N(:, ip)) .LT. tol), &
            "T = N at the time point")
    CALL OK(ALL(ABS(stelemsd(ip)%dTdTheta - elemsd%dNdXi(:, 1, ip)) &
                .LT. tol), "dTdTheta = dN/dxi at the time point")
    CALL OK(ABS(stelemsd(ip)%wt - elemsd%ws(ip)) .LT. tol, &
            "wt = ws at the time point")
  END DO
END BLOCK

END PROGRAM main
