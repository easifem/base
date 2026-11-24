!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: HRQI length scale with no quadrature points

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE ElemShapeData_Method, ONLY: GetHRQIParam
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: shape data with nips = 0 gives empty outputs and no error
BLOCK
  TYPE(ElemShapeData_) :: cell
  REAL(DFP), ALLOCATABLE :: h(:), hmax(:), hmin(:), r(:, :)
  REAL(DFP) :: p(4)

  cell%nsd = 2
  cell%xidim = 2
  cell%nns = 4
  cell%nips = 0
  ALLOCATE (cell%N(4, 0), cell%dNdXi(4, 2, 0), cell%dNdXt(4, 2, 0))
  ALLOCATE (cell%jacobian(2, 2, 0), cell%js(0), cell%ws(0))
  p = [0.0_DFP, 1.0_DFP, 2.0_DFP, 3.0_DFP]

  CALL GetHRQIParam(obj=cell, h=h, val=NodalVariable(p, Scalar, Space), &
                    hmax=hmax, hmin=hmin, r=r)
  CALL OK(SIZE(h) .EQ. 0, "nips = 0: SIZE(h) = 0")
  CALL OK(SIZE(hmax) .EQ. 0 .AND. SIZE(hmin) .EQ. 0, &
          "nips = 0: SIZE(hmax) = SIZE(hmin) = 0")
END BLOCK

END PROGRAM main
