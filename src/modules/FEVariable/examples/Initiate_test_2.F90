!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Initiate a FEVariable, edge cases

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE FEVariable_Method, ONLY: Initiate
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: only val(1:len) is copied when val is longer than len
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val(5)

  val = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP]
  CALL Initiate(obj=obj, s=[3], defineon=fevaropt%nodal, &
                vartype=fevaropt%space, rank=fevaropt%scalar, len=3, &
                val=val)

  CALL OK(obj%len .EQ. 3, "len = 3")
  CALL OK(ALL(obj%val(1:3) .EQ. val(1:3)), "val(1:3) is copied")
END BLOCK

! Test 2: Initiate with len = 0
BLOCK
  TYPE(FEVariable_) :: obj

  CALL Initiate(obj=obj, s=[0], defineon=fevaropt%quadrature, &
                vartype=fevaropt%constant, rank=fevaropt%scalar, len=0)

  CALL OK(obj%isInit, "isInit is true for len = 0")
  CALL OK(obj%len .EQ. 0, "len = 0")
  CALL OK(ALLOCATED(obj%val), "val is allocated for len = 0")
  CALL OK(SIZE(obj%val) .EQ. 0, "SIZE(val) = 0")
END BLOCK

! Test 3: re-Initiate with fewer dimensions
! s(tshape+1:) should be zero, as for a new object
BLOCK
  TYPE(FEVariable_) :: obj

  CALL Initiate(obj=obj, s=[3, 3], defineon=fevaropt%nodal, &
                vartype=fevaropt%space, rank=fevaropt%matrix, len=9)
  CALL Initiate(obj=obj, s=[4], defineon=fevaropt%nodal, &
                vartype=fevaropt%space, rank=fevaropt%scalar, len=4)

  CALL OK(obj%tshape .EQ. 1, "tshape = 1 after re-Initiate")
  CALL OK(obj%s(1) .EQ. 4, "s(1) = 4 after re-Initiate")
  CALL OK(ALL(obj%s(2:) .EQ. 0), "s(2:) = 0 after re-Initiate")
END BLOCK

END PROGRAM main
