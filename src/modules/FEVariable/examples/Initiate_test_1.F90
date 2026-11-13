!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Initiate a FEVariable with and without values

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE FEVariable_Method, ONLY: Initiate
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: Initiate with values, vector space nodal variable
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP) :: val(6)

  val = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  CALL Initiate(obj=obj, s=[3, 2], defineon=fevaropt%nodal, &
                vartype=fevaropt%space, rank=fevaropt%vector, len=6, &
                val=val)

  CALL OK(obj%isInit, "isInit is true")
  CALL OK(obj%tshape .EQ. 2, "tshape = 2")
  CALL OK(ALL(obj%s(1:2) .EQ. [3, 2]), "s = [3, 2]")
  CALL OK(obj%defineOn .EQ. fevaropt%nodal, "defineOn = nodal")
  CALL OK(obj%varType .EQ. fevaropt%space, "varType = space")
  CALL OK(obj%rank .EQ. fevaropt%vector, "rank = vector")
  CALL OK(obj%len .EQ. 6, "len = 6")
  CALL OK(obj%capacity .GE. 6, "capacity >= len")
  CALL OK(ALL(obj%val(1:6) .EQ. val), "val is copied")
END BLOCK

! Test 2: Initiate without values gives zero data
BLOCK
  TYPE(FEVariable_) :: obj

  CALL Initiate(obj=obj, s=[4], defineon=fevaropt%quadrature, &
                vartype=fevaropt%space, rank=fevaropt%scalar, len=4)

  CALL OK(obj%len .EQ. 4, "len = 4 without val")
  CALL OK(obj%defineOn .EQ. fevaropt%quadrature, "defineOn = quadrature")
  CALL OK(ALL(obj%val(1:4) .EQ. 0.0_DFP), "val is zero without val")
END BLOCK

! Test 3: re-Initiate with a smaller len keeps the allocation
BLOCK
  TYPE(FEVariable_) :: obj

  CALL Initiate(obj=obj, s=[8], defineon=fevaropt%nodal, &
                vartype=fevaropt%space, rank=fevaropt%scalar, len=8, &
                val=SPREAD(1.0_DFP, 1, 8))
  CALL Initiate(obj=obj, s=[2], defineon=fevaropt%nodal, &
                vartype=fevaropt%space, rank=fevaropt%scalar, len=2)

  CALL OK(obj%len .EQ. 2, "len = 2 after re-Initiate")
  CALL OK(obj%capacity .EQ. 8, "capacity = 8 is kept")
  CALL OK(SIZE(obj%val) .EQ. 8, "SIZE(val) = 8 is kept")
  CALL OK(ALL(obj%val .EQ. 0.0_DFP), "val is reset to zero")
END BLOCK

! Test 4: re-Initiate with a larger len grows the allocation
BLOCK
  TYPE(FEVariable_) :: obj

  CALL Initiate(obj=obj, s=[2], defineon=fevaropt%nodal, &
                vartype=fevaropt%space, rank=fevaropt%scalar, len=2)
  CALL Initiate(obj=obj, s=[3, 3], defineon=fevaropt%nodal, &
                vartype=fevaropt%constant, rank=fevaropt%matrix, len=9, &
                val=SPREAD(2.0_DFP, 1, 9))

  CALL OK(obj%len .EQ. 9, "len = 9 after growing")
  CALL OK(SIZE(obj%val) .GE. 9, "SIZE(val) >= 9 after growing")
  CALL OK(ALL(obj%val(1:9) .EQ. 2.0_DFP), "val after growing")
END BLOCK

END PROGRAM main
