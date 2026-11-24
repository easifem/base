!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Copy a FEVariable into an object with too small capacity

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: DEALLOCATE
USE FEVariable_Method, ONLY: Initiate
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(FEVariable_) :: obj2
REAL(DFP) :: val(6)

val = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
CALL Initiate(obj=obj2, s=[3, 2], defineon=fevaropt%nodal, &
              vartype=fevaropt%space, rank=fevaropt%vector, len=6, val=val)

! Test 1: assignment into an object with smaller capacity grows it
BLOCK
  TYPE(FEVariable_) :: obj1
  INTEGER(I4B) :: cap

  CALL Initiate(obj=obj1, s=[2], defineon=fevaropt%nodal, &
                vartype=fevaropt%space, rank=fevaropt%scalar, len=2)
  obj1 = obj2
  cap = fevaropt%capacityExpandFactor * 6

  CALL OK(obj1%len .EQ. 6, "len = 6 after growing")
  CALL OK(obj1%capacity .EQ. cap, "capacity = expandFactor * len")
  CALL OK(SIZE(obj1%val) .EQ. cap, "SIZE(val) = capacity")
  CALL OK(ALL(obj1%val(1:6) .EQ. val), "val after growing")
  CALL OK(obj1%tshape .EQ. 2, "tshape after growing")
  CALL OK(ALL(obj1%s(1:2) .EQ. [3, 2]), "s after growing")
END BLOCK

! Test 2: the copy does not share data with the source
BLOCK
  TYPE(FEVariable_) :: obj1, obj3

  obj3 = obj2
  obj1 = obj3
  obj3%val(1) = -1.0_DFP

  CALL OK(obj1%val(1) .EQ. 1.0_DFP, "copy is independent of source")
END BLOCK

! Test 3: copy into a deallocated object
BLOCK
  TYPE(FEVariable_) :: obj1

  obj1 = obj2
  CALL DEALLOCATE(obj1)
  obj1 = obj2

  CALL OK(obj1%isInit, "isInit after copy into deallocated")
  CALL OK(obj1%len .EQ. 6, "len after copy into deallocated")
  CALL OK(ALL(obj1%val(1:6) .EQ. val), "val after copy into deallocated")
END BLOCK

CALL DEALLOCATE(obj2)

END PROGRAM main
