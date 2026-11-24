!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Set the length of a FEVariable

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE FEVariable_Method, ONLY: Initiate
USE FEVariable_Method, ONLY: SetLen
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: SetLen to a smaller length keeps capacity, zeroes data
BLOCK
  TYPE(FEVariable_) :: obj

  CALL Initiate(obj=obj, s=[6], defineon=fevaropt%nodal, &
                vartype=fevaropt%space, rank=fevaropt%scalar, len=6, &
                val=SPREAD(1.0_DFP, 1, 6))
  CALL SetLen(obj=obj, tsize=3)

  CALL OK(obj%len .EQ. 3, "len = 3")
  CALL OK(obj%capacity .EQ. 6, "capacity = 6 is kept")
  CALL OK(ALL(obj%val .EQ. 0.0_DFP), "val is zero")
END BLOCK

! Test 2: SetLen to a larger length grows the allocation
BLOCK
  TYPE(FEVariable_) :: obj

  CALL Initiate(obj=obj, s=[2], defineon=fevaropt%nodal, &
                vartype=fevaropt%space, rank=fevaropt%scalar, len=2)
  CALL SetLen(obj=obj, tsize=10)

  CALL OK(obj%len .EQ. 10, "len = 10")
  CALL OK(obj%capacity .EQ. 10, "capacity = 10")
  CALL OK(SIZE(obj%val) .EQ. 10, "SIZE(val) = 10")
END BLOCK

! Test 3: SetLen does nothing on an uninitiated object
BLOCK
  TYPE(FEVariable_) :: obj

  CALL SetLen(obj=obj, tsize=5)

  CALL OK(obj%len .EQ. 0, "len = 0 when not initiated")
  CALL OK(.NOT. ALLOCATED(obj%val), "val not allocated")
END BLOCK

END PROGRAM main
