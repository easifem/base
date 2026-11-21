!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Deallocate a FEVariable

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE FEVariable_Method, ONLY: DEALLOCATE
USE FEVariable_Method, ONLY: Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: Deallocate resets all components
BLOCK
  TYPE(FEVariable_) :: obj

  CALL Initiate(obj=obj, s=[3, 2], defineon=fevaropt%nodal, &
                vartype=fevaropt%space, rank=fevaropt%vector, len=6)
  CALL DEALLOCATE(obj)

  CALL OK(.NOT. obj%isInit, "isInit is false")
  CALL OK(ALL(obj%s .EQ. 0), "s = 0")
  CALL OK(obj%tshape .EQ. 0, "tshape = 0")
  CALL OK(obj%defineOn .EQ. 0, "defineOn = 0")
  CALL OK(obj%varType .EQ. 0, "varType = 0")
  CALL OK(obj%rank .EQ. 0, "rank = 0")
  CALL OK(obj%len .EQ. 0, "len = 0")
  CALL OK(obj%capacity .EQ. 0, "capacity = 0")
  CALL OK(.NOT. ALLOCATED(obj%val), "val is deallocated")
END BLOCK

! Test 2: Deallocate an uninitiated object is safe
BLOCK
  TYPE(FEVariable_) :: obj

  CALL DEALLOCATE(obj)
  CALL OK(.NOT. obj%isInit, "uninitiated object stays empty")
END BLOCK

END PROGRAM main
