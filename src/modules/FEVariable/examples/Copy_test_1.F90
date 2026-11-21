!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Copy a FEVariable by Copy and by assignment

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: Copy
USE FEVariable_Method, ONLY: Initiate
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(FEVariable_) :: obj2
REAL(DFP) :: val(6)

val = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
CALL Initiate(obj=obj2, s=[3, 2], defineon=fevaropt%quadrature, &
              vartype=fevaropt%space, rank=fevaropt%vector, len=6, val=val)

! Test 1: Copy into an empty object
BLOCK
  TYPE(FEVariable_) :: obj1

  CALL Copy(obj1, obj2)

  CALL OK(obj1%isInit, "isInit is copied")
  CALL OK(obj1%tshape .EQ. 2, "tshape is copied")
  CALL OK(ALL(obj1%s .EQ. obj2%s), "s is copied")
  CALL OK(obj1%defineOn .EQ. fevaropt%quadrature, "defineOn is copied")
  CALL OK(obj1%varType .EQ. fevaropt%space, "varType is copied")
  CALL OK(obj1%rank .EQ. fevaropt%vector, "rank is copied")
  CALL OK(obj1%len .EQ. 6, "len is copied")
  CALL OK(ALL(obj1%val(1:6) .EQ. val), "val is copied")
END BLOCK

! Test 2: assignment into an object with larger capacity
BLOCK
  TYPE(FEVariable_) :: obj1

  CALL Initiate(obj=obj1, s=[10], defineon=fevaropt%nodal, &
                vartype=fevaropt%space, rank=fevaropt%scalar, len=10)
  obj1 = obj2

  CALL OK(obj1%len .EQ. 6, "len = 6 after assignment")
  CALL OK(obj1%capacity .EQ. 10, "capacity = 10 is kept")
  CALL OK(obj1%rank .EQ. fevaropt%vector, "rank after assignment")
  CALL OK(ALL(obj1%val(1:6) .EQ. val), "val after assignment")
END BLOCK

! Test 3: copy of an uninitiated object
BLOCK
  TYPE(FEVariable_) :: obj1, obj3

  obj1 = obj3
  CALL OK(.NOT. obj1%isInit, "uninitiated copy has isInit false")
  CALL OK(obj1%len .EQ. 0, "uninitiated copy has len 0")
END BLOCK

END PROGRAM main
