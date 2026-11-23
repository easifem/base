!> author: Vikas Sharma, Ph. D.
! date: 2026-11-23
! summary: MdEncode labels use component names and Not ALLOCATED

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE ElemShapeData_Method, ONLY: MdEncode
USE GlobalData, ONLY: DFP
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: a missing normal is reported as Not ALLOCATED
BLOCK
  TYPE(String) :: ans
  TYPE(ElemShapeData_) :: empty

  ans = MdEncode(empty)
  CALL OK(INDEX(ans%chars(), "**normal Not ALLOCATED**") .GT. 0, &
          "empty: normal Not ALLOCATED")
  CALL OK(INDEX(ans%chars(), "**js Not ALLOCATED**") .GT. 0, &
          "empty: js Not ALLOCATED")
END BLOCK

! Test 2: the js table is titled with the component name js
BLOCK
  TYPE(String) :: ans
  TYPE(ElemShapeData_) :: obj

  obj%nips = 2
  obj%js = [1.0_DFP, 2.0_DFP]
  ans = MdEncode(obj)
  CALL OK(INDEX(ans%chars(), "**js**") .GT. 0, "js table title")
END BLOCK

END PROGRAM main
