!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing Append for vectors of BoundingBox_

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: Append
USE BoundingBox_Method, ONLY: BoundingBox
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(BoundingBox_) :: b1, b2, b3

b1 = BoundingBox(nsd=1_I4B, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                 0.0_DFP, 0.0_DFP, 0.0_DFP])
b2 = BoundingBox(nsd=2_I4B, lim=[0.0_DFP, 2.0_DFP, 0.0_DFP, &
                                 2.0_DFP, 0.0_DFP, 0.0_DFP])
b3 = BoundingBox(nsd=3_I4B, lim=[0.0_DFP, 3.0_DFP, 0.0_DFP, &
                                 3.0_DFP, 0.0_DFP, 3.0_DFP])

! Test 1: Append to an unallocated vector
BLOCK
  TYPE(BoundingBox_), ALLOCATABLE :: obj(:)
  CALL Append(obj, [b1])
  CALL OK(ALLOCATED(obj), "Append: new vector is allocated")
  CALL OK(SIZE(obj) .EQ. 1, "Append: new vector, size")
  CALL OK(obj(1)%nsd .EQ. 1 .AND. &
          ABS(obj(1)%box(2, 1) - 1.0_DFP) .LT. tol, &
          "Append: new vector, value")
END BLOCK

! Test 2: Append to a vector that has values
BLOCK
  TYPE(BoundingBox_), ALLOCATABLE :: obj(:)
  CALL Append(obj, [b1])
  CALL Append(obj, [b2, b3])
  CALL OK(SIZE(obj) .EQ. 3, "Append: existing vector, size")
  CALL OK(ALL(obj(:)%nsd .EQ. [1, 2, 3]), &
          "Append: existing vector, order kept")
  CALL OK(ABS(obj(3)%box(2, 3) - 3.0_DFP) .LT. tol .AND. &
          ABS(obj(3)%l(3) - 3.0_DFP) .LT. tol, &
          "Append: existing vector, last value")
END BLOCK

END PROGRAM main
