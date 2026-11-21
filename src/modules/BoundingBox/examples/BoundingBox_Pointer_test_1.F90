!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing BoundingBox_Pointer

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: BoundingBox_Pointer
USE BoundingBox_Method, ONLY: Initiate
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! lim = [xmin, xmax, ymin, ymax, zmin, zmax]
REAL(DFP), PARAMETER :: lim(6) = [0.0_DFP, 1.0_DFP, 0.0_DFP, &
                                  2.0_DFP, 0.0_DFP, 3.0_DFP]

! Test 1: BoundingBox_Pointer(nsd, lim) allocates a new box
BLOCK
  TYPE(BoundingBox_), POINTER :: ptr
  ptr => BoundingBox_Pointer(nsd=3_I4B, lim=lim)
  CALL OK(ASSOCIATED(ptr), "BoundingBox_Pointer(nsd, lim): associated")
  CALL OK(ptr%nsd .EQ. 3, "BoundingBox_Pointer(nsd, lim): nsd")
  CALL OK(ALL(ABS(ptr%box(2, :) - [1.0_DFP, 2.0_DFP, 3.0_DFP]) &
              .LT. tol), "BoundingBox_Pointer(nsd, lim): max corner")
  DEALLOCATE (ptr)
END BLOCK

! Test 2: BoundingBox_Pointer(anotherobj) returns a copy
BLOCK
  TYPE(BoundingBox_) :: obj
  TYPE(BoundingBox_), POINTER :: ptr
  CALL Initiate(obj, nsd=2_I4B, lim=lim)
  ptr => BoundingBox_Pointer(obj)
  CALL OK(ASSOCIATED(ptr), "BoundingBox_Pointer(anotherobj): associated")
  CALL OK(ptr%nsd .EQ. 2, "BoundingBox_Pointer(anotherobj): nsd")
  CALL OK(ALL(ABS(ptr%box - obj%box) .LT. tol) .AND. &
          ALL(ABS(ptr%l - obj%l) .LT. tol), &
          "BoundingBox_Pointer(anotherobj): box and lengths")
  DEALLOCATE (ptr)
END BLOCK

END PROGRAM main
