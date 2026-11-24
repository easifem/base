!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing Initiate, Copy and assignment of BoundingBox_

PROGRAM main
USE BaseType, ONLY: BoundingBox_
USE BoundingBox_Method, ONLY: ASSIGNMENT(=)
USE BoundingBox_Method, ONLY: Copy
USE BoundingBox_Method, ONLY: Initiate
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! lim = [xmin, xmax, ymin, ymax, zmin, zmax]
REAL(DFP), PARAMETER :: lim(6) = [0.0_DFP, 1.0_DFP, -1.0_DFP, &
                                  2.0_DFP, 3.0_DFP, 7.0_DFP]

! Test 1: Initiate from nsd and lim
BLOCK
  TYPE(BoundingBox_) :: obj
  CALL Initiate(obj, nsd=3_I4B, lim=lim)
  CALL OK(obj%nsd .EQ. 3, "Initiate(nsd, lim): nsd")
  CALL OK(ALL(ABS(obj%box(1, :) - [0.0_DFP, -1.0_DFP, 3.0_DFP]) &
              .LT. tol), "Initiate(nsd, lim): min corner")
  CALL OK(ALL(ABS(obj%box(2, :) - [1.0_DFP, 2.0_DFP, 7.0_DFP]) &
              .LT. tol), "Initiate(nsd, lim): max corner")
  CALL OK(ALL(ABS(obj%l - [1.0_DFP, 3.0_DFP, 4.0_DFP]) .LT. tol), &
          "Initiate(nsd, lim): lengths")
END BLOCK

! Test 2: Initiate from another box
BLOCK
  TYPE(BoundingBox_) :: obj, obj2
  CALL Initiate(obj, nsd=3_I4B, lim=lim)
  CALL Initiate(obj2, obj)
  CALL OK(isSame(obj, obj2), "Initiate(obj, anotherobj)")
END BLOCK

! Test 3: Copy and assignment of a scalar box
BLOCK
  TYPE(BoundingBox_) :: obj, obj2, obj3
  CALL Initiate(obj, nsd=2_I4B, lim=lim)
  CALL Copy(obj2, obj)
  obj3 = obj
  CALL OK(isSame(obj, obj2), "Copy: scalar")
  CALL OK(isSame(obj, obj3), "ASSIGNMENT(=): scalar")
END BLOCK

! Test 4: Initiate, Copy and assignment of arrays of boxes
BLOCK
  TYPE(BoundingBox_) :: a(2), b(2), c(2), d(2)
  CALL Initiate(a(1), nsd=3_I4B, lim=lim)
  CALL Initiate(a(2), nsd=2_I4B, lim=2.0_DFP * lim)
  CALL Initiate(b, a)
  CALL Copy(c, a)
  d = a
  CALL OK(isSame(a(1), b(1)) .AND. isSame(a(2), b(2)), &
          "Initiate: array")
  CALL OK(isSame(a(1), c(1)) .AND. isSame(a(2), c(2)), "Copy: array")
  CALL OK(isSame(a(1), d(1)) .AND. isSame(a(2), d(2)), &
          "ASSIGNMENT(=): array")
END BLOCK

CONTAINS

PURE FUNCTION isSame(x, y) RESULT(ans)
  TYPE(BoundingBox_), INTENT(IN) :: x, y
  LOGICAL :: ans
  ans = (x%nsd .EQ. y%nsd) .AND. ALL(ABS(x%box - y%box) .LT. tol) &
        .AND. ALL(ABS(x%l - y%l) .LT. tol)
END FUNCTION isSame

END PROGRAM main
