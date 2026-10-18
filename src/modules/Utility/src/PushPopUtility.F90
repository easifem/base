! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> author: Vikas Sharma, Ph. D.
! date: 2022-09-05
! summary: Insert an entry into a vector or remove one from it.
!
! Description:
!   This module contains two generic functions.
!
!   - `Push(vec, pos, value)` returns a copy of `vec` with `value`
!     inserted at position `pos`, so the result has `SIZE(vec)+1`
!     entries.
!   - `Pop(vec, pos)` returns a copy of `vec` without the entry at
!     position `pos`, so the result has `MAX(SIZE(vec)-1, 0)` entries.
!
!   `vec` is never changed. Both are available for integer vectors
!   (`INT8`, `INT16`, `INT32`, `INT64`) and real vectors (`REAL32`,
!   `REAL64`).

MODULE PushPopUtility
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: INT16
USE GlobalData, ONLY: INT32
USE GlobalData, ONLY: INT64
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
IMPLICIT NONE
PRIVATE

PUBLIC :: Push
PUBLIC :: Pop

!----------------------------------------------------------------------------
!                                                                      Push
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-09-05
! summary: Insert an integer value into an integer vector.
!
! Description:
!   Returns a copy of `vec` with `value` inserted at position `pos`.
!   The entries `vec(pos:)` are shifted one place to the right. It is
!   available for `INT8`, `INT16`, `INT32` and `INT64`.
!
!   - `1 <= pos <= SIZE(vec)`: `ans(pos) = value`. For example
!     `pos = SIZE(vec)` puts `value` before the last entry.
!   - `pos > SIZE(vec)`: `value` is appended at the end.
!   - `pos < 1`: `value` is put at the front.
!
! Arguments:
!   vec : INTEGER, rank(1) - input vector
!   pos : INTEGER(I4B) - position of the new entry in the result
!   value : INTEGER - value to insert, same kind as `vec`
!
! Returns:
!   ans : INTEGER, rank(1) - `SIZE(ans) = SIZE(vec) + 1`
!
! Formula:
!   $$\mathrm{ans} = [\,vec_1, \ldots, vec_{pos-1}, \mathrm{value},
!   vec_{pos}, \ldots, vec_n\,]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE PushPopUtility, ONLY: Push
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: vec(5)
! INTEGER(I4B), ALLOCATABLE :: ans(:)
!
! vec = [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]
!
! ! push at the front
! ans = Push(vec, 1_I4B, 0_I4B)
! CALL OK(ALL(ans .EQ. [0_I4B, 1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
!         "push at pos=1")
!
! ! push in the middle
! ans = Push(vec, 2_I4B, 0_I4B)
! CALL OK(ALL(ans .EQ. [1_I4B, 0_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
!         "push at pos=2")
!
! ! pos = SIZE(vec) puts value before the last entry
! ans = Push(vec, 5_I4B, 0_I4B)
! CALL OK(ALL(ans .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 0_I4B, 5_I4B]), &
!         "push at pos=5")
!
! ! pos = SIZE(vec) + 1 appends
! ans = Push(vec, 6_I4B, 0_I4B)
! CALL OK(ALL(ans .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 0_I4B]), &
!         "push at pos=6")
!
! ! out of range positions
! ans = Push(vec, 100_I4B, 0_I4B)
! CALL OK(ans(6) .EQ. 0_I4B, "pos > SIZE(vec) appends")
! ans = Push(vec, 0_I4B, 9_I4B)
! CALL OK(ans(1) .EQ. 9_I4B .AND. ALL(ans(2:6) .EQ. vec), &
!         "pos < 1 prepends")
!
! ! the result has one more entry
! CALL OK(SIZE(Push(vec, 3_I4B, 0_I4B)) .EQ. 6_I4B, "size of the result")
!
! END PROGRAM main
!```

INTERFACE Push
  MODULE PURE FUNCTION push_int8(vec, pos, value) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(INT8), INTENT(IN) :: value
    INTEGER(INT8) :: ans(SIZE(vec) + 1)
  END FUNCTION push_int8

  MODULE PURE FUNCTION push_int16(vec, pos, value) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(INT16), INTENT(IN) :: value
    INTEGER(INT16) :: ans(SIZE(vec) + 1)
  END FUNCTION push_int16

  MODULE PURE FUNCTION push_int32(vec, pos, value) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(INT32), INTENT(IN) :: value
    INTEGER(INT32) :: ans(SIZE(vec) + 1)
  END FUNCTION push_int32

  MODULE PURE FUNCTION push_int64(vec, pos, value) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(INT64), INTENT(IN) :: value
    INTEGER(INT64) :: ans(SIZE(vec) + 1)
  END FUNCTION push_int64
END INTERFACE Push

!----------------------------------------------------------------------------
!                                                                      Push
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-09-05
! summary: Insert a real value into a real vector.
!
! Description:
!   Returns a copy of `vec` with `value` inserted at position `pos`.
!   The entries `vec(pos:)` are shifted one place to the right. It is
!   available for `REAL32` and `REAL64`.
!
!   - `1 <= pos <= SIZE(vec)`: `ans(pos) = value`.
!   - `pos > SIZE(vec)`: `value` is appended at the end.
!   - `pos < 1`: `value` is put at the front.
!
! Arguments:
!   vec : REAL, rank(1) - input vector
!   pos : INTEGER(I4B) - position of the new entry in the result
!   value : REAL - value to insert, same kind as `vec`
!
! Returns:
!   ans : REAL, rank(1) - `SIZE(ans) = SIZE(vec) + 1`
!
! Formula:
!   $$\mathrm{ans} = [\,vec_1, \ldots, vec_{pos-1}, \mathrm{value},
!   vec_{pos}, \ldots, vec_n\,]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE PushPopUtility, ONLY: Push
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: vec(3)
! REAL(DFP), ALLOCATABLE :: ans(:)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! vec = [1.5_DFP, 2.5_DFP, 3.5_DFP]
!
! ! push at the front
! ans = Push(vec, 1_I4B, -1.0_DFP)
! CALL OK(ALL(ABS(ans - [-1.0_DFP, 1.5_DFP, 2.5_DFP, 3.5_DFP]) .LT. tol), &
!         "push at pos=1")
!
! ! push in the middle
! ans = Push(vec, 2_I4B, -1.0_DFP)
! CALL OK(ALL(ABS(ans - [1.5_DFP, -1.0_DFP, 2.5_DFP, 3.5_DFP]) .LT. tol), &
!         "push at pos=2")
!
! ! pos = SIZE(vec) + 1 appends
! ans = Push(vec, 4_I4B, -1.0_DFP)
! CALL OK(ALL(ABS(ans - [1.5_DFP, 2.5_DFP, 3.5_DFP, -1.0_DFP]) .LT. tol), &
!         "push at pos=4")
!
! ! the result has one more entry
! CALL OK(SIZE(ans) .EQ. 4_I4B, "size of the result")
!
! END PROGRAM main
!```

INTERFACE Push
  MODULE PURE FUNCTION push_real32(vec, pos, value) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    REAL(REAL32), INTENT(IN) :: value
    REAL(REAL32) :: ans(SIZE(vec) + 1)
  END FUNCTION push_real32

  MODULE PURE FUNCTION push_real64(vec, pos, value) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    REAL(REAL64), INTENT(IN) :: value
    REAL(REAL64) :: ans(SIZE(vec) + 1)
  END FUNCTION push_real64
END INTERFACE Push

!----------------------------------------------------------------------------
!                                                                       Pop
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-09-05
! summary: Remove an entry from an integer vector.
!
! Description:
!   Returns a copy of `vec` without the entry `vec(pos)`. The entries
!   `vec(pos+1:)` are shifted one place to the left. It is available
!   for `INT8`, `INT16`, `INT32` and `INT64`.
!
!   - `pos > SIZE(vec)`: the last entry is removed.
!   - `pos < 1`: the first entry is removed.
!   - If `SIZE(vec) <= 1` the result is an empty vector.
!
! Arguments:
!   vec : INTEGER, rank(1) - input vector
!   pos : INTEGER(I4B) - position of the entry to remove
!
! Returns:
!   ans : INTEGER, rank(1) - `SIZE(ans) = MAX(SIZE(vec) - 1, 0)`
!
! Formula:
!   $$\mathrm{ans} = [\,vec_1, \ldots, vec_{pos-1},
!   vec_{pos+1}, \ldots, vec_n\,]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE PushPopUtility, ONLY: Pop
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: vec(5), one(1)
! INTEGER(I4B), ALLOCATABLE :: ans(:)
!
! vec = [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]
!
! ! pop the first entry
! ans = Pop(vec, 1_I4B)
! CALL OK(ALL(ans .EQ. [2_I4B, 3_I4B, 4_I4B, 5_I4B]), "pop at pos=1")
!
! ! pop a middle entry
! ans = Pop(vec, 2_I4B)
! CALL OK(ALL(ans .EQ. [1_I4B, 3_I4B, 4_I4B, 5_I4B]), "pop at pos=2")
!
! ! pop the last entry
! ans = Pop(vec, 5_I4B)
! CALL OK(ALL(ans .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B]), "pop at pos=5")
!
! ! out of range positions
! ans = Pop(vec, 100_I4B)
! CALL OK(ALL(ans .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B]), &
!         "pos > SIZE(vec) pops the last entry")
! ans = Pop(vec, 0_I4B)
! CALL OK(ALL(ans .EQ. [2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
!         "pos < 1 pops the first entry")
!
! ! a vector of size 1 gives an empty vector
! one = [7_I4B]
! ans = Pop(one, 1_I4B)
! CALL OK(SIZE(ans) .EQ. 0_I4B, "pop from a vector of size 1")
!
! END PROGRAM main
!```

INTERFACE Pop
  MODULE PURE FUNCTION Pop_int8(vec, pos) RESULT(ans)
    INTEGER(INT8), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(INT8) :: ans(MAX(SIZE(vec) - 1, 0))
  END FUNCTION Pop_int8

  MODULE PURE FUNCTION Pop_int16(vec, pos) RESULT(ans)
    INTEGER(INT16), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(INT16) :: ans(MAX(SIZE(vec) - 1, 0))
  END FUNCTION Pop_int16

  MODULE PURE FUNCTION Pop_int32(vec, pos) RESULT(ans)
    INTEGER(INT32), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(INT32) :: ans(MAX(SIZE(vec) - 1, 0))
  END FUNCTION Pop_int32

  MODULE PURE FUNCTION Pop_int64(vec, pos) RESULT(ans)
    INTEGER(INT64), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(INT64) :: ans(MAX(SIZE(vec) - 1, 0))
  END FUNCTION Pop_int64
END INTERFACE Pop

!----------------------------------------------------------------------------
!                                                                       Pop
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-09-05
! summary: Remove an entry from a real vector.
!
! Description:
!   Returns a copy of `vec` without the entry `vec(pos)`. The entries
!   `vec(pos+1:)` are shifted one place to the left. It is available
!   for `REAL32` and `REAL64`.
!
!   - `pos > SIZE(vec)`: the last entry is removed.
!   - `pos < 1`: the first entry is removed.
!   - If `SIZE(vec) <= 1` the result is an empty vector.
!
! Arguments:
!   vec : REAL, rank(1) - input vector
!   pos : INTEGER(I4B) - position of the entry to remove
!
! Returns:
!   ans : REAL, rank(1) - `SIZE(ans) = MAX(SIZE(vec) - 1, 0)`
!
! Formula:
!   $$\mathrm{ans} = [\,vec_1, \ldots, vec_{pos-1},
!   vec_{pos+1}, \ldots, vec_n\,]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE PushPopUtility, ONLY: Pop
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: vec(4), one(1)
! REAL(DFP), ALLOCATABLE :: ans(:)
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
!
! vec = [1.5_DFP, 2.5_DFP, 3.5_DFP, 4.5_DFP]
!
! ! pop the first entry
! ans = Pop(vec, 1_I4B)
! CALL OK(ALL(ABS(ans - [2.5_DFP, 3.5_DFP, 4.5_DFP]) .LT. tol), &
!         "pop at pos=1")
!
! ! pop a middle entry
! ans = Pop(vec, 3_I4B)
! CALL OK(ALL(ABS(ans - [1.5_DFP, 2.5_DFP, 4.5_DFP]) .LT. tol), &
!         "pop at pos=3")
!
! ! pop the last entry
! ans = Pop(vec, 4_I4B)
! CALL OK(ALL(ABS(ans - [1.5_DFP, 2.5_DFP, 3.5_DFP]) .LT. tol), &
!         "pop at pos=4")
!
! ! a vector of size 1 gives an empty vector
! one = [7.0_DFP]
! ans = Pop(one, 1_I4B)
! CALL OK(SIZE(ans) .EQ. 0_I4B, "pop from a vector of size 1")
!
! END PROGRAM main
!```

INTERFACE Pop
  MODULE PURE FUNCTION Pop_real32(vec, pos) RESULT(ans)
    REAL(REAL32), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    REAL(REAL32) :: ans(MAX(SIZE(vec) - 1, 0))
  END FUNCTION Pop_real32

  MODULE PURE FUNCTION Pop_real64(vec, pos) RESULT(ans)
    REAL(REAL64), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    REAL(REAL64) :: ans(MAX(SIZE(vec) - 1, 0))
  END FUNCTION Pop_real64
END INTERFACE Pop

END MODULE PushPopUtility
