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

MODULE PushPopUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: Push
PUBLIC :: Pop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2022
! summary: Push a value

INTERFACE
  MODULE PURE FUNCTION push_int8(vec, pos, value) RESULT(ans)
    INTEGER(Int8), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(Int8), INTENT(IN) :: value
    INTEGER(Int8) :: ans(SIZE(vec) + 1)
  END FUNCTION push_int8
END INTERFACE

INTERFACE Push
  MODULE PROCEDURE push_int8
END INTERFACE Push

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2022
! summary: Push a value

INTERFACE
  MODULE PURE FUNCTION push_int16(vec, pos, value) RESULT(ans)
    INTEGER(Int16), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(Int16), INTENT(IN) :: value
    INTEGER(Int16) :: ans(SIZE(vec) + 1)
  END FUNCTION push_int16
END INTERFACE

INTERFACE Push
  MODULE PROCEDURE push_int16
END INTERFACE Push

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2022
! summary: Push a value

INTERFACE
  MODULE PURE FUNCTION push_int32(vec, pos, value) RESULT(ans)
    INTEGER(Int32), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(Int32), INTENT(IN) :: value
    INTEGER(Int32) :: ans(SIZE(vec) + 1)
  END FUNCTION push_int32
END INTERFACE

INTERFACE Push
  MODULE PROCEDURE push_int32
END INTERFACE Push

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2022
! summary: Push a value

INTERFACE
  MODULE PURE FUNCTION push_int64(vec, pos, value) RESULT(ans)
    INTEGER(Int64), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(Int64), INTENT(IN) :: value
    INTEGER(Int64) :: ans(SIZE(vec) + 1)
  END FUNCTION push_int64
END INTERFACE

INTERFACE Push
  MODULE PROCEDURE push_int64
END INTERFACE Push

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2022
! summary: Push a value

INTERFACE
  MODULE PURE FUNCTION push_real32(vec, pos, value) RESULT(ans)
    REAL(Real32), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    REAL(Real32), INTENT(IN) :: value
    REAL(Real32) :: ans(SIZE(vec) + 1)
  END FUNCTION push_real32
END INTERFACE

INTERFACE Push
  MODULE PROCEDURE push_real32
END INTERFACE Push

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2022
! summary: Push a value

INTERFACE
  MODULE PURE FUNCTION push_real64(vec, pos, value) RESULT(ans)
    REAL(Real64), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    REAL(Real64), INTENT(IN) :: value
    REAL(Real64) :: ans(SIZE(vec) + 1)
  END FUNCTION push_real64
END INTERFACE

INTERFACE Push
  MODULE PROCEDURE push_real64
END INTERFACE Push

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2022
! summary: Pop a value

INTERFACE
  MODULE PURE FUNCTION Pop_int8(vec, pos) RESULT(ans)
    INTEGER(Int8), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(Int8) :: ans(MAX(SIZE(vec) - 1, 0))
  END FUNCTION Pop_int8
END INTERFACE

INTERFACE Pop
  MODULE PROCEDURE Pop_int8
END INTERFACE Pop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2022
! summary: Pop a value

INTERFACE
  MODULE PURE FUNCTION Pop_int16(vec, pos) RESULT(ans)
    INTEGER(Int16), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(Int16) :: ans(MAX(SIZE(vec) - 1, 0))
  END FUNCTION Pop_int16
END INTERFACE

INTERFACE Pop
  MODULE PROCEDURE Pop_int16
END INTERFACE Pop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2022
! summary: Pop a value

INTERFACE
  MODULE PURE FUNCTION Pop_int32(vec, pos) RESULT(ans)
    INTEGER(Int32), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(Int32) :: ans(MAX(SIZE(vec) - 1, 0))
  END FUNCTION Pop_int32
END INTERFACE

INTERFACE Pop
  MODULE PROCEDURE Pop_int32
END INTERFACE Pop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2022
! summary: Pop a value

INTERFACE
  MODULE PURE FUNCTION Pop_int64(vec, pos) RESULT(ans)
    INTEGER(Int64), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    INTEGER(Int64) :: ans(MAX(SIZE(vec) - 1, 0))
  END FUNCTION Pop_int64
END INTERFACE

INTERFACE Pop
  MODULE PROCEDURE Pop_int64
END INTERFACE Pop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2022
! summary: Pop a value

INTERFACE
  MODULE PURE FUNCTION Pop_real32(vec, pos) RESULT(ans)
    REAL(Real32), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    REAL(Real32) :: ans(MAX(SIZE(vec) - 1, 0))
  END FUNCTION Pop_real32
END INTERFACE

INTERFACE Pop
  MODULE PROCEDURE Pop_real32
END INTERFACE Pop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Sept 2022
! summary: Pop a value

INTERFACE
  MODULE PURE FUNCTION Pop_real64(vec, pos) RESULT(ans)
    REAL(Real64), INTENT(IN) :: vec(:)
    INTEGER(I4B), INTENT(IN) :: pos
    REAL(Real64) :: ans(MAX(SIZE(vec) - 1, 0))
  END FUNCTION Pop_real64
END INTERFACE

INTERFACE Pop
  MODULE PROCEDURE Pop_real64
END INTERFACE Pop

END MODULE PushPopUtility
