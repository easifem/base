! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

! #define _OP_ *

MODULE _MODULE_NAME_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: varopt => TypeFEVariableOpt
USE GlobalData, ONLY: I4B, DFP, LGT

PRIVATE

PUBLIC :: SCALAR_VECTOR_MASTER
PUBLIC :: GetRankCase

CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetRankCase
!----------------------------------------------------------------------------

PURE FUNCTION GetRankCase(rank1, rank2) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: rank1, rank2
  INTEGER(I4B) :: ans

  INTEGER(I4B) :: a, b

  a = 0
  b = 0

  SELECT CASE (rank1)
  CASE (varopt%scalar)
    a = 0
  CASE (varopt%vector)
    a = 1
  CASE (varopt%matrix)
    a = 2
  END SELECT

  SELECT CASE (rank2)
  CASE (varopt%scalar)
    b = 0
  CASE (varopt%vector)
    b = 1
  CASE (varopt%matrix)
    b = 2
  END SELECT

  ans = a * 10 + b
END FUNCTION GetRankCase

!----------------------------------------------------------------------------
!                                                                  GetVarCase
!----------------------------------------------------------------------------

PURE FUNCTION GetVarCase(vartype1, vartype2) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: vartype1, vartype2
  INTEGER(I4B) :: ans

  INTEGER(I4B) :: a, b

  a = 0
  b = 0

  SELECT CASE (vartype1)
  CASE (varopt%constant)
    a = 0
  CASE (varopt%space)
    a = 1
  CASE (varopt%time)
    a = 2
  CASE (varopt%spacetime)
    a = 3
  END SELECT

  SELECT CASE (vartype2)
  CASE (varopt%constant)
    b = 0
  CASE (varopt%space)
    b = 1
  CASE (varopt%time)
    b = 2
  CASE (varopt%spacetime)
    b = 3
  END SELECT

  ans = a * 10 + b
END FUNCTION GetVarCase

!----------------------------------------------------------------------------
!                                                                      master
!----------------------------------------------------------------------------

PURE SUBROUTINE scalar_vector_master(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: varCase

  varCase = GetVarCase(obj1%vartype, obj2%vartype)

  SELECT CASE (varCase)
  CASE (00)
    CALL constant_constant(obj1, obj2, ans)
  CASE (01)
    CALL constant_space(obj1, obj2, ans)
  CASE (02)
    CALL constant_time(obj1, obj2, ans)
  CASE (03)
    CALL constant_spacetime(obj1, obj2, ans)
  CASE (10)
    CALL space_constant(obj1, obj2, ans)
  CASE (11)
    CALL space_space(obj1, obj2, ans)
  CASE (12)
    CALL space_time(obj1, obj2, ans)
  CASE (13)
    CALL space_spacetime(obj1, obj2, ans)
  CASE (20)
    CALL time_constant(obj1, obj2, ans)
  CASE (21)
    CALL time_time(obj1, obj2, ans)
  CASE (22)
    CALL time_space(obj1, obj2, ans)
  CASE (23)
    CALL time_spacetime(obj1, obj2, ans)
  CASE (30)
    CALL spacetime_constant(obj1, obj2, ans)
  CASE (31)
    CALL spacetime_space(obj1, obj2, ans)
  CASE (32)
    CALL spacetime_time(obj1, obj2, ans)
  CASE (33)
    CALL spacetime_spacetime(obj1, obj2, ans)
  END SELECT
END SUBROUTINE scalar_vector_master

!----------------------------------------------------------------------------
!                                                          constant constant
!----------------------------------------------------------------------------

! Result will be a constant vector
PURE SUBROUTINE constant_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj2%len
  ans%s(1) = obj2%s(1)
  ans%val(1:ans%len) = obj1%val(1) _OP_ obj2%val(1:ans%len)
END SUBROUTINE constant_constant

!----------------------------------------------------------------------------
!                                                              constant space
!----------------------------------------------------------------------------

! ans will be a space vector
PURE SUBROUTINE constant_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj2%len
  ans%s(1:2) = obj2%s(1:2)
  ans%val(1:ans%len) = obj1%val(1) _OP_ obj2%val(1:ans%len)
END SUBROUTINE constant_space

!----------------------------------------------------------------------------
!                                                              constant time
!----------------------------------------------------------------------------

PURE SUBROUTINE constant_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj2%len
  ans%s(1:2) = obj2%s(1:2)
  ans%val(1:ans%len) = obj1%val(1) _OP_ obj2%val(1:ans%len)
END SUBROUTINE constant_time

!----------------------------------------------------------------------------
!                                                          constant spacetime
!----------------------------------------------------------------------------

PURE SUBROUTINE constant_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj2%len
  ans%s(1:3) = obj2%s(1:3)
  ans%val(1:ans%len) = obj1%val(1) _OP_ obj2%val(1:ans%len)
END SUBROUTINE constant_spacetime

!----------------------------------------------------------------------------
!                                                              space constant
!----------------------------------------------------------------------------

! ans will be a space vector
PURE SUBROUTINE space_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, nsd, np

  nsd = obj2%s(1)
  np = obj1%s(1)

  ans%s(1) = nsd ! take space compo from obj2
  ans%s(2) = np ! take number of points from obj1
  ans%len = nsd * np

  DO CONCURRENT(ii=1:nsd, jj=1:np)
    ans%val(ii + (jj - 1) * nsd) = obj1%val(jj) _OP_ obj2%val(ii)
  END DO
END SUBROUTINE space_constant

!----------------------------------------------------------------------------
!                                                                space space
!----------------------------------------------------------------------------

! ans will be a space vector
PURE SUBROUTINE space_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, nsd, np

  nsd = obj2%s(1)
  np = MIN(obj1%s(1), obj2%s(2))

  ans%s(1) = nsd ! take space compo from obj2
  ans%s(2) = np ! take number of points from obj1
  ans%len = nsd * np

  DO CONCURRENT(ii=1:nsd, jj=1:np)
    ans%val(ii + (jj - 1) * nsd) = obj1%val(jj) _OP_ &
                                   obj2%val(ii + (jj - 1) * nsd)
  END DO
END SUBROUTINE space_space

!----------------------------------------------------------------------------
!                                                                space time
!----------------------------------------------------------------------------

! ans will be a space time vector
PURE SUBROUTINE space_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, kk, nsd, np, nnt

  nsd = obj2%s(1)
  nnt = obj2%s(2)
  np = obj1%s(1)

  ans%s(1) = nsd ! take space compo from obj2
  ans%s(2) = np ! take number of points in space from obj1
  ans%s(3) = nnt ! take number of points  in time from obj2

  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ans%val(ii + (jj - 1) * nsd + (kk - 1) * nsd * np) = &
      obj1%val(jj) _OP_ obj2%val(ii + (kk - 1) * nsd)
  END DO

END SUBROUTINE space_time

!----------------------------------------------------------------------------
!                                                                space time
!----------------------------------------------------------------------------

! ans will be a space time vector
PURE SUBROUTINE space_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, kk, nsd, np, nnt

  nsd = obj2%s(1)
  nnt = obj2%s(3)
  np = MIN(obj1%s(1), obj2%s(2))

  ans%s(1) = nsd ! take space compo from obj2
  ans%s(2) = np ! take number of points in space from obj1
  ans%s(3) = nnt ! take number of points  in time from obj2

  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ans%val(ii + (jj - 1) * nsd + (kk - 1) * nsd * np) = &
      obj1%val(jj) _OP_ obj2%val(ii + (jj - 1) * nsd + (kk - 1) * nsd * np)
  END DO

END SUBROUTINE space_spacetime

!----------------------------------------------------------------------------
!                                                              time constant
!----------------------------------------------------------------------------

! ans will be a time vector
PURE SUBROUTINE time_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ! internal variables
  INTEGER(I4B) :: jj, nsd, np

  nsd = obj2%s(1)
  np = obj1%s(1)

  ans%s(1) = nsd ! take space compo from obj2
  ans%s(2) = np ! take number of points from obj1
  ans%len = nsd * np

  DO CONCURRENT(jj=1:np)
    ans%val((jj - 1) * nsd + 1:jj * nsd) = obj1%val(jj) _OP_ obj2%val(1:nsd)
  END DO
END SUBROUTINE time_constant

!----------------------------------------------------------------------------
!                                                                time time
!----------------------------------------------------------------------------

! ans will be a time vector
PURE SUBROUTINE time_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: jj, nsd, np

  nsd = obj2%s(1)
  np = MIN(obj1%s(1), obj2%s(2))

  ans%s(1) = nsd ! take space compo from obj2
  ans%s(2) = np ! take number of points from obj1
  ans%len = nsd * np

  DO CONCURRENT(jj=1:np)
    ans%val((jj - 1) * nsd + 1:jj * nsd) = obj1%val(jj) _OP_ &
                                         obj2%val((jj - 1) * nsd + 1:jj * nsd)
  END DO
END SUBROUTINE time_time

!----------------------------------------------------------------------------
!                                                                time space
!----------------------------------------------------------------------------

! ans will be a space time vector
PURE SUBROUTINE time_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, kk, nsd, np, nnt

  nsd = obj2%s(1)
  np = obj2%s(2)
  nnt = obj1%s(1)

  ans%s(1) = nsd ! take space compo from obj2
  ans%s(2) = np ! take number of points in space from obj1
  ans%s(3) = nnt ! take number of points  in time from obj2

  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ans%val(ii + (jj - 1) * nsd + (kk - 1) * nsd * np) = &
      obj1%val(kk) _OP_ obj2%val(ii + (jj - 1) * nsd)
  END DO
END SUBROUTINE time_space

!----------------------------------------------------------------------------
!                                                                time space
!----------------------------------------------------------------------------

! ans will be a space time vector
PURE SUBROUTINE time_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, kk, nsd, np, nnt

  nsd = obj2%s(1)
  np = obj2%s(2)
  nnt = MIN(obj1%s(1), obj2%s(3))

  ans%s(1) = nsd ! take space compo from obj2
  ans%s(2) = np ! take number of points in space from obj1
  ans%s(3) = nnt ! take number of points  in time from obj2

  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ans%val(ii + (jj - 1) * nsd + (kk - 1) * nsd * np) = &
      obj1%val(kk) _OP_ obj2%val(ii + (jj - 1) * nsd + (kk - 1) * nsd * np)
  END DO

END SUBROUTINE time_spacetime

!----------------------------------------------------------------------------
!                                                          spacetime constant
!----------------------------------------------------------------------------

! result will be a spacetime vector
PURE SUBROUTINE spacetime_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: nsd, np, nnt

  nsd = obj2%s(1)
  np = obj1%s(1)
  nnt = obj1%s(2)

  ans%s(1) = nsd
  ans%s(2) = np
  ans%s(3) = nnt

  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ans%val(ii + (jj - 1) * nsd + (kk - 1) * nsd * np) = &
      obj1%val(jj + (kk - 1) * np) _OP_ obj2%val(ii)
  END DO

END SUBROUTINE spacetime_constant

!----------------------------------------------------------------------------
!                                                          spacetime space
!----------------------------------------------------------------------------

! result will be a spacetime vector
PURE SUBROUTINE spacetime_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: nsd, np, nnt

  nsd = obj2%s(1)
  np = MIN(obj1%s(1), obj2%s(2))
  nnt = obj1%s(2)

  ans%s(1) = nsd
  ans%s(2) = np
  ans%s(3) = nnt

  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ans%val(ii + (jj - 1) * nsd + (kk - 1) * nsd * np) = &
      obj1%val(jj + (kk - 1) * np) _OP_ obj2%val(ii + (jj - 1) * nsd)
  END DO

END SUBROUTINE spacetime_space

!----------------------------------------------------------------------------
!                                                          spacetime time
!----------------------------------------------------------------------------

! result will be a spacetime vector
PURE SUBROUTINE spacetime_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: nsd, np, nnt

  nsd = obj2%s(1)
  np = obj1%s(1)
  nnt = MIN(obj1%s(2), obj2%s(2))

  ans%s(1) = nsd
  ans%s(2) = np
  ans%s(3) = nnt

  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ans%val(ii + (jj - 1) * nsd + (kk - 1) * nsd * np) = &
      obj1%val(jj + (kk - 1) * np) _OP_ obj2%val(ii + (kk - 1) * nsd)
  END DO

END SUBROUTINE spacetime_time

!----------------------------------------------------------------------------
!                                                        spacetime spacetime
!----------------------------------------------------------------------------

! result will be a spacetime vector
PURE SUBROUTINE spacetime_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: nsd, np, nnt

  nsd = obj2%s(1)
  np = MIN(obj1%s(1), obj2%s(2))
  nnt = MIN(obj1%s(2), obj2%s(3))

  ans%s(1) = nsd
  ans%s(2) = np
  ans%s(3) = nnt

  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ans%val(ii + (jj - 1) * nsd + (kk - 1) * nsd * np) = &
      obj1%val(jj + (kk - 1) * np) _OP_ &
      obj2%val(ii + (jj - 1) * nsd + (kk - 1) * nsd * np)
  END DO

END SUBROUTINE spacetime_spacetime

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE _MODULE_NAME_

! #undef _OP_
