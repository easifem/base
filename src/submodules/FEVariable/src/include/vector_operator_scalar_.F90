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

MODULE _MODULE_NAME_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: varopt => TypeFEVariableOpt
USE GlobalData, ONLY: I4B, DFP, LGT
USE FEVariable_Method, ONLY: GetRankCase
USE FEVariable_Method, ONLY: GetVarCase
USE IndexUtility, ONLY: FortranIndex

PRIVATE

PUBLIC :: Vector_Scalar_Master

CONTAINS

!----------------------------------------------------------------------------
!                                                                      master
!----------------------------------------------------------------------------

PURE SUBROUTINE Vector_Scalar_Master(obj1, obj2, ans)
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
END SUBROUTINE Vector_Scalar_Master

!----------------------------------------------------------------------------
!                                                          constant constant
!----------------------------------------------------------------------------

! Result will be a constant vector
PURE SUBROUTINE constant_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj1%len
  ans%s(1) = obj1%s(1)
  ans%val(1:ans%len) = obj1%val(1:ans%len) _OP_ obj2%val(1)
END SUBROUTINE constant_constant

!----------------------------------------------------------------------------
!                                                             constant space
!----------------------------------------------------------------------------

! ans will be a space vector
PURE SUBROUTINE constant_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, nsd, np

  nsd = obj1%s(1)
  np = obj2%s(1)

  ans%s(1) = nsd ! take space compo from obj1
  ans%s(2) = np ! take number of points from obj2
  ans%len = nsd * np

  DO CONCURRENT(ii=1:nsd, jj=1:np)
    ! ans index: nsd, np
    ! obj1 index: nsd
    ! obj2 index: np
    ans%val(FortranIndex(ii, jj, nsd, np)) = &
      obj1%val(ii) _OP_ &
      obj2%val(jj)
  END DO
END SUBROUTINE constant_space

!----------------------------------------------------------------------------
!                                                             constant time
!----------------------------------------------------------------------------

! ans will be a time vector
PURE SUBROUTINE constant_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ! internal variables
  INTEGER(I4B) :: ii, kk, nsd, nnt

  nsd = obj1%s(1); nnt = obj2%s(1)

  ans%s(1) = nsd; ans%s(2) = nnt
  ans%len = nsd * nnt

  DO CONCURRENT(ii=1:nsd, kk=1:nnt)
    ! ans index: nsd, nnt
    ! obj1 index: nsd
    ! obj2 index: nnt
    ans%val(FortranIndex(ii, kk, nsd, nnt)) = &
      obj1%val(ii) _OP_ &
      obj2%val(kk)
  END DO
END SUBROUTINE constant_time

!----------------------------------------------------------------------------
!                                                          constant spacetime
!----------------------------------------------------------------------------

! result will be a spacetime vector
PURE SUBROUTINE constant_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: nsd, np, nnt, ii, jj, kk

  nsd = obj1%s(1); np = obj2%s(1); nnt = obj2%s(2)
  ans%s(1) = nsd; ans%s(2) = np; ans%s(3) = nnt

  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ! ans index: nsd, np, nnt
    ! obj1 index: nsd
    ! obj2 index: np, nnt
    ans%val(FortranIndex(ii, jj, kk, nsd, np, nnt)) = &
      obj1%val(ii) _OP_ &
      obj2%val(FortranIndex(jj, kk, np, nnt))
  END DO
END SUBROUTINE constant_spacetime

!----------------------------------------------------------------------------
!                                                              space constant
!----------------------------------------------------------------------------

! ans will be a space vector
PURE SUBROUTINE space_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj1%len
  ans%s(1:2) = obj1%s(1:2)
  ans%val(1:ans%len) = obj1%val(1:ans%len) _OP_ obj2%val(1)
END SUBROUTINE space_constant

!----------------------------------------------------------------------------
!                                                                space space
!----------------------------------------------------------------------------

! ans will be a space vector
PURE SUBROUTINE space_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, nsd, np

  nsd = obj1%s(1)
  np = MIN(obj2%s(1), obj1%s(2))

  ans%s(1) = nsd; ans%s(2) = np
  ans%len = nsd * np

  DO CONCURRENT(ii=1:nsd, jj=1:np)
    ! ans index: nsd, np
    ! obj1 index: nsd, np
    ! obj2 index: np
    ans%val(FortranIndex(ii, jj, nsd, np)) = &
      obj1%val(FortranIndex(ii, jj, nsd, np)) _OP_ &
      obj2%val(jj)
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

  nsd = obj1%s(1); np = obj1%s(2); nnt = obj2%s(1)
  ans%s(1) = nsd; ans%s(2) = np; ans%s(3) = nnt

  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ! ans index: nsd, np, nnt
    ! obj1 index: nsd, np
    ! obj2 index: nnt
    ans%val(FortranIndex(ii, jj, kk, nsd, np, nnt)) = &
      obj1%val(FortranIndex(ii, jj, nsd, np)) _OP_ &
      obj2%val(kk)
  END DO
END SUBROUTINE space_time

!----------------------------------------------------------------------------
!                                                             space spacetime
!----------------------------------------------------------------------------

! result will be a spacetime vector
PURE SUBROUTINE space_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: nsd, np, nnt, ii, jj, kk

  nsd = obj1%s(1); np = MIN(obj2%s(1), obj1%s(2)); nnt = obj2%s(2)
  ans%s(1) = nsd; ans%s(2) = np; ans%s(3) = nnt
  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ! ans index: nsd, np, nnt
    ! obj1 index: nsd, np
    ! obj2 index: np, nnt
    ans%val(FortranIndex(ii, jj, kk, nsd, np, nnt)) = &
      obj1%val(FortranIndex(ii, jj, nsd, np)) _OP_ &
      obj2%val(FortranIndex(jj, kk, np, nnt))
  END DO
END SUBROUTINE space_spacetime

!----------------------------------------------------------------------------
!                                                               time constant
!----------------------------------------------------------------------------

! ans will be a time vector
PURE SUBROUTINE time_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj1%len
  ans%s(1:2) = obj1%s(1:2)
  ans%val(1:ans%len) = obj1%val(1:ans%len) _OP_ obj2%val(1)
END SUBROUTINE time_constant

!----------------------------------------------------------------------------
!                                                                  time space
!----------------------------------------------------------------------------

! ans will be a space time vector
PURE SUBROUTINE time_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, kk, nsd, np, nnt

  nsd = obj1%s(1); nnt = obj1%s(2); np = obj2%s(1)
  ans%s(1) = nsd; ans%s(2) = np; ans%s(3) = nnt
  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ans%val(FortranIndex(ii, jj, kk, nsd, np, nnt)) = &
      obj1%val(FortranIndex(ii, kk, nsd, nnt)) _OP_ &
      obj2%val(jj)
  END DO
END SUBROUTINE time_space

!----------------------------------------------------------------------------
!                                                                  time time
!----------------------------------------------------------------------------

! ans will be a time vector
PURE SUBROUTINE time_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, kk, nsd, nnt

  nsd = obj1%s(1); nnt = MIN(obj2%s(1), obj1%s(2))

  ans%s(1) = nsd; ans%s(2) = nnt
  ans%len = nsd * nnt

  DO CONCURRENT(ii=1:nsd, kk=1:nnt)
    ! ans index: nsd, nnt
    ! obj1 index: nsd, nnt
    ! obj2 index: nnt
    ans%val(FortranIndex(ii, kk, nsd, nnt)) = &
      obj1%val(FortranIndex(ii, kk, nsd, nnt)) _OP_ &
      obj2%val(kk)
  END DO
END SUBROUTINE time_time

!----------------------------------------------------------------------------
!                                                             time spacetime
!----------------------------------------------------------------------------

! result will be a spacetime vector
PURE SUBROUTINE time_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: nsd, np, nnt, ii, jj, kk

  nsd = obj1%s(1); np = obj2%s(1); nnt = MIN(obj2%s(2), obj1%s(2))
  ans%s(1) = nsd; ans%s(2) = np; ans%s(3) = nnt
  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ! ans index: nsd, np, nnt
    ! obj1 index: nsd, nnt
    ! obj2 index: np, nnt
    ans%val(FortranIndex(ii, jj, kk, nsd, np, nnt)) = &
      obj1%val(FortranIndex(ii, kk, nsd, nnt)) _OP_ &
      obj2%val(FortranIndex(jj, kk, np, nnt))
  END DO

END SUBROUTINE time_spacetime

!----------------------------------------------------------------------------
!                                                          spacetime constant
!----------------------------------------------------------------------------

! ans will be a space time vector
PURE SUBROUTINE spacetime_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj1%len
  ans%s(1:3) = obj1%s(1:3)
  ans%val(1:ans%len) = obj1%val(1:ans%len) _OP_ obj2%val(1)
END SUBROUTINE spacetime_constant

!----------------------------------------------------------------------------
!                                                             spacetime space
!----------------------------------------------------------------------------

! ans will be a space-time vector
PURE SUBROUTINE spacetime_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, kk, nsd, np, nnt

  nsd = obj1%s(1); nnt = obj1%s(3); np = MIN(obj2%s(1), obj1%s(2))

  ans%s(1) = nsd; ans%s(2) = np; ans%s(3) = nnt
  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ans%val(FortranIndex(ii, jj, kk, nsd, np, nnt)) = &
      obj1%val(FortranIndex(ii, jj, kk, nsd, np, nnt)) _OP_ &
      obj2%val(jj)
  END DO
END SUBROUTINE spacetime_space

!----------------------------------------------------------------------------
!                                                             spacetime time
!----------------------------------------------------------------------------

! ans will be a space time vector
PURE SUBROUTINE spacetime_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, kk, nsd, np, nnt

  nsd = obj1%s(1); np = obj1%s(2); nnt = MIN(obj2%s(1), obj1%s(3))

  ans%s(1) = nsd; ans%s(2) = np; ans%s(3) = nnt

  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ! ans index: nsd, np, nnt
    ! obj1 index: nsd, np, nnt
    ! obj2 index: nnt
    ans%val(FortranIndex(ii, jj, kk, nsd, np, nnt)) = &
      obj1%val(FortranIndex(ii, jj, kk, nsd, np, nnt)) _OP_ &
      obj2%val(kk)
  END DO
END SUBROUTINE spacetime_time

!----------------------------------------------------------------------------
!                                                        spacetime spacetime
!----------------------------------------------------------------------------

! result will be a spacetime vector
PURE SUBROUTINE spacetime_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: nsd, np, nnt, ii, jj, kk

  nsd = obj1%s(1); np = MIN(obj2%s(1), obj1%s(2))
  nnt = MIN(obj2%s(2), obj1%s(3))

  ans%s(1) = nsd; ans%s(2) = np; ans%s(3) = nnt
  ans%len = nsd * np * nnt

  DO CONCURRENT(ii=1:nsd, jj=1:np, kk=1:nnt)
    ! ans index: nsd, np, nnt
    ! obj1 index: nsd, np, nnt
    ! obj2 index: np, nnt
    ans%val(FortranIndex(ii, jj, kk, nsd, np, nnt)) = &
      obj1%val(FortranIndex(ii, jj, kk, nsd, np, nnt)) _OP_ &
      obj2%val(FortranIndex(jj, kk, np, nnt))
  END DO
END SUBROUTINE spacetime_spacetime

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE _MODULE_NAME_

