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

USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: varopt => TypeFEVariableOpt
USE GlobalData, ONLY: I4B, DFP, LGT
USE FEVariable_Method, ONLY: GetRankCase
USE FEVariable_Method, ONLY: GetVarCase
USE IndexUtility, ONLY: FortranIndex

PRIVATE

PUBLIC :: Scalar_Matrix_Master

CONTAINS

!----------------------------------------------------------------------------
!                                                                      master
!----------------------------------------------------------------------------

PURE SUBROUTINE Scalar_Matrix_Master(obj1, obj2, ans, varCase)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans
  INTEGER(I4B), INTENT(IN) :: varCase

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
    CALL time_space(obj1, obj2, ans)
  CASE (22)
    CALL time_time(obj1, obj2, ans)
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
END SUBROUTINE Scalar_Matrix_Master

!----------------------------------------------------------------------------
!                                                          constant constant
!----------------------------------------------------------------------------

! Result will be a constant matrix
PURE SUBROUTINE constant_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj2%len
  ans%s(1:2) = obj2%s(1:2)
  ans%val(1:ans%len) = obj1%val(1) _OP_ obj2%val(1:ans%len)
END SUBROUTINE constant_constant

!----------------------------------------------------------------------------
!                                                              constant space
!----------------------------------------------------------------------------

! ans will be a space matrix
PURE SUBROUTINE constant_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj2%len
  ans%s(1:3) = obj2%s(1:3)
  ans%val(1:ans%len) = obj1%val(1) _OP_ obj2%val(1:ans%len)
END SUBROUTINE constant_space

!----------------------------------------------------------------------------
!                                                              constant time
!----------------------------------------------------------------------------

! ans will be a time matrix
PURE SUBROUTINE constant_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj2%len
  ans%s(1:3) = obj2%s(1:3)
  ans%val(1:ans%len) = obj1%val(1) _OP_ obj2%val(1:ans%len)
END SUBROUTINE constant_time

!----------------------------------------------------------------------------
!                                                          constant spacetime
!----------------------------------------------------------------------------

! ans will be a spacetime matrix
PURE SUBROUTINE constant_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj2%len
  ans%s(1:4) = obj2%s(1:4)
  ans%val(1:ans%len) = obj1%val(1) _OP_ obj2%val(1:ans%len)
END SUBROUTINE constant_spacetime

!----------------------------------------------------------------------------
!                                                              space constant
!----------------------------------------------------------------------------

! ans will be a space matrix
PURE SUBROUTINE space_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, kk, nx, ny, np

  nx = obj2%s(1); ny = obj2%s(2); np = obj1%s(1)
  ans%s(1) = nx; ans%s(2) = ny; ans%s(3) = np
  ans%len = nx * ny * np

  DO CONCURRENT(ii=1:nx, jj=1:ny, kk=1:np)
    ans%val(FortranIndex(ii, jj, kk, nx, ny, np)) = &
      obj1%val(kk) _OP_ &
      obj2%val(FortranIndex(ii, jj, nx, ny))
  END DO
END SUBROUTINE space_constant

!----------------------------------------------------------------------------
!                                                                space space
!----------------------------------------------------------------------------

! ans will be a space matrix
PURE SUBROUTINE space_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, kk, nx, ny, np

  nx = obj2%s(1); ny = obj2%s(2); np = MIN(obj1%s(1), obj2%s(3))
  ans%s(1) = nx; ans%s(2) = ny; ans%s(3) = np
  ans%len = nx * ny * np

  DO CONCURRENT(ii=1:nx, jj=1:ny, kk=1:np)
    ans%val(FortranIndex(ii, jj, kk, nx, ny, np)) = &
      obj1%val(kk) _OP_ &
      obj2%val(FortranIndex(ii, jj, kk, nx, ny, np))
  END DO
END SUBROUTINE space_space

!----------------------------------------------------------------------------
!                                                                  space time
!----------------------------------------------------------------------------

! ans will be a space-time matrix
PURE SUBROUTINE space_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, kk, ll, nx, ny, np, nnt

  nx = obj2%s(1); ny = obj2%s(2); nnt = obj2%s(3); np = obj1%s(1)
  ans%s(1) = nx; ans%s(2) = ny; ans%s(3) = np; ans%s(4) = nnt

  ans%len = nx * ny * np * nnt

  DO CONCURRENT(ii=1:nx, jj=1:ny, kk=1:np, ll=1:nnt)
    ans%val(FortranIndex(ii, jj, kk, ll, nx, ny, np, nnt)) = &
      obj1%val(kk) _OP_ obj2%val(FortranIndex(ii, jj, ll, nx, ny, nnt))
  END DO
END SUBROUTINE space_time

!----------------------------------------------------------------------------
!                                                                space time
!----------------------------------------------------------------------------

! ans will be a space time matrix
PURE SUBROUTINE space_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, kk, ll, nx, ny, np, nnt

  nx = obj2%s(1); ny = obj2%s(2); nnt = obj2%s(4)
  np = MIN(obj1%s(1), obj2%s(3))

  ans%s(1) = nx; ans%s(2) = ny; ans%s(3) = np; ans%s(4) = nnt
  ans%len = nx * ny * np * nnt

  DO CONCURRENT(ii=1:nx, jj=1:ny, kk=1:np, ll=1:nnt)
    ans%val(FortranIndex(ii, jj, kk, ll, nx, ny, np, nnt)) = &
      obj1%val(kk) _OP_ &
      obj2%val(FortranIndex(ii, jj, kk, ll, nx, ny, np, nnt))
  END DO
END SUBROUTINE space_spacetime

!----------------------------------------------------------------------------
!                                                              time constant
!----------------------------------------------------------------------------

! ans will be a time matrix
PURE SUBROUTINE time_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ! internal variables
  INTEGER(I4B) :: ii, jj, ll, nx, ny, nnt

  nx = obj2%s(1); ny = obj2%s(2); nnt = obj1%s(1)

  ans%s(1) = nx; ans%s(2) = ny; ans%s(3) = nnt
  ans%len = nx * ny * nnt

  DO CONCURRENT(ii=1:nx, jj=1:ny, ll=1:nnt)
    ans%val(FortranIndex(ii, jj, ll, nx, ny, nnt)) = &
      obj1%val(ll) _OP_ &
      obj2%val(FortranIndex(ii, jj, nx, ny))
  END DO
END SUBROUTINE time_constant

!----------------------------------------------------------------------------
!                                                                time time
!----------------------------------------------------------------------------

! ans will be a time matrix
PURE SUBROUTINE time_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, ll, nx, ny, nnt

  nx = obj2%s(1); ny = obj2%s(2); nnt = MIN(obj1%s(1), obj2%s(3))

  ans%s(1) = nx; ans%s(2) = ny; ans%s(3) = nnt
  ans%len = nx * ny * nnt

  DO CONCURRENT(ii=1:nx, jj=1:ny, ll=1:nnt)
    ans%val(FortranIndex(ii, jj, ll, nx, ny, nnt)) = &
      obj1%val(ll) _OP_ &
      obj2%val(FortranIndex(ii, jj, ll, nx, ny, nnt))
  END DO
END SUBROUTINE time_time

!----------------------------------------------------------------------------
!                                                                time space
!----------------------------------------------------------------------------

! ans will be a space time matrix
PURE SUBROUTINE time_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, kk, ll, nx, ny, np, nnt

  nx = obj2%s(1); ny = obj2%s(2); np = obj2%s(3); nnt = obj1%s(1)
  ans%s(1) = nx; ans%s(2) = ny; ans%s(3) = np; ans%s(4) = nnt

  ans%len = nx * ny * np * nnt

  DO CONCURRENT(ii=1:nx, jj=1:ny, kk=1:np, ll=1:nnt)
    ans%val(FortranIndex(ii, jj, kk, ll, nx, ny, np, nnt)) = &
      obj1%val(ll) _OP_ &
      obj2%val(FortranIndex(ii, jj, kk, nx, ny, np))
  END DO
END SUBROUTINE time_space

!----------------------------------------------------------------------------
!                                                                time space
!----------------------------------------------------------------------------

! ans will be a space time matrix
PURE SUBROUTINE time_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: ii, jj, kk, ll, nx, ny, np, nnt

  nx = obj2%s(1); ny = obj2%s(2); np = obj2%s(3)
  nnt = MIN(obj1%s(1), obj2%s(4))

  ans%s(1) = nx; ans%s(2) = ny; ans%s(3) = np; ans%s(4) = nnt
  ans%len = nx * ny * np * nnt

  DO CONCURRENT(ii=1:nx, jj=1:ny, kk=1:np, ll=1:nnt)
    ans%val(FortranIndex(ii, jj, kk, ll, nx, ny, np, nnt)) = &
      obj1%val(ll) _OP_ &
      obj2%val(FortranIndex(ii, jj, kk, ll, nx, ny, np, nnt))
  END DO
END SUBROUTINE time_spacetime

!----------------------------------------------------------------------------
!                                                          spacetime constant
!----------------------------------------------------------------------------

! result will be a spacetime matrix
PURE SUBROUTINE spacetime_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: nx, ny, np, nnt, ii, jj, kk, ll

  nx = obj2%s(1); ny = obj2%s(2); np = obj1%s(1); nnt = obj1%s(2)

  ans%s(1) = nx; ans%s(2) = ny; ans%s(3) = np; ans%s(4) = nnt
  ans%len = nx * ny * np * nnt

  DO CONCURRENT(ii=1:nx, jj=1:ny, kk=1:np, ll=1:nnt)
    ans%val(FortranIndex(ii, jj, kk, ll, nx, ny, np, nnt)) = &
      obj1%val(FortranIndex(kk, ll, np, nnt)) _OP_ &
      obj2%val(FortranIndex(ii, jj, nx, ny))
  END DO
END SUBROUTINE spacetime_constant

!----------------------------------------------------------------------------
!                                                          spacetime space
!----------------------------------------------------------------------------

! result will be a spacetime space time matrix
PURE SUBROUTINE spacetime_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: nx, ny, np, nnt, ii, jj, kk, ll

  nx = obj2%s(1); ny = obj2%s(2); np = MIN(obj1%s(1), obj2%s(3))
  nnt = obj1%s(2)
  ans%s(1) = nx; ans%s(2) = ny; ans%s(3) = np; ans%s(4) = nnt
  ans%len = nx * ny * np * nnt

  DO CONCURRENT(ii=1:nx, jj=1:ny, kk=1:np, ll=1:nnt)
    ans%val(FortranIndex(ii, jj, kk, ll, nx, ny, np, nnt)) = &
      obj1%val(FortranIndex(kk, ll, np, nnt)) _OP_ &
      obj2%val(FortranIndex(ii, jj, kk, nx, ny, np))
  END DO
END SUBROUTINE spacetime_space

!----------------------------------------------------------------------------
!                                                          spacetime time
!----------------------------------------------------------------------------

! result will be a spacetime matrix
PURE SUBROUTINE spacetime_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: nx, ny, np, nnt, ii, jj, kk, ll

  nx = obj2%s(1); ny = obj2%s(2); np = obj1%s(1)
  nnt = MIN(obj1%s(2), obj2%s(3))

  ans%s(1) = nx; ans%s(2) = ny; ans%s(3) = np; ans%s(4) = nnt
  ans%len = nx * ny * np * nnt

  DO CONCURRENT(ii=1:nx, jj=1:ny, kk=1:np, ll=1:nnt)
    ans%val(FortranIndex(ii, jj, kk, ll, nx, ny, np, nnt)) = &
      obj1%val(FortranIndex(kk, ll, np, nnt)) _OP_ &
      obj2%val(FortranIndex(ii, jj, ll, nx, ny, nnt))
  END DO
END SUBROUTINE spacetime_time

!----------------------------------------------------------------------------
!                                                        spacetime spacetime
!----------------------------------------------------------------------------

! result will be a spacetime matrix
PURE SUBROUTINE spacetime_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: nx, ny, np, nnt, ii, jj, kk, ll

  nx = obj2%s(1); ny = obj2%s(2); np = MIN(obj1%s(1), obj2%s(3))
  nnt = MIN(obj1%s(2), obj2%s(4))

  ans%s(1) = nx; ans%s(2) = ny; ans%s(3) = np; ans%s(4) = nnt
  ans%len = nx * ny * np * nnt

  DO CONCURRENT(ii=1:nx, jj=1:ny, kk=1:np, ll=1:nnt)
    ans%val(FortranIndex(ii, jj, kk, ll, nx, ny, np, nnt)) = &
      obj1%val(FortranIndex(kk, ll, np, nnt)) _OP_ &
      obj2%val(FortranIndex(ii, jj, kk, ll, nx, ny, np, nnt))
  END DO
END SUBROUTINE spacetime_spacetime

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

