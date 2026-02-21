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
PUBLIC :: Scalar_Scalar_Master

CONTAINS

!----------------------------------------------------------------------------
!                                                                      master
!----------------------------------------------------------------------------

PURE SUBROUTINE Scalar_Scalar_Master(obj1, obj2, ans, varCase)
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
  CASE DEFAULT
  END SELECT
END SUBROUTINE Scalar_Scalar_Master

!----------------------------------------------------------------------------
!                                                          constant constant
!----------------------------------------------------------------------------

! Result will be a constant scalar
PURE SUBROUTINE constant_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = 1
  ans%s(1) = 1
  ans%val(1) = obj1%val(1) _OP_ obj2%val(1)
  ans%varType = varopt%constant
END SUBROUTINE constant_constant

!----------------------------------------------------------------------------
!                                                              constant space
!----------------------------------------------------------------------------

! ans will be a space scalar
PURE SUBROUTINE constant_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj2%len
  ans%s(1) = obj2%s(1)
  ans%val(1:ans%len) = obj1%val(1) _OP_ obj2%val(1:ans%len)
  ans%varType = varopt%space
END SUBROUTINE constant_space

!----------------------------------------------------------------------------
!                                                              constant time
!----------------------------------------------------------------------------

! ans will be time scalar
PURE SUBROUTINE constant_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj2%len
  ans%s(1) = obj2%s(1)
  ans%val(1:ans%len) = obj1%val(1) _OP_ obj2%val(1:ans%len)
  ans%varType = varopt%time
END SUBROUTINE constant_time

!----------------------------------------------------------------------------
!                                                          constant spacetime
!----------------------------------------------------------------------------

! ans will be a spacetime scalar
PURE SUBROUTINE constant_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj2%len
  ans%s(1:2) = obj2%s(1:2)
  ans%val(1:ans%len) = obj1%val(1) _OP_ obj2%val(1:ans%len)
  ans%varType = varopt%spacetime
END SUBROUTINE constant_spacetime

!----------------------------------------------------------------------------
!                                                              space constant
!----------------------------------------------------------------------------

! ans will be a space scalar
PURE SUBROUTINE space_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj1%len
  ans%s(1) = obj1%s(1)
  ans%val(1:ans%len) = obj1%val(1:ans%len) _OP_ obj2%val(1)
  ans%varType = varopt%space
END SUBROUTINE space_constant

!----------------------------------------------------------------------------
!                                                                space space
!----------------------------------------------------------------------------

! ans will be a space scalar
PURE SUBROUTINE space_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%s(1) = MIN(obj1%s(1), obj2%s(1))
  ans%len = ans%s(1)
  ans%val(1:ans%len) = obj1%val(1:ans%len) _OP_ obj2%val(1:ans%len)
  ans%varType = varopt%space
END SUBROUTINE space_space

!----------------------------------------------------------------------------
!                                                                space time
!----------------------------------------------------------------------------

! ans will be a spacetime scalar
PURE SUBROUTINE space_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: jj, kk, np, nnt

  np = obj1%s(1)
  nnt = obj2%s(1)
  ans%s(1) = np
  ans%s(2) = nnt
  ans%len = np * nnt
  ans%varType = varopt%spacetime

  DO CONCURRENT(jj=1:np, kk=1:nnt)
    ans%val(FortranIndex(jj, kk, np, nnt)) = &
      obj1%val(jj) _OP_ &
      obj2%val(kk)
  END DO
END SUBROUTINE space_time

!----------------------------------------------------------------------------
!                                                            space spacetime
!----------------------------------------------------------------------------

! ans will be a spacetime scalar
PURE SUBROUTINE space_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: jj, kk, np, nnt

  np = MIN(obj1%s(1), obj2%s(1))
  nnt = obj2%s(2)
  ans%s(1) = np
  ans%s(2) = nnt
  ans%len = np * nnt
  ans%varType = varopt%spacetime

  DO CONCURRENT(jj=1:np, kk=1:nnt)
    ans%val(FortranIndex(jj, kk, np, nnt)) = &
      obj1%val(jj) _OP_ &
      obj2%val(FortranIndex(jj, kk, np, nnt))
  END DO
END SUBROUTINE space_spacetime

!----------------------------------------------------------------------------
!                                                              time constant
!----------------------------------------------------------------------------

! ans will be a time scalar
PURE SUBROUTINE time_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj1%len
  ans%s(1) = obj1%s(1)
  ans%val(1:ans%len) = obj1%val(1:ans%len) _OP_ obj2%val(1)
  ans%varType = varopt%time

END SUBROUTINE time_constant

!----------------------------------------------------------------------------
!                                                                  time time
!----------------------------------------------------------------------------

! ans will be a time scalar
PURE SUBROUTINE time_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%s(1) = MIN(obj1%s(1), obj2%s(1))
  ans%len = ans%s(1)
  ans%val(1:ans%len) = obj1%val(1:ans%len) _OP_ obj2%val(1:ans%len)
  ans%varType = varopt%time
END SUBROUTINE time_time

!----------------------------------------------------------------------------
!                                                                  time space
!----------------------------------------------------------------------------

! ans will be a spacetime scalar
PURE SUBROUTINE time_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: jj, kk, np, nnt

  np = obj2%s(2)
  nnt = obj1%s(1)
  ans%s(1) = np
  ans%s(2) = nnt
  ans%len = np * nnt
  ans%varType = varopt%spacetime

  DO CONCURRENT(jj=1:np, kk=1:nnt)
    ans%val(FortranIndex(jj, kk, np, nnt)) = &
      obj1%val(kk) _OP_ &
      obj2%val(jj)
  END DO
END SUBROUTINE time_space

!----------------------------------------------------------------------------
!                                                              time spacetime
!----------------------------------------------------------------------------

! ans will be a spacetime scalar
PURE SUBROUTINE time_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: jj, kk, np, nnt

  np = obj2%s(1)
  nnt = MIN(obj1%s(1), obj2%s(2))
  ans%s(1) = np
  ans%s(2) = nnt
  ans%len = np * nnt
  ans%varType = varopt%spacetime

  DO CONCURRENT(jj=1:np, kk=1:nnt)
    ans%val(FortranIndex(jj, kk, np, nnt)) = &
      obj1%val(kk) _OP_ &
      obj2%val(FortranIndex(jj, kk, np, nnt))
  END DO
END SUBROUTINE time_spacetime

!----------------------------------------------------------------------------
!                                                          spacetime constant
!----------------------------------------------------------------------------

! result will be a spacetime scalar
PURE SUBROUTINE spacetime_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%s(1:2) = obj1%s(1:2)
  ans%len = obj1%len
  ans%val(1:ans%len) = obj1%val(1:ans%len) _OP_ obj2%val(1)
  ans%varType = varopt%spacetime

END SUBROUTINE spacetime_constant

!----------------------------------------------------------------------------
!                                                            spacetime space
!----------------------------------------------------------------------------

! result will be a spacetime scalar
PURE SUBROUTINE spacetime_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: np, nnt, jj, kk

  np = MIN(obj1%s(1), obj2%s(1))
  nnt = obj1%s(2)
  ans%s(1) = np
  ans%s(2) = nnt
  ans%len = np * nnt
  ans%varType = varopt%spacetime

  DO CONCURRENT(jj=1:np, kk=1:nnt)
    ans%val(FortranIndex(jj, kk, np, nnt)) = &
      obj1%val(FortranIndex(jj, kk, np, nnt)) _OP_ &
      obj2%val(jj)
  END DO
END SUBROUTINE spacetime_space

!----------------------------------------------------------------------------
!                                                              spacetime time
!----------------------------------------------------------------------------

! result will be a spacetime scalar
PURE SUBROUTINE spacetime_time(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  INTEGER(I4B) :: np, nnt, jj, kk

  np = obj1%s(1)
  nnt = MIN(obj1%s(2), obj2%s(1))
  ans%s(1) = np
  ans%s(2) = nnt
  ans%len = np * nnt
  ans%varType = varopt%spacetime

  DO CONCURRENT(jj=1:np, kk=1:nnt)
    ans%val(FortranIndex(jj, kk, np, nnt)) = &
      obj1%val(FortranIndex(jj, kk, np, nnt)) _OP_ &
      obj2%val(kk)
  END DO
END SUBROUTINE spacetime_time

!----------------------------------------------------------------------------
!                                                         spacetime spacetime
!----------------------------------------------------------------------------

! result will be a spacetime scalar
PURE SUBROUTINE spacetime_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%s(1) = MIN(obj1%s(1), obj2%s(1))
  ans%s(2) = MIN(obj1%s(2), obj2%s(2))
  ans%len = ans%s(1) * ans%s(2)
  ans%varType = varopt%spacetime
  ans%val(1:ans%len) = obj1%val(1:ans%len) _OP_ obj2%val(1:ans%len)
END SUBROUTINE spacetime_spacetime

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
