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

SUBMODULE(FEVariable_UnaryMethod) Methods
USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableConstant
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableTime
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE FEVariable_Method, ONLY: Get
USE IntegerUtility, ONLY: Get1DIndexFortran
USE ReallocateUtility, ONLY: Reallocate

USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE IndexUtility, ONLY: FortranIndex
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: varopt => TypeFEVariableOpt
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                         Abs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Abs
ans = obj
CALL ABS_(obj=obj, ans=ans)
END PROCEDURE obj_Abs

!----------------------------------------------------------------------------
!                                                                       ABS_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Abs_
ans%len = obj%len
ans%s = obj%s
ans%val(ans%len) = ABS(obj%val(ans%len))
END PROCEDURE obj_Abs_

!----------------------------------------------------------------------------
!                                                                      Power
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Power
ans = obj
CALL Power_(obj=obj, n=n, ans=ans)
END PROCEDURE obj_Power

!----------------------------------------------------------------------------
!                                                                      Power_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Power_
INTEGER(I4B) :: ii
ans%len = obj%len
ans%s = obj%s
DO CONCURRENT(ii=1:ans%len)
  ans%val(ii) = (obj%val(ii))**n
END DO
END PROCEDURE obj_Power_

!----------------------------------------------------------------------------
!                                                                       Sqrt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Sqrt
ans = obj
CALL SQRT_(obj=obj, ans=ans)
END PROCEDURE obj_Sqrt

!----------------------------------------------------------------------------
!                                                                      Sqrt_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Sqrt_
ans%len = obj%len
ans%s = obj%s
ans%val(ans%len) = SQRT(obj%val(ans%len))
END PROCEDURE obj_Sqrt_

!----------------------------------------------------------------------------
!                                                             NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_norm2
LOGICAL(LGT) :: isNodal

isNodal = obj%defineon .EQ. varopt%nodal

SELECT CASE (obj%vartype)
CASE (varopt%constant)

  IF (isNodal) THEN
    ans = NodalVariable( &
          val=math%zero, rank=TypeFEVariableScalar, &
          vartype=TypeFEVariableConstant)
  ELSE
    ans = QuadratureVariable( &
          val=math%zero, rank=TypeFEVariableScalar, &
          vartype=TypeFEVariableConstant)
  END IF

CASE (varopt%space)

  IF (isNodal) THEN
    ans = NodalVariable( &
          tsize=obj%s(2), rank=TypeFEVariableScalar, &
          vartype=TypeFEVariableSpace)
  ELSE
    ans = QuadratureVariable( &
          tsize=obj%s(2), rank=TypeFEVariableScalar, &
          vartype=TypeFEVariableSpace)
  END IF

CASE (varopt%time)

  IF (isNodal) THEN
    ans = NodalVariable( &
          tsize=obj%s(2), rank=TypeFEVariableScalar, &
          vartype=TypeFEVariableTime)
  ELSE
    ans = QuadratureVariable( &
          tsize=obj%s(2), rank=TypeFEVariableScalar, &
          vartype=TypeFEVariableTime)
  END IF

CASE (varopt%spacetime)

  IF (isNodal) THEN
    ans = NodalVariable( &
          nrow=obj%s(2), ncol=obj%s(3), rank=TypeFEVariableScalar, &
          vartype=TypeFEVariableSpaceTime)
  ELSE
    ans = QuadratureVariable( &
          nrow=obj%s(2), ncol=obj%s(3), rank=TypeFEVariableScalar, &
          vartype=TypeFEVariableSpaceTime)
  END IF

END SELECT

CALL Norm2_(obj=obj, ans=ans)
END PROCEDURE obj_norm2

!----------------------------------------------------------------------------
!                                                                      Norm2_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_norm2_
INTEGER(I4B) :: ii, jj, a, b

SELECT CASE (obj%vartype)

CASE (varopt%constant)

  ans%len = 1
  ans%s(1) = 1
  ans%val(1) = NORM2(obj%val(1:obj%len))

CASE (varopt%space)

  ans%s(1) = obj%s(2)
  ans%len = ans%s(1)

  DO jj = 1, obj%s(2)
    a = FortranIndex(1, jj, obj%s(1), obj%s(2))
    b = FortranIndex(obj%s(1), jj, obj%s(1), obj%s(2))
    ans%val(jj) = NORM2(obj%val(a:b))
  END DO

CASE (varopt%time)

  ans%s(1) = obj%s(2)
  ans%len = ans%s(1)

  DO jj = 1, obj%s(2)
    a = FortranIndex(1, jj, obj%s(1), obj%s(2))
    b = FortranIndex(obj%s(1), jj, obj%s(1), obj%s(2))
    ans%val(jj) = NORM2(obj%val(a:b))
  END DO

CASE (varopt%spacetime)

  ans%s(1:2) = obj%s(2:3)
  ans%len = ans%s(1) * ans%s(2)

  DO jj = 1, obj%s(3)
    DO ii = 1, obj%s(2)
      a = FortranIndex(1, ii, jj, obj%s(1), obj%s(2), obj%s(3))
      b = FortranIndex(obj%s(1), ii, jj, obj%s(1), obj%s(2), obj%s(3))
      ans%val(FortranIndex(ii, jj, ans%s(1), ans%s(2))) = NORM2(obj%val(a:b))
    END DO
  END DO
END SELECT
END PROCEDURE obj_norm2_

!----------------------------------------------------------------------------
!                                                                    IsEqual
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsEqual
LOGICAL(LGT) :: isok

ans = .FALSE.

isok = obj1%len .NE. obj2%len
IF (isok) RETURN

isok = obj1%defineon .NE. obj2%defineon
IF (isok) RETURN

isok = obj1%rank .NE. obj2%rank
IF (isok) RETURN

isok = obj1%varType .NE. obj2%varType
IF (isok) RETURN

isok = obj1%tshape .NE. obj2%tshape
IF (isok) RETURN

isok = ANY(obj1%s(1:obj1%tshape) .NE. obj2%s(1:obj2%tshape))
IF (isok) RETURN

isok = ALL(obj1%val(1:obj1%len) .APPROXEQ.obj2%val(1:obj2%len))
IF (isok) ans = .TRUE.
END PROCEDURE obj_IsEqual

!----------------------------------------------------------------------------
!                                                                   NotEqual
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_NotEqual
LOGICAL(LGT) :: isok

ans = .FALSE.

isok = .NOT. ALL(obj1%val.APPROXEQ.obj2%val)
IF (isok) THEN
  ans = .TRUE.
  RETURN
END IF

isok = obj1%defineon .NE. obj2%defineon
IF (isok) THEN
  ans = .TRUE.
  RETURN
END IF

isok = obj1%rank .NE. obj2%rank
IF (isok) THEN
  ans = .TRUE.
  RETURN
END IF

isok = obj1%varType .NE. obj2%varType
IF (isok) THEN
  ans = .TRUE.
  RETURN
END IF

isok = ANY(obj1%s .NE. obj2%s)
IF (isok) THEN
  ans = .TRUE.
  RETURN
END IF
END PROCEDURE obj_NotEqual

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods

