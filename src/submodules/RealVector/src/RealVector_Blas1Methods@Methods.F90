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

SUBMODULE(RealVector_Blas1Methods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     ASUM
!----------------------------------------------------------------------------

MODULE PROCEDURE ASUMscalar
ans = ASUM(obj%Val)
END PROCEDURE ASUMscalar

!----------------------------------------------------------------------------
!                                                                     ASUM
!----------------------------------------------------------------------------

MODULE PROCEDURE ASUMvector
INTEGER(I4B) :: i
DO i = 1, SIZE(obj)
  ans = ans + ASUM(obj(i)%Val)
END DO
END PROCEDURE ASUMvector

!----------------------------------------------------------------------------
!                                                                      AXPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarAXPYscalar
CALL AXPY(X=X%Val, Y=Y%Val, A=A)
END PROCEDURE scalarAXPYscalar

!----------------------------------------------------------------------------
!                                                                      AXPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarAXPYintrinsic
CALL AXPY(X=X, Y=Y%Val, A=A)
END PROCEDURE scalarAXPYintrinsic

!----------------------------------------------------------------------------
!                                                                      AXPY
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorAXPYvector
INTEGER(I4B) :: i
DO i = 1, SIZE(X)
  CALL AXPY(Y=Y(i)%Val, A=A(i), X=X(i)%Val)
END DO
END PROCEDURE vectorAXPYvector

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarCOPYscalar
CALL SHALLOWCOPY(Y=Y%Val, X=X%Val)
CALL setTotalDimension(Y, 1_I4B)
CALL COPY(Y=Y%Val, X=X%Val)
END PROCEDURE scalarCOPYscalar

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarCOPYintrinsic_1a
CALL SHALLOWCOPY(Y=Y%Val, X=X)
CALL setTotalDimension(Y, 1_I4B)
Y%Val = X
END PROCEDURE scalarCOPYintrinsic_1a

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarCOPYintrinsic_1b
CALL SHALLOWCOPY(Y=Y%Val, X=X)
CALL setTotalDimension(Y, 1_I4B)
CALL COPY(Y=Y%Val, X=X)
! Y%Val = X
END PROCEDURE scalarCOPYintrinsic_1b

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE intrinsicCOPYscalar_1a
CALL SHALLOWCOPY(Y=Y, X=X%Val)
Y = X%Val
! CALL COPY(Y=Y, X=X%Val)
END PROCEDURE intrinsicCOPYscalar_1a

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE intrinsicCOPYscalar_1b
CALL SHALLOWCOPY(Y=Y, X=X%Val)
! Y = X%Val
CALL COPY(Y=Y, X=X%Val)
END PROCEDURE intrinsicCOPYscalar_1b

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorCOPYvector
INTEGER(I4B) :: i
CALL SHALLOWCOPY(Y=Y, X=X)
DO i = 1, SIZE(X)
  CALL COPY(Y=Y(i)%Val, X=X(i)%Val)
  CALL setTotalDimension(Y(i), 1_I4B)
END DO
END PROCEDURE vectorCOPYvector

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

!Y=X(:)%Val
MODULE PROCEDURE scalarCOPYvector
INTEGER(I4B) :: i, r1, r2
CALL SHALLOWCOPY(Y=Y, X=X)
CALL setTotalDimension(Y, 1_I4B)
r1 = 0; r2 = 0
DO i = 1, SIZE(X)
  r1 = r2 + 1
  r2 = r2 + SIZE(X(i)%Val)
  Y%Val(r1:r2) = X(i)%Val
END DO
END PROCEDURE scalarCOPYvector

!----------------------------------------------------------------------------
!                                                                   Compact
!----------------------------------------------------------------------------

MODULE PROCEDURE Compact_real_1
INTEGER(I4B) :: m
REAL(DFP), ALLOCATABLE :: Temp_Val(:)
m = SIZE(Val)
IF (m .GT. row) THEN
  CALL Reallocate(Temp_Val, m)
  CALL COPY(Y=Temp_Val, X=Val)
  CALL Reallocate(Val, row)
  CALL COPY(Y=Val, X=Temp_Val(1:row))
  DEALLOCATE (Temp_Val)
END IF
END PROCEDURE Compact_real_1

!----------------------------------------------------------------------------
!                                                                   Compact
!----------------------------------------------------------------------------

MODULE PROCEDURE Compact_Int_1
INTEGER(I4B) :: m
INTEGER(I4B), ALLOCATABLE :: Temp_Val(:)
m = SIZE(Val)
IF (m .GT. row) THEN
  Temp_Val = Val
  CALL Reallocate(Val, row)
  Val = Temp_Val(1:row)
  DEALLOCATE (Temp_Val)
END IF
END PROCEDURE Compact_Int_1

!----------------------------------------------------------------------------
!                                                                       DOT
!----------------------------------------------------------------------------

PURE FUNCTION inner_dot(obj1, obj2) RESULT(ans)
  REAL(DFP), INTENT(IN) :: obj1(:)
  REAL(DFP), INTENT(IN) :: obj2(:)
  REAL(DFP) :: ans
  ans = DOT(obj1, obj2)
END FUNCTION inner_dot

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarDOTscalar
ans = inner_dot(obj1%Val, obj2%Val)
END PROCEDURE scalarDOTscalar

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarDOTintrinsic
ans = inner_dot(obj%val, val)
END PROCEDURE scalarDOTintrinsic

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorDOTvector
INTEGER(I4B) :: i
ans = 0.0
DO i = 1, SIZE(obj1)
  ans = ans + DOT_PRODUCT(obj1(i), obj2(i))
END DO
END PROCEDURE vectorDOTvector

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorDOTscalar
INTEGER(I4B) :: i
ans = 0.0
DO i = 1, SIZE(obj1)
  ans = ans + DOT_PRODUCT(obj1(i)%Val, obj2%Val)
END DO
END PROCEDURE vectorDOTscalar

!----------------------------------------------------------------------------
!                                                                      DOT
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarDOTvector
INTEGER(I4B) :: i
ans = 0.0
DO i = 1, SIZE(obj2)
  ans = ans + DOT_PRODUCT(obj1%Val, obj2(i)%Val)
END DO
END PROCEDURE scalarDOTvector

!----------------------------------------------------------------------------
!                                                                      NRM2
!----------------------------------------------------------------------------

PURE FUNCTION inner_nrm2(X) RESULT(ans)
  REAL(DFP), INTENT(IN) :: X(:)
  REAL(DFP) :: ans
  ans = NRM2(X) ! blas
END FUNCTION inner_nrm2

!----------------------------------------------------------------------------
!                                                                     NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE NRM2scalar
ans = inner_nrm2(obj%Val)
END PROCEDURE NRM2scalar

!----------------------------------------------------------------------------
!                                                                     NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE NRM2vector
ans = SQRT(DOT_PRODUCT(obj, obj))
END PROCEDURE NRM2vector

!----------------------------------------------------------------------------
!                                                                     NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Norm1
ans = ASUM(obj)
END PROCEDURE obj_Norm1

!----------------------------------------------------------------------------
!                                                                     NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Normi
ans = MAXVAL(ABS(obj%val))
END PROCEDURE obj_Normi

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarSWAPscalar
CALL SWAP(X=X%Val, Y=Y%Val)
END PROCEDURE scalarSWAPscalar

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE vectorSWAPvector
INTEGER(I4B) :: i
DO i = 1, SIZE(X)
  CALL SWAP(X=X(i)%Val, Y=Y(i)%Val)
END DO
END PROCEDURE vectorSWAPvector

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE scalarSWAPintrinsic
CALL SWAP(X=X%Val, Y=Y)
END PROCEDURE scalarSWAPintrinsic

!----------------------------------------------------------------------------
!                                                                       SCAL
!----------------------------------------------------------------------------

MODULE PROCEDURE SCALscalar
CALL SCAL(A=A, X=X%Val)
END PROCEDURE SCALscalar

!----------------------------------------------------------------------------
!                                                                       SCAL
!----------------------------------------------------------------------------

MODULE PROCEDURE SCALvector
INTEGER(I4B) :: i
DO i = 1, SIZE(X)
  CALL SCAL(A=A, X=X(i)%Val)
END DO
END PROCEDURE SCALvector

!----------------------------------------------------------------------------
!                                                                      PMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PMUL1
INTEGER(I4B) :: ii, tsize

tsize = SIZE(obj)
ASSOCIATE (z => obj%val, x => obj1%val, y => obj2%val)
  DO CONCURRENT(ii=1:tsize)
    z(ii) = x(ii) * y(ii)
  END DO
END ASSOCIATE

END PROCEDURE obj_PMUL1

!----------------------------------------------------------------------------
!                                                                      PDIV
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PDIV1
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: check0

check0 = Input(default=.FALSE., option=check_divide_by_zero)
tsize = SIZE(obj)

ASSOCIATE (z => obj%val, x => obj1%val, y => obj2%val)

  IF (check0) THEN

    DO CONCURRENT(ii=1:tsize, y(ii) .NE. 0.0_DFP)
      z(ii) = x(ii) / y(ii)
    END DO

  ELSE

    DO CONCURRENT(ii=1:tsize)
      z(ii) = x(ii) / y(ii)
    END DO

  END IF

END ASSOCIATE

END PROCEDURE obj_PDIV1

!----------------------------------------------------------------------------
!                                                                Reciprocal
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Reciprocal1
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: check0

check0 = Input(default=.FALSE., option=check_divide_by_zero)
tsize = SIZE(obj1)

ASSOCIATE (x => obj1%val, y => obj2%val)

  IF (check0) THEN

    DO CONCURRENT(ii=1:tsize, y(ii) .NE. 0.0_DFP)
      x(ii) = 1.0_DFP / y(ii)
    END DO

  ELSE

    DO CONCURRENT(ii=1:tsize)
      x(ii) = 1.0_DFP / y(ii)
    END DO

  END IF

END ASSOCIATE

END PROCEDURE obj_Reciprocal1

END SUBMODULE Methods
