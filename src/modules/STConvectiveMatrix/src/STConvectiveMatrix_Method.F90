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

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This module contains method to construct finite element matrices

MODULE STConvectiveMatrix_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: STConvectiveMatrix

!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_1(test, trial, &
    & term1, term2, opt, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    !! This option is used to create
    !! ncopy Mii(I,J,a,b)
    !! and Mi1(I,J,a,b)
    !! and M1i(I,J,a,b)
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    !! "trial" take projection of C on trial
    !! "test" take projection of C on test
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_1
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_1
END INTERFACE STConvectiveMatrix

!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_2(test, trial, c, crank, &
    & term1, term2, opt, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    !! scalar FEVariable
    TYPE(FEVariableScalar_), INTENT( IN ) :: crank
    !! scalar variable
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    !! This option is used to create
    !! ncopy Mii(I,J,a,b)
    !! and Mi1(I,J,a,b)
    !! and M1i(I,J,a,b)
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    !! "trial" take projection of C on trial
    !! "test" take projection of C on test
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_2
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_2
END INTERFACE STConvectiveMatrix

!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_3(test, trial, c, crank, &
    & term1, term2, opt, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    !! vector FEVariable, convective velocity
    TYPE(FEVariableVector_), INTENT( IN ) :: crank
    !! convective velocity
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z => space derivative
    !! DEL_t => time derivative
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    !! This option is used to create
    !! ncopy Mii(I,J,a,b)
    !! and Mi1(I,J,a,b)
    !! and M1i(I,J,a,b)
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    !! "trial" take projection of C on trial
    !! "test" take projection of C on test
    !! it is needed only when
    !! term1=term2= {del_x, del_y, del_z, del_x_all}
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_3
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_3
END INTERFACE STConvectiveMatrix

!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_4(test, trial, c1, c2, &
      & c1rank, c2rank, term1, term2, opt, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! Scalar FE variable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! convective velocity, vector FEVariable,
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! Scalar FE variable
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    !! vector FEVariable,
    INTEGER(I4B), INTENT(IN) :: term1
    !! term1 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z, DEL_X_ALL => space derivative
    !! DEL_t => time derivative
    INTEGER(I4B), INTENT(IN) :: term2
    !! term2 denotes first order derivative in space or time
    !! DEL_NONE => no derivative
    !! DEL_X, DEL_Y, DEL_Z, DEL_X_ALL => space derivative
    !! DEL_t => time derivative
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
    !! This option is used to create
    !! ncopy Mii(I,J,a,b)
    !! and Mi1(I,J,a,b)
    !! and M1i(I,J,a,b)
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: projecton
    !! "trial" take projection of C on trial
    !! "test" take projection of C on test
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_4
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_4
END INTERFACE STConvectiveMatrix

!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array
!
! This is a special matrix
! it calls STCM_13a, STCM_13b, STCM_13c, STCM_13d
! it calls STCM_14a, STCM_14b, STCM_14c, STCM_14d

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_5(test, trial, c, crank, &
    & term1, term2, opt) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    !! convective velocity
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    !! convective velocity
    INTEGER(I4B), INTENT(IN) :: term1
    !!
    INTEGER(I4B), INTENT(IN) :: term2
    !!
    INTEGER( I4B ), INTENT( IN ) :: opt(1)
    !! 1 --> v(i) dNTdXt(:,:,j)
    !! 2 --> dNTdXt(:,:,i) v(j)
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_5
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_5
END INTERFACE STConvectiveMatrix

!----------------------------------------------------------------------------
!                                       STConvectiveMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-22
! update: 2021-11-22
! summary: Returns the space-time convective matrix in rank-4 array
!
! This is a special matrix
! it calls STCM_15a, STCM_15b, STCM_15c, STCM_15d
! it calls STCM_16a, STCM_16b, STCM_16c, STCM_16d
! it calls STCM_17a, STCM_17b, STCM_17c, STCM_17d

INTERFACE
  MODULE PURE FUNCTION Mat4_STConvectiveMatrix_6(test, trial, c1, &
    & c2, c1rank, c2rank, term1, term2, opt, projecton) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! scalar FEVariable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! vector FEVariable
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    !! scalar FEvariable
    TYPE(FEVariableVector_), INTENT( IN ) :: c2rank
    !! vector FEVariable
    INTEGER(I4B), INTENT(IN) :: term1
    !!
    INTEGER(I4B), INTENT(IN) :: term2
    !!
    INTEGER( I4B ), INTENT( IN ) :: opt(1)
    !! 1 --> v(i) dNTdXt(:,:,j)
    !! 2 --> dNTdXt(:,:,i) v(j)
    CHARACTER(LEN=*), OPTIONAL, INTENT( IN ) :: projecton
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! returned finite element matrix.
  END FUNCTION Mat4_STConvectiveMatrix_6
END INTERFACE

INTERFACE STConvectiveMatrix
  MODULE PROCEDURE Mat4_STConvectiveMatrix_6
END INTERFACE STConvectiveMatrix

END MODULE STConvectiveMatrix_Method
