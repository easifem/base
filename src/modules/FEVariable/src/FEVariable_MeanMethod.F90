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

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-27
! summary: Mean of FEVariable_ over space and time points

MODULE FEVariable_MeanMethod
USE BaseType, ONLY: FEVariable_, &
                    FEVariableScalar_, &
                    FEVariableVector_, &
                    FEVariableMatrix_, &
                    FEVariableConstant_, &
                    FEVariableSpace_, &
                    FEVariableTime_, &
                    FEVariableSpaceTime_, &
                    TypeFEVariableOpt

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: MEAN

!----------------------------------------------------------------------------
!                                                          MEAN@MeanMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-27
! summary: FEVariable = Mean( obj )
!
! Returns the mean of `obj` as a constant `FEVariable_`.
! The mean averages over all space and time points.
!
! The result has the same rank and `defineOn` as `obj`.
! The rank can be scalar, vector or matrix.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Mean_test_1.F90) with scalar and
!   vector variables
! - [test 2](|base|FEVariable/examples/Mean_test_2.F90) with matrix
!   variables and constant input
!
! See also: [[fevar_Mean2]], [[fevar_Mean3]], [[fevar_Mean4]]

INTERFACE MEAN
  MODULE PURE FUNCTION fevar_Mean1(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariable_) :: ans
    !! constant variable with the mean value
  END FUNCTION fevar_Mean1
END INTERFACE MEAN

!----------------------------------------------------------------------------
!                                                            MEAN@MeanMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-27
! summary: Mean of a scalar FEVariable
!
! Returns the mean of a scalar variable as a real number.
! The mean is `SUM(obj%val(1:obj%len)) / obj%len`.
! It covers all space and time points.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Mean_test_1.F90)
!
! See also: [[fevar_Mean1]]

INTERFACE MEAN
  MODULE PURE FUNCTION fevar_Mean2(obj, dataType) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! scalar variable
    TYPE(FEVariableScalar_), INTENT(IN) :: dataType
    !! TypeFEVariableScalar
    REAL(DFP) :: ans
    !! mean value
  END FUNCTION fevar_Mean2
END INTERFACE MEAN

!----------------------------------------------------------------------------
!                                                            MEAN@MeanMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-27
! summary: Mean of a vector FEVariable
!
! Returns the mean vector of a vector variable.
! The mean averages over all space and time points.
! The result has size `obj%s(1)`.
!
! For a constant variable, it returns the vector itself.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Mean_test_1.F90)
!
! See also: [[fevar_Mean1]]

INTERFACE MEAN
  MODULE PURE FUNCTION fevar_Mean3(obj, dataType) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! vector variable
    TYPE(FEVariableVector_), INTENT(IN) :: dataType
    !! TypeFEVariableVector
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! mean vector, `SIZE(ans) = obj%s(1)`
  END FUNCTION fevar_Mean3
END INTERFACE MEAN

!----------------------------------------------------------------------------
!                                                           MEAN@MeanMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-27
! summary: Mean of a matrix FEVariable
!
! Returns the mean matrix of a matrix variable.
! The mean averages over all space and time points.
! The result has shape `obj%s(1:2)`.
!
! For a constant variable, it returns the matrix itself.
!
! Changes:
!   - 2026-11-14: Starts the sum from zero for space, time and
!     space-time variables
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Mean_test_1.F90) with 2x2 matrices
! - [test 2](|base|FEVariable/examples/Mean_test_2.F90) with 2x3 matrices
!
! See also: [[fevar_Mean1]]

INTERFACE MEAN
  MODULE PURE FUNCTION fevar_Mean4(obj, dataType) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! matrix variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: dataType
    !! TypeFEVariableMatrix
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! mean matrix, `SHAPE(ans) = obj%s(1:2)`
  END FUNCTION fevar_Mean4
END INTERFACE MEAN

END MODULE FEVariable_MeanMethod
