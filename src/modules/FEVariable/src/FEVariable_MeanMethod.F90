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
! Returns a constant FEVariable_ with the same rank and `defineOn` as
! `obj`, whose value is the mean of `obj` over all space and time points.
! See [[fevar_Mean2]], [[fevar_Mean3]] and [[fevar_Mean4]].
!
! Examples
!
!```fortran
! TYPE(FEVariable_) :: obj, ans
! obj = NodalVariable(val, TypeFEVariableVector, TypeFEVariableSpace)
! ans = MEAN(obj)
!```
!
! Full example: src/modules/FEVariable/examples/Mean_test_1.F90

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
! Returns `SUM(obj%val(1:len)) / len`, the mean over all space and time
! points of a scalar variable.
!
! Examples
!
!```fortran
! ans = MEAN(obj, TypeFEVariableScalar)
!```
!
! Full example: src/modules/FEVariable/examples/Mean_test_1.F90

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
! Returns the mean vector over all space and time points of a vector
! variable, `SIZE(ans) = obj%s(1)`. For a constant variable the vector
! itself is returned.
!
! Examples
!
!```fortran
! ans = MEAN(obj, TypeFEVariableVector)
!```
!
! Full example: src/modules/FEVariable/examples/Mean_test_1.F90

INTERFACE MEAN
  MODULE PURE FUNCTION fevar_Mean3(obj, dataType) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! vector variable
    TYPE(FEVariableVector_), INTENT(IN) :: dataType
    !! TypeFEVariableVector
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! mean vector, SIZE(ans) = obj%s(1)
  END FUNCTION fevar_Mean3
END INTERFACE MEAN

!----------------------------------------------------------------------------
!                                                           MEAN@MeanMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-27
! summary: Mean of a matrix FEVariable
!
! Returns the mean matrix over all space and time points of a matrix
! variable, `SHAPE(ans) = obj%s(1:2)`. For a constant variable the matrix
! itself is returned.
!
! Changes:
!   - 2026-11-14: the sum is initialised to zero for space, time and
!     space-time variables
!
! Examples
!
!```fortran
! ans = MEAN(obj, TypeFEVariableMatrix)
!```
!
! Full example: src/modules/FEVariable/examples/Mean_test_1.F90

INTERFACE MEAN
  MODULE PURE FUNCTION fevar_Mean4(obj, dataType) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! matrix variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: dataType
    !! TypeFEVariableMatrix
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! mean matrix, SHAPE(ans) = obj%s(1:2)
  END FUNCTION fevar_Mean4
END INTERFACE MEAN

END MODULE FEVariable_MeanMethod
