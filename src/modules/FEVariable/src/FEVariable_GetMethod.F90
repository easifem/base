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
! date: 2021-11-27
! summary: Get methods of FEVariable_
!
! Query the shape, rank, variable type, length and data of a finite
! element variable. Helpers compute the rank, variable type and shape of
! the result of a binary operation between two variables. They take the
! rank case and variable case of the two variables.

MODULE FEVariable_GetMethod
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: FEVariableScalar_
USE BaseType, ONLY: FEVariableVector_
USE BaseType, ONLY: FEVariableMatrix_
USE BaseType, ONLY: FEVariableConstant_
USE BaseType, ONLY: FEVariableSpace_
USE BaseType, ONLY: FEVariableTime_
USE BaseType, ONLY: FEVariableSpaceTime_
USE BaseType, ONLY: TypeFEVariableOpt
USE GlobalData, ONLY: I4B, DFP, LGT
IMPLICIT NONE

PRIVATE

PUBLIC :: SIZE
PUBLIC :: SHAPE
PUBLIC :: GetShape
PUBLIC :: OPERATOR(.rank.)
PUBLIC :: GetRank
PUBLIC :: OPERATOR(.vartype.)
PUBLIC :: GetVarType
PUBLIC :: OPERATOR(.defineon.)
PUBLIC :: GetDefineOn
PUBLIC :: OPERATOR(.len.)
PUBLIC :: GetLen
PUBLIC :: isNodalVariable
PUBLIC :: isQuadratureVariable
PUBLIC :: FEVariable_ToChar
PUBLIC :: FEVariable_ToInteger
PUBLIC :: GetLambdaFromYoungsModulus
PUBLIC :: GetTotalShape
PUBLIC :: GetRankCase
PUBLIC :: GetVarCase
PUBLIC :: Get
PUBLIC :: Get_

!----------------------------------------------------------------------------
!                                                    GetTotalShape@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-02
! summary: Total shape of the result of a binary operation
!
! Returns the number of entries in the shape of a result.
! The result comes from a binary operation between two variables.
! The operation can be `+`, `-`, `*` or `/`.
!
! `tshape1` and `tshape2` are the total shapes of the first and second
! variable. `rankCase` is the code from [[GetRankCase]]. `varCase` is the
! code from [[GetVarCase]].
!
! The result has the rank given by [[GetRank]] and the variable type
! given by [[GetVarType]]. A space variable and a time variable give a
! `spaceTime` variable.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetTotalShape_test_1.F90)
! - [test 2](|base|FEVariable/examples/GetTotalShape_test_2.F90) rank cases
!
! See also: [[GetShape]], [[GetRankCase]], [[GetVarCase]]

INTERFACE GetTotalShape
  MODULE PURE FUNCTION GetTotalShapeFromRankVarCase( &
    rankCase, varCase, tshape1, tshape2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: rankCase
    !! rank case of the two variables, see GetRankCase
    INTEGER(I4B), INTENT(IN) :: varCase
    !! variable case of the two variables, see GetVarCase
    INTEGER(I4B), INTENT(IN) :: tshape1
    !! total shape of the first variable
    INTEGER(I4B), INTENT(IN) :: tshape2
    !! total shape of the second variable
    INTEGER(I4B) :: ans
    !! total shape of the result
  END FUNCTION GetTotalShapeFromRankVarCase
END INTERFACE GetTotalShape

!----------------------------------------------------------------------------
!                                                         GetShape@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-02
! summary: Shape of the result of a binary operation
!
! Returns the shape of the result of a binary operation between two
! variables.
!
! The routine writes the shape to `ans(1:tsize)`. `s1` and `s2` are the
! shapes of the first and second variable. `tsize` is the value that
! [[GetTotalShapeFromRankVarCase]] returns.
!
! The shape lists the components of a vector, or the rows and columns of
! a matrix. The space size follows, then the time size.
!
! @note
! If both variables have the same variable type, they must have the same
! number of space points and time points. The routine does not check this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetShape_test_1.F90)
! - [test 2](|base|FEVariable/examples/GetShape_test_2.F90) each rank case
!
! See also: [[GetTotalShape]], [[GetRankCase]], [[GetVarCase]]

INTERFACE GetShape
  MODULE PURE SUBROUTINE GetShapeFromRankVarCase( &
    rankCase, varCase, s1, s2, tshape1, tshape2, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: rankCase
    !! rank case of the two variables, see GetRankCase
    INTEGER(I4B), INTENT(IN) :: varCase
    !! variable case of the two variables, see GetVarCase
    INTEGER(I4B), INTENT(IN) :: s1(:)
    !! shape of the first variable, SIZE(s1) >= tshape1
    INTEGER(I4B), INTENT(IN) :: s2(:)
    !! shape of the second variable, SIZE(s2) >= tshape2
    INTEGER(I4B), INTENT(IN) :: tshape1
    !! total shape of the first variable
    INTEGER(I4B), INTENT(IN) :: tshape2
    !! total shape of the second variable
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! shape of the result, SIZE(ans) >= tsize (at most 4)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
  END SUBROUTINE GetShapeFromRankVarCase
END INTERFACE GetShape

!----------------------------------------------------------------------------
!                                                       GetVarType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-02
! summary: Variable type of the result of a binary operation
!
! Returns the variable type of the result of a binary operation between
! two variables.
!
! `varCase` is the code from [[GetVarCase]]. The result is:
!
! - constant for 00
! - space for 01, 10 and 11
! - time for 02, 20 and 22
! - `spaceTime` for 03, 13, 23, 30, 31, 32, 33, 12 and 21
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetVarType_test_1.F90) with var cases
! - [test 2](|base|FEVariable/examples/GetVarType_test_2.F90) each case
!
! See also: [[GetVarCase]], [[GetRank]]

INTERFACE GetVarType
  MODULE PURE FUNCTION GetVarTypeFromVarCase(varCase) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: varCase
    !! variable case of two variables, see GetVarCase
    INTEGER(I4B) :: ans
    !! variable type, `TypeFEVariableOpt%constant`, space, time or spacetime
  END FUNCTION GetVarTypeFromVarCase
END INTERFACE GetVarType

!----------------------------------------------------------------------------
!                                                          GetRank@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-02
! summary: Rank of the result of a binary operation
!
! Returns the rank of the result of a binary operation between two
! variables.
!
! `rankCase` is the code from [[GetRankCase]]. The result is:
!
! - scalar for 00
! - vector for 01, 10 and 11
! - matrix for 02, 20, 22, 12 and 21
!
! Changes:
!   - 2026-11-14: rank cases 12 and 21 (vector and matrix) give matrix,
!     as in GetShape and GetTotalShape
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetRank_test_1.F90) with rank cases
!
! See also: [[GetRankCase]], [[GetVarType]]

INTERFACE GetRank
  MODULE PURE FUNCTION GetRankFromRankCase(rankCase) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: rankCase
    !! rank case of two variables, see GetRankCase
    INTEGER(I4B) :: ans
    !! rank, `TypeFEVariableOpt%scalar`, vector or matrix
  END FUNCTION GetRankFromRankCase
END INTERFACE GetRank

!----------------------------------------------------------------------------
!                                                      GetRankCase@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-29
! summary: Rank case of two variables
!
! Returns a two-digit code for the ranks of two variables.
!
! The code is `10 * a + b`. Here `a` is the code of `rank1` and `b` is
! the code of `rank2`. The codes are 0 for scalar, 1 for vector and 2 for
! matrix. For example, a vector and a scalar give 10.
!
! The order of the arguments matters. The code 12 differs from 21.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetRankCase_test_1.F90)
!
! See also: [[GetRank]], [[GetVarCase]]

INTERFACE
  MODULE PURE FUNCTION GetRankCase(rank1, rank2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: rank1
    !! rank of the first variable, `TypeFEVariableOpt%scalar`, ...
    INTEGER(I4B), INTENT(IN) :: rank2
    !! rank of the second variable
    INTEGER(I4B) :: ans
    !! rank case, 00 to 22
  END FUNCTION GetRankCase
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetVarCase@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-29
! summary: Variable case of two variables
!
! Returns a two-digit code for the variable types of two variables.
!
! The code is `10 * a + b`. Here `a` is the code of `vartype1` and `b`
! is the code of `vartype2`. The codes are 0 for constant, 1 for space,
! 2 for time and 3 for `spaceTime`. For example, a space variable and a
! time variable give 12.
!
! The order of the arguments matters. The code 21 differs from 12.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetVarCase_test_1.F90)
! - [test 2](|base|FEVariable/examples/GetVarCase_test_2.F90) all 16 pairs
!
! See also: [[GetVarType]], [[GetRankCase]]

INTERFACE
  MODULE PURE FUNCTION GetVarCase(vartype1, vartype2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: vartype1
    !! variable type of the first variable, `TypeFEVariableOpt%space`, ...
    INTEGER(I4B), INTENT(IN) :: vartype2
    !! variable type of the second variable
    INTEGER(I4B) :: ans
    !! variable case, 00 to 33
  END FUNCTION GetVarCase
END INTERFACE

!----------------------------------------------------------------------------
!                                       GetLambdaFromYoungsModulus@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-12
! summary: Lame parameter lambda from Young's and shear modulus
!
! Computes the Lame parameter `lambda` from Young's modulus and the
! shear modulus.
!
! `lambda` gets the shape, rank and variable type of `youngsModulus`.
! Each value of `lambda` uses the values at the same position in
! `youngsModulus` and `shearModulus`.
!
! @warning
! `youngsModulus` and `shearModulus` must have the same shape, rank,
! variable type and `len`. For example, a constant `youngsModulus` with a
! `shearModulus` that varies in space is not supported. The routine does
! not check this. It reads out of bounds when `shearModulus` is shorter.
! @endwarning
!
! Formula:
!
! $$ \lambda = \frac{\mu (E - 2 \mu)}{3 \mu - E} $$
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetLambdaFromYoungsModulus_test_1.F90)

INTERFACE GetLambdaFromYoungsModulus
  MODULE PURE SUBROUTINE fevar_GetLambdaFromYoungsModulus( &
    youngsModulus, shearModulus, lambda)
    TYPE(FEVariable_), INTENT(IN) :: youngsModulus
    !! Young's modulus E
    TYPE(FEVariable_), INTENT(IN) :: shearModulus
    !! shear modulus mu, same shape, rank, vartype as `youngsModulus`
    TYPE(FEVariable_), INTENT(INOUT) :: lambda
    !! Lame parameter lambda
  END SUBROUTINE fevar_GetLambdaFromYoungsModulus
END INTERFACE GetLambdaFromYoungsModulus

!----------------------------------------------------------------------------
!                                                FEVariable_ToChar@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary: Converts scalar, vector, matrix to string name
!
! Returns the name of a rank as a string.
!
! The names are "Scalar", "Vector" and "Matrix". They match
! `TypeFEVariableOpt%scalar`, `TypeFEVariableOpt%vector` and
! `TypeFEVariableOpt%matrix`. Any other value of `name` gives "Scalar".
!
! If `isUpper` is true, the name is in upper case.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/FEVariable_ToChar_test_1.F90)
!
! See also: [[FEVariable_ToInteger]]

INTERFACE
  MODULE PURE FUNCTION FEVariable_ToChar(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    !! rank, `TypeFEVariableOpt%scalar`, vector or matrix
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isUpper
    !! if true, return the name in upper case; default false
    CHARACTER(:), ALLOCATABLE :: ans
    !! name of the rank
  END FUNCTION FEVariable_ToChar
END INTERFACE

!----------------------------------------------------------------------------
!                                             FEVariable_ToInteger@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary: Converts scalar, vector, matrix name to integer
!
! Returns the rank that matches a rank name.
!
! The function reads only the first character of `name`, in any case.
! The characters "s", "v" and "m" give `TypeFEVariableOpt%scalar`,
! `TypeFEVariableOpt%vector` and `TypeFEVariableOpt%matrix`.
! Any other first character gives `TypeFEVariableOpt%scalar`.
!
! @note
! `name` must not be empty.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/FEVariable_ToInteger_test_1.F90)
!
! See also: [[FEVariable_ToChar]]

INTERFACE
  MODULE PURE FUNCTION FEVariable_ToInteger(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    !! name of the rank, "Scalar", "Vector", "Matrix"; LEN(name) >= 1
    INTEGER(I4B) :: ans
    !! rank, `TypeFEVariableOpt%scalar`, vector or matrix
  END FUNCTION FEVariable_ToInteger
END INTERFACE

!----------------------------------------------------------------------------
!                                                             SIZE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-12
! summary: Returns the size of variable
!
! Returns the size of `obj` along one dimension, or the total number of
! values.
!
! If `dim` is present, it returns `obj%s(dim)`.
! Otherwise it returns `obj%len`, the total number of values.
!
! @warning
! `dim` is not checked. It must not exceed the total shape of `obj`.
! The total shape is 1 for a scalar that is constant, space or time.
! It is 2 for a scalar `spaceTime` or a vector that is space or time.
! See [[GetTotalShape]] for the other cases.
! @endwarning
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Size_test_1.F90)
!
! See also: [[GetLen]], [[SHAPE]]

INTERFACE Size
  MODULE PURE FUNCTION fevar_Size(obj, Dim) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Dim
    !! dimension, 1 <= dim <= `obj%tshape`
    INTEGER(I4B) :: ans
    !! `obj%s(dim)` if `dim` is present, else `obj%len`
  END FUNCTION fevar_Size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                            SHAPE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-12
! summary: Returns the shape of data
!
! Returns the shape of `obj` as an allocated array.
! The array is `obj%s(1:obj%tshape)`. Its size depends on the rank and
! the variable type of `obj`, as the table shows.
!
!| rank | vartype | ans |
!| --- | --- | ---  |
!| Scalar | Constant | `[1]` |
!| Scalar | Space, Time | `[s(1)]` |
!| Scalar | SpaceTime | `[s(1), s(2)]` |
!| Vector | Constant | `[s(1)]` |
!| Vector | Space, Time | `[s(1), s(2)]` |
!| Vector | SpaceTime | `[s(1), s(2), s(3)]` |
!| Matrix | Constant | `[s(1), s(2)]` |
!| Matrix | Space, Time | `[s(1), s(2), s(3)]` |
!| Matrix | SpaceTime | `[s(1), s(2), s(3), s(4)]` |
!
! Here `s` stands for `obj%s`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Shape_test_1.F90)
!
! See also: [[fevar_GetShape]], [[GetTotalShape]]

INTERFACE Shape
  MODULE PURE FUNCTION fevar_Shape(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! shape of data, `SIZE(ans) = obj%tshape`
  END FUNCTION fevar_Shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                         GetShape@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary: Returns the shape of data without allocation
!
! Writes the shape of `obj` to `ans(1:tsize)`. The value of `tsize` is
! `obj%tshape`.
!
! This gives the same shape as [[fevar_Shape]]. It does not allocate
! memory, so `ans` must be large enough.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetShape_test_1.F90)
!
! See also: [[fevar_Shape]], [[GetTotalShape]]

INTERFACE GetShape
  MODULE PURE SUBROUTINE fevar_GetShape(obj, ans, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! shape of data, `SIZE(ans) >= obj%tshape`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in `ans`, equal to `obj%tshape`
  END SUBROUTINE fevar_GetShape
END INTERFACE GetShape

!----------------------------------------------------------------------------
!                                                    GetTotalShape@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-03
! summary: Returns the total size of shape of data
!
! Returns the number of entries in the shape of `obj`, that is
! `obj%tshape`. The number depends on the rank and the variable type.
!
!| rank | vartype | ans |
!| --- | --- | ---  |
!| Scalar | Constant | 1 |
!| Scalar | Space, Time | 1 |
!| Scalar | SpaceTime | 2 |
!| Vector | Constant | 1 |
!| Vector | Space, Time | 2 |
!| Vector | SpaceTime | 3 |
!| Matrix | Constant | 2 |
!| Matrix | Space, Time | 3 |
!| Matrix | SpaceTime | 4 |
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetTotalShape_test_1.F90)
!
! See also: [[SHAPE]], [[GetShape]], [[GetTotalShapeFromRankVarCase]]

INTERFACE GetTotalShape
  MODULE PURE FUNCTION fevar_GetTotalShape(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B) :: ans
    !! total shape, `obj%tshape`
  END FUNCTION fevar_GetTotalShape
END INTERFACE GetTotalShape

!----------------------------------------------------------------------------
!                                                          GetRank@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the rank of FEVariable
!
! Returns the rank of `obj`, that is `obj%rank`.
! The result is one of `TypeFEVariableOpt%scalar`,
! `TypeFEVariableOpt%vector` and `TypeFEVariableOpt%matrix`.
! You can also use the operator `.rank.`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetRank_test_1.F90)
!
! See also: [[GetVarType]], [[GetDefineOn]]

INTERFACE OPERATOR(.RANK.)
  MODULE PURE FUNCTION fevar_rank(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B) :: ans
    !! rank of `obj`
  END FUNCTION fevar_rank
END INTERFACE OPERATOR(.RANK.)

INTERFACE GetRank
  MODULE PROCEDURE fevar_rank
END INTERFACE GetRank

!----------------------------------------------------------------------------
!                                                       GetVarType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the vartype of FEVariable
!
! Returns the variable type of `obj`, that is `obj%vartype`.
! The result is one of `TypeFEVariableOpt%constant`,
! `TypeFEVariableOpt%space`, `TypeFEVariableOpt%time` and
! `TypeFEVariableOpt%spacetime`.
! You can also use the operator `.vartype.`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetVarType_test_1.F90)
!
! See also: [[GetRank]], [[GetDefineOn]]

INTERFACE OPERATOR(.vartype.)
  MODULE PURE FUNCTION fevar_vartype(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B) :: ans
    !! variable type of `obj`
  END FUNCTION fevar_vartype
END INTERFACE OPERATOR(.vartype.)

INTERFACE GetVarType
  MODULE PROCEDURE fevar_vartype
END INTERFACE GetVarType

!----------------------------------------------------------------------------
!                                                      GetDefineOn@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the defineon of FEVariable
!
! Returns where `obj` holds its values, that is `obj%defineon`.
! The result is `TypeFEVariableOpt%nodal` or `TypeFEVariableOpt%quadrature`.
! You can also use the operator `.defineon.`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetDefineOn_test_1.F90)
!
! See also: [[IsNodalVariable]], [[IsQuadratureVariable]]

INTERFACE OPERATOR(.defineon.)
  MODULE PURE FUNCTION fevar_defineon(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B) :: ans
    !! where `obj` holds its values, nodal or quadrature
  END FUNCTION fevar_defineon
END INTERFACE OPERATOR(.defineon.)

INTERFACE GetDefineOn
  MODULE PROCEDURE fevar_defineon
END INTERFACE GetDefineOn

!----------------------------------------------------------------------------
!                                                           GetLen@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the length of data in FEVariable
!
! Returns the number of values stored in `obj`, that is `obj%len`.
! You can also use the operator `.len.`.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/GetLen_test_1.F90)
!
! See also: [[Size]]

INTERFACE OPERATOR(.len.)
  MODULE PURE FUNCTION fevar_len(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B) :: ans
    !! length of data, `obj%len`
  END FUNCTION fevar_len
END INTERFACE OPERATOR(.len.)

INTERFACE GetLen
  MODULE PROCEDURE fevar_len
END INTERFACE GetLen

!----------------------------------------------------------------------------
!                                                  IsNodalVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns true if FEVariable is defined on nodes
!
! Returns true if `obj` holds values at nodes.
! It returns false if `obj` holds values at quadrature points.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/IsNodalVariable_test_1.F90)
!
! See also: [[IsQuadratureVariable]]

INTERFACE IsNodalVariable
  MODULE PURE FUNCTION fevar_IsNodalVariable(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    LOGICAL(LGT) :: ans
    !! true if `obj` is a nodal variable
  END FUNCTION fevar_IsNodalVariable
END INTERFACE IsNodalVariable

!----------------------------------------------------------------------------
!                                             IsQuadratureVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns true if FEVariable is defined on quadrature points
!
! Returns true if `obj` holds values at quadrature points.
! It returns false if `obj` holds values at nodes.
!
! @note
! The test is "not nodal". An object that is not initiated also gives
! true.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/IsQuadratureVariable_test_1.F90)
!
! See also: [[IsNodalVariable]]

INTERFACE IsQuadratureVariable
  MODULE PURE FUNCTION fevar_IsQuadratureVariable(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    LOGICAL(LGT) :: ans
    !! true if `obj` is not a nodal variable
  END FUNCTION fevar_IsQuadratureVariable
END INTERFACE IsQuadratureVariable

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is scalar, constant
!
! Returns the single value stored in a scalar, constant variable.
! This is `obj%val(1)`.
!
! @note
! `obj` must be a scalar, constant variable. The routine does not
! check this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get_test_1.F90)
!
! See also: [[Get_]]

INTERFACE Get
  MODULE PURE FUNCTION Scalar_Constant(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! `TypeFEVariableScalar`
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! `TypeFEVariableConstant`
    REAL(DFP) :: val
    !! the constant value, `obj%val(1)`
  END FUNCTION Scalar_Constant
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is scalar, space
!
! Returns the values of a scalar variable that varies in space.
! The result is an allocated vector of size `nns`, the number of nodes
! in space. This size is `obj%s(1)`.
!
! @note
! `obj` must be a scalar, space variable. The routine does not check
! this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get_test_1.F90)
!
! See also: [[Scalar_Space_]]

INTERFACE Get
  MODULE PURE FUNCTION Scalar_Space(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! `TypeFEVariableScalar`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! `TypeFEVariableSpace`
    REAL(DFP), ALLOCATABLE :: val(:)
    !! values, `SHAPE(val) = [nns]`
  END FUNCTION Scalar_Space
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is scalar, space without allocation
!
! Copies the values of a scalar, space variable into `val(1:tsize)`.
! It does not allocate `val`.
!
! @note
! `obj` must be a scalar, space variable. `val` must hold at least
! `nns` values. The routine checks neither condition.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get__test_1.F90)
!
! See also: [[Scalar_Space]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Scalar_Space_(obj, rank, vartype, val, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! `TypeFEVariableScalar`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! `TypeFEVariableSpace`
    REAL(DFP), INTENT(INOUT) :: val(:)
    !! values, `SIZE(val)` at least `nns`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written, `obj%len`
  END SUBROUTINE Scalar_Space_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is scalar, time
!
! Returns the values of a scalar variable that varies in time.
! The result is an allocated vector of size `nnt`, the number of nodes
! in time. This size is `obj%s(1)`.
!
! @note
! `obj` must be a scalar, time variable. The routine does not check
! this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get_test_1.F90)
!
! See also: [[Scalar_Time_]]

INTERFACE Get
  MODULE PURE FUNCTION Scalar_Time(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! `TypeFEVariableScalar`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! `TypeFEVariableTime`
    REAL(DFP), ALLOCATABLE :: val(:)
    !! values, `SHAPE(val) = [nnt]`
  END FUNCTION Scalar_Time
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is scalar, time without allocation
!
! Copies the values of a scalar, time variable into `val(1:tsize)`.
! It does not allocate `val`.
!
! @note
! `obj` must be a scalar, time variable. `val` must hold at least
! `nnt` values. The routine checks neither condition.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get__test_1.F90)
!
! See also: [[Scalar_Time]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Scalar_Time_(obj, rank, vartype, val, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! `TypeFEVariableScalar`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! `TypeFEVariableTime`
    REAL(DFP), INTENT(INOUT) :: val(:)
    !! values, `SIZE(val)` at least `nnt`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written, `obj%len`
  END SUBROUTINE Scalar_Time_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is scalar, spaceTime
!
! Returns the values of a scalar variable that varies in space and
! time.
! The result is an allocated matrix of shape `[nns, nnt]`. Here `nns`
! is the number of nodes in space and `nnt` the number in time. They
! are `obj%s(1)` and `obj%s(2)`.
!
! @note
! `obj` must be a scalar, `spaceTime` variable. The routine does not
! check this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get_test_1.F90)
!
! See also: [[Scalar_SpaceTime_]]

INTERFACE Get
  MODULE PURE FUNCTION Scalar_SpaceTime(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! `TypeFEVariableScalar`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! `TypeFEVariableSpaceTime`
    REAL(DFP), ALLOCATABLE :: val(:, :)
    !! values, `SHAPE(val) = [nns, nnt]`
  END FUNCTION Scalar_SpaceTime
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is scalar, spaceTime without allocation
!
! Copies the values of a scalar, `spaceTime` variable into
! `val(1:nrow, 1:ncol)`.
! It does not allocate `val`.
!
! @note
! `obj` must be a scalar, `spaceTime` variable. `val` must have at least
! `nns` rows and `nnt` columns. The routine checks neither condition.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get__test_1.F90)
!
! See also: [[Scalar_SpaceTime]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Scalar_SpaceTime_( &
    obj, rank, vartype, val, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! `TypeFEVariableScalar`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! `TypeFEVariableSpaceTime`
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    !! values, shape at least `[nns, nnt]`
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written, `nns`
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written, `nnt`
  END SUBROUTINE Scalar_SpaceTime_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is vector, constant
!
! Returns the components of a constant vector variable.
! The result is an allocated vector of size `ncomp`, the number of
! components. This size is `obj%s(1)`.
!
! @note
! `obj` must be a vector, constant variable. The routine does not
! check this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get_test_1.F90)
!
! See also: [[Vector_Constant_]]

INTERFACE Get
  MODULE PURE FUNCTION Vector_Constant(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! `TypeFEVariableVector`
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! `TypeFEVariableConstant`
    REAL(DFP), ALLOCATABLE :: val(:)
    !! values, `SHAPE(val) = [ncomp]`
  END FUNCTION Vector_Constant
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is vector, constant without allocation
!
! Copies the components of a constant vector variable into
! `val(1:tsize)`.
! It does not allocate `val`.
!
! @note
! `obj` must be a vector, constant variable. `val` must hold at least
! `ncomp` values. The routine checks neither condition.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get__test_1.F90)
!
! See also: [[Vector_Constant]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Vector_Constant_(obj, rank, vartype, val, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! `TypeFEVariableVector`
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! `TypeFEVariableConstant`
    REAL(DFP), INTENT(INOUT) :: val(:)
    !! values, `SIZE(val)` at least `ncomp`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written, `obj%len`
  END SUBROUTINE Vector_Constant_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is vector, space
!
! Returns the values of a vector variable that varies in space.
! The result is an allocated matrix of shape `[ncomp, nns]`. Here
! `ncomp` is the number of components and `nns` the number of nodes in
! space. They are `obj%s(1)` and `obj%s(2)`.
!
! @note
! `obj` must be a vector, space variable. The routine does not check
! this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get_test_1.F90)
!
! See also: [[Vector_Space_]]

INTERFACE Get
  MODULE PURE FUNCTION Vector_Space(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! `TypeFEVariableVector`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! `TypeFEVariableSpace`
    REAL(DFP), ALLOCATABLE :: val(:, :)
    !! values, `SHAPE(val) = [ncomp, nns]`
  END FUNCTION Vector_Space
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is vector, space without allocation
!
! Copies the values of a vector variable that varies in space into
! `val(1:nrow, 1:ncol)`.
! It does not allocate `val`.
! Here `nrow` is the number of components `ncomp`.
! Also `ncol` is the number of nodes in space `nns`.
!
! @note
! `obj` must be a vector, space variable. `val` must hold at least
! `ncomp` rows and `nns` columns. The routine checks neither condition.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get__test_1.F90)
!
! See also: [[Vector_Space]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Vector_Space_(obj, rank, vartype, val, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! `TypeFEVariableVector`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! `TypeFEVariableSpace`
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    !! values, `SHAPE(val)` at least `[ncomp, nns]`
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written, `ncomp`
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written, `nns`
  END SUBROUTINE Vector_Space_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is vector, time
!
! Returns the values of a vector variable that varies in time.
! The result is an allocated matrix of shape `[ncomp, nnt]`. Here
! `ncomp` is the number of components and `nnt` the number of nodes in
! time. They are `obj%s(1)` and `obj%s(2)`.
!
! @note
! `obj` must be a vector, time variable. The routine does not check
! this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get_test_1.F90)
!
! See also: [[Vector_Time_]]

INTERFACE Get
  MODULE PURE FUNCTION Vector_Time(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! `TypeFEVariableVector`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! `TypeFEVariableTime`
    REAL(DFP), ALLOCATABLE :: val(:, :)
    !! values, `SHAPE(val) = [ncomp, nnt]`
  END FUNCTION Vector_Time
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is vector, time without allocation
!
! Copies the values of a vector variable that varies in time into
! `val(1:nrow, 1:ncol)`.
! It does not allocate `val`.
! Here `nrow` is the number of components `ncomp`.
! Also `ncol` is the number of nodes in time `nnt`.
!
! @note
! `obj` must be a vector, time variable. `val` must hold at least
! `ncomp` rows and `nnt` columns. The routine checks neither condition.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get__test_1.F90)
!
! See also: [[Vector_Time]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Vector_Time_(obj, rank, vartype, val, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! `TypeFEVariableVector`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! `TypeFEVariableTime`
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    !! values, `SHAPE(val)` at least `[ncomp, nnt]`
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written, `ncomp`
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written, `nnt`
  END SUBROUTINE Vector_Time_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is vector, spaceTime
!
! Returns the values of a vector variable that varies in space and time.
! The result is an allocated array of shape `[ncomp, nns, nnt]`.
! Here `ncomp` is the number of components.
! Also `nns` and `nnt` are the numbers of nodes in space and in time.
! They are `obj%s(1)`, `obj%s(2)` and `obj%s(3)`.
!
! @note
! `obj` must be a vector, `spaceTime` variable. The routine does not
! check this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get_test_1.F90)
!
! See also: [[Vector_SpaceTime_]]

INTERFACE Get
  MODULE PURE FUNCTION Vector_SpaceTime(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! `TypeFEVariableVector`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! `TypeFEVariableSpaceTime`
    REAL(DFP), ALLOCATABLE :: val(:, :, :)
    !! values, `SHAPE(val) = [ncomp, nns, nnt]`
  END FUNCTION Vector_SpaceTime
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is vector, spaceTime without allocation
!
! Copies the values of a vector variable that varies in space and time
! into `val(1:dim1, 1:dim2, 1:dim3)`.
! It does not allocate `val`.
! The extents are `dim1 = ncomp`, `dim2 = nns` and `dim3 = nnt`.
! Here `ncomp` is the number of components.
! Also `nns` and `nnt` are the numbers of nodes in space and in time.
!
! @note
! `obj` must be a vector, `spaceTime` variable. `val` must be at least
! `[ncomp, nns, nnt]` in shape. The routine checks neither condition.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get__test_1.F90)
!
! See also: [[Vector_SpaceTime]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Vector_SpaceTime_( &
    obj, rank, vartype, val, dim1, dim2, dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! `TypeFEVariableVector`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! `TypeFEVariableSpaceTime`
    REAL(DFP), INTENT(INOUT) :: val(:, :, :)
    !! values, `SHAPE(val)` at least `[ncomp, nns, nnt]`
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent written in the first dimension, `ncomp`
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent written in the second dimension, `nns`
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent written in the third dimension, `nnt`
  END SUBROUTINE Vector_SpaceTime_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is matrix, constant
!
! Returns the values of a constant matrix variable.
! The result is an allocated matrix of shape `[nrow, ncol]`.
! These extents are `obj%s(1)` and `obj%s(2)`.
!
! @note
! `obj` must be a matrix, constant variable. The routine does not check
! this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get_test_1.F90)
!
! See also: [[Matrix_Constant_]]

INTERFACE Get
  MODULE PURE FUNCTION Matrix_Constant(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! `TypeFEVariableMatrix`
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! `TypeFEVariableConstant`
    REAL(DFP), ALLOCATABLE :: val(:, :)
    !! values, `SHAPE(val) = [nrow, ncol]`
  END FUNCTION Matrix_Constant
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is matrix, constant without allocation
!
! Copies the values of a constant matrix variable into
! `val(1:nrow, 1:ncol)`.
! It does not allocate `val`.
! The extents `nrow` and `ncol` are `obj%s(1)` and `obj%s(2)`.
!
! @note
! `obj` must be a matrix, constant variable. `val` must be large enough
! to hold `nrow` rows and `ncol` columns. The routine checks neither
! condition.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get__test_1.F90)
!
! See also: [[Matrix_Constant]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Matrix_Constant_(obj, rank, vartype, val, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! `TypeFEVariableMatrix`
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! `TypeFEVariableConstant`
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    !! values, `SHAPE(val)` at least `[nrow, ncol]`
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written
  END SUBROUTINE Matrix_Constant_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is matrix, space
!
! Returns the values of a matrix variable that varies in space.
! The result is an allocated array of shape `[nrow, ncol, nns]`.
! Here `nns` is the number of nodes in space.
! The extents are `obj%s(1)`, `obj%s(2)` and `obj%s(3)`.
!
! @note
! `obj` must be a matrix, space variable. The routine does not check
! this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get_test_1.F90)
!
! See also: [[Matrix_Space_]]

INTERFACE Get
  MODULE PURE FUNCTION Matrix_Space(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! `TypeFEVariableMatrix`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! `TypeFEVariableSpace`
    REAL(DFP), ALLOCATABLE :: val(:, :, :)
    !! values, `SHAPE(val) = [nrow, ncol, nns]`
  END FUNCTION Matrix_Space
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is matrix, space without allocation
!
! Copies the values of a matrix variable that varies in space into
! `val(1:dim1, 1:dim2, 1:dim3)`.
! It does not allocate `val`.
! The extents are `dim1 = nrow`, `dim2 = ncol` and `dim3 = nns`.
! Here `nns` is the number of nodes in space.
!
! @note
! `obj` must be a matrix, space variable. `val` must be at least
! `[nrow, ncol, nns]` in shape. The routine checks neither condition.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get__test_1.F90)
!
! See also: [[Matrix_Space]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Matrix_Space_( &
    obj, rank, vartype, val, dim1, dim2, dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! `TypeFEVariableMatrix`
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! `TypeFEVariableSpace`
    REAL(DFP), INTENT(INOUT) :: val(:, :, :)
    !! values, `SHAPE(val)` at least `[nrow, ncol, nns]`
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent written in the first dimension, `nrow`
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent written in the second dimension, `ncol`
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent written in the third dimension, `nns`
  END SUBROUTINE Matrix_Space_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is matrix, time
!
! Returns the values of a matrix variable that varies in time.
! The result is an allocated array of shape `[nrow, ncol, nnt]`.
! Here `nnt` is the number of nodes in time.
! The extents are `obj%s(1)`, `obj%s(2)` and `obj%s(3)`.
!
! @note
! `obj` must be a matrix, time variable. The routine does not check
! this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get_test_1.F90)
!
! See also: [[Matrix_Time_]]

INTERFACE Get
  MODULE PURE FUNCTION Matrix_Time(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! `TypeFEVariableMatrix`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! `TypeFEVariableTime`
    REAL(DFP), ALLOCATABLE :: val(:, :, :)
    !! values, `SHAPE(val) = [nrow, ncol, nnt]`
  END FUNCTION Matrix_Time
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is matrix, time without allocation
!
! Copies the values of a matrix variable that varies in time into the
! array `val`.
! The routine writes the values in `val(1:dim1, 1:dim2, 1:dim3)`.
! These extents are `[nrow, ncol, nnt]`.
! Here `nnt` is the number of nodes in time.
!
! @note
! `obj` must be a matrix, time variable. `val` must be at least
! `[nrow, ncol, nnt]` in shape. The routine checks neither condition.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get__test_1.F90)
!
! See also: [[Matrix_Time]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Matrix_Time_( &
    obj, rank, vartype, val, dim1, dim2, dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! `TypeFEVariableMatrix`
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! `TypeFEVariableTime`
    REAL(DFP), INTENT(INOUT) :: val(:, :, :)
    !! values, `SHAPE(val)` at least `[nrow, ncol, nnt]`
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent written in the first dimension, `nrow`
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent written in the second dimension, `ncol`
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent written in the third dimension, `nnt`
  END SUBROUTINE Matrix_Time_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is matrix, `spaceTime`
!
! Returns the values of a matrix variable that varies in space and time.
! The result is an allocated array of shape `[nrow, ncol, nns, nnt]`.
! Here `nns` and `nnt` are the numbers of nodes in space and in time.
! The extents are `obj%s(1)` to `obj%s(4)`.
!
! @note
! `obj` must be a matrix, space-time variable. The routine does not
! check this.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get_test_1.F90)
!
! See also: [[Matrix_SpaceTime_]]

INTERFACE Get
  MODULE PURE FUNCTION Matrix_SpaceTime(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! `TypeFEVariableMatrix`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! `TypeFEVariableSpaceTime`
    REAL(DFP), ALLOCATABLE :: val(:, :, :, :)
    !! values, `SHAPE(val) = [nrow, ncol, nns, nnt]`
  END FUNCTION Matrix_SpaceTime
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is matrix, `spaceTime` without allocation
!
! Copies the values of a matrix variable that varies in space and time
! into the array `val`.
! The routine writes the values in `val(1:dim1, 1:dim2, 1:dim3, 1:dim4)`.
! These extents are `[nrow, ncol, nns, nnt]`.
! Here `nns` and `nnt` are the numbers of nodes in space and in time.
!
! @note
! `obj` must be a matrix, space-time variable. `val` must be at least
! `[nrow, ncol, nns, nnt]` in shape. The routine checks neither
! condition.
! @endnote
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Get__test_1.F90)
!
! See also: [[Matrix_SpaceTime]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Matrix_SpaceTime_( &
    obj, rank, vartype, val, dim1, dim2, dim3, dim4)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! `TypeFEVariableMatrix`
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! `TypeFEVariableSpaceTime`
    REAL(DFP), INTENT(INOUT) :: val(:, :, :, :)
    !! values, `SHAPE(val)` at least `[nrow, ncol, nns, nnt]`
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent written in the first dimension, `nrow`
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent written in the second dimension, `ncol`
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent written in the third dimension, `nns`
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! extent written in the fourth dimension, `nnt`
  END SUBROUTINE Matrix_SpaceTime_
END INTERFACE Get_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_GetMethod
