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
! element variable. Also contains helpers that compute the rank, variable
! type and shape of the result of a binary operation between two
! variables from their rank case and variable case.

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
! Returns the number of dimensions of the data that results from a binary
! operation (`+`, `-`, `*`, `/`) between two variables. The first
! variable has total shape `tshape1`, the second `tshape2`. `rankCase`
! is given by [[GetRankCase]] and `varCase` by [[GetVarCase]].
!
! The result follows the rank of the higher rank operand and the variable
! type given by [[GetVarType]] of `varCase`. A space and a time variable
! (varCase 12 or 21) give a space-time variable.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Constant => TypeFEVariableConstant
! USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
! USE BaseType, ONLY: Vector => TypeFEVariableVector
! USE FEVariable_Method, ONLY: GetTotalShape
! USE FEVariable_Method, ONLY: NodalVariable
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: total shape of one variable
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   obj = NodalVariable(1.0_DFP, Scalar, Constant)
!   CALL OK(GetTotalShape(obj) .EQ. 1, "scalar constant: 1")
!   obj = NodalVariable(nrow=3, ncol=2, rank=Scalar, vartype=SpaceTime)
!   CALL OK(GetTotalShape(obj) .EQ. 2, "scalar spaceTime: 2")
!   obj = NodalVariable(nrow=3, ncol=4, rank=Vector, vartype=Space)
!   CALL OK(GetTotalShape(obj) .EQ. 2, "vector space: 2")
!   obj = NodalVariable(dim1=2, dim2=2, dim3=3, dim4=2, rank=Matrix, &
!                       vartype=SpaceTime)
!   CALL OK(GetTotalShape(obj) .EQ. 4, "matrix spaceTime: 4")
! END BLOCK
!
! ! Test 2: total shape of the result of a binary operation
! BLOCK
!   ! scalar space (tshape 1) and scalar time (tshape 1): spaceTime
!   CALL OK(GetTotalShape(rankCase=00, varCase=12, tshape1=1, &
!                         tshape2=1) .EQ. 2, "scalar space * scalar time")
!   ! scalar constant (1) and vector space (2)
!   CALL OK(GetTotalShape(rankCase=01, varCase=01, tshape1=1, &
!                         tshape2=2) .EQ. 2, "scalar constant * vector space")
!   ! vector space (2) and scalar constant (1)
!   CALL OK(GetTotalShape(rankCase=10, varCase=10, tshape1=2, &
!                         tshape2=1) .EQ. 2, "vector space * scalar constant")
!   ! scalar space (1) and matrix constant (2)
!   CALL OK(GetTotalShape(rankCase=02, varCase=10, tshape1=1, &
!                         tshape2=2) .EQ. 3, "scalar space * matrix constant")
!   ! vector space (2) and vector time (2): vector spaceTime
!   CALL OK(GetTotalShape(rankCase=11, varCase=12, tshape1=2, &
!                         tshape2=2) .EQ. 3, "vector space * vector time")
!   ! matrix time (3) and vector space (2): matrix spaceTime
!   CALL OK(GetTotalShape(rankCase=21, varCase=21, tshape1=3, &
!                         tshape2=2) .EQ. 4, "matrix time * vector space")
! END BLOCK
!
! END PROGRAM main
!```
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
! Returns in `ans(1:tsize)` the shape of the data that results from a
! binary operation between two variables with shapes `s1` and `s2`.
! `tsize` is the same as [[GetTotalShapeFromRankVarCase]].
!
! The result is ordered as components (vector), or rows and columns
! (matrix), followed by the space size and then the time size. When both
! variables have the same variable type, the number of space and time
! points of the two must be the same; this is not checked.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
! USE BaseType, ONLY: Vector => TypeFEVariableVector
! USE FEVariable_Method, ONLY: GetShape
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: shape of one variable without allocation
! BLOCK
!   TYPE(FEVariable_) :: obj
!   INTEGER(I4B) :: s(4), tsize
!
!   obj = QuadratureVariable(dim1=3, dim2=4, dim3=2, rank=Vector, &
!                            vartype=SpaceTime)
!   CALL GetShape(obj=obj, ans=s, tsize=tsize)
!   CALL OK(tsize .EQ. 3, "vector spaceTime: tsize = 3")
!   CALL OK(ALL(s(1:tsize) .EQ. [3, 4, 2]), "vector spaceTime: shape")
!
!   obj = QuadratureVariable(dim1=2, dim2=2, dim3=5, rank=Matrix, &
!                            vartype=Space)
!   CALL GetShape(obj=obj, ans=s, tsize=tsize)
!   CALL OK(tsize .EQ. 3, "matrix space: tsize = 3")
!   CALL OK(ALL(s(1:tsize) .EQ. [2, 2, 5]), "matrix space: shape")
! END BLOCK
!
! ! Test 2: shape of the result of a binary operation
! BLOCK
!   INTEGER(I4B) :: s(4), tsize
!
!   ! scalar space [5] and scalar time [2]: [5, 2]
!   CALL GetShape(rankCase=00, varCase=12, s1=[5], s2=[2], tshape1=1, &
!                 tshape2=1, ans=s, tsize=tsize)
!   CALL OK(tsize .EQ. 2, "scalar space, scalar time: tsize")
!   CALL OK(ALL(s(1:tsize) .EQ. [5, 2]), "scalar space, scalar time")
!
!   ! scalar time [2] and scalar space [5]: [5, 2]
!   CALL GetShape(rankCase=00, varCase=21, s1=[2], s2=[5], tshape1=1, &
!                 tshape2=1, ans=s, tsize=tsize)
!   CALL OK(ALL(s(1:tsize) .EQ. [5, 2]), "scalar time, scalar space")
!
!   ! scalar space [5] and vector constant [3]: [3, 5]
!   CALL GetShape(rankCase=01, varCase=10, s1=[5], s2=[3], tshape1=1, &
!                 tshape2=1, ans=s, tsize=tsize)
!   CALL OK(tsize .EQ. 2, "scalar space, vector constant: tsize")
!   CALL OK(ALL(s(1:tsize) .EQ. [3, 5]), "scalar space, vector constant")
!
!   ! vector space [3, 5] and vector time [3, 2]: [3, 5, 2]
!   CALL GetShape(rankCase=11, varCase=12, s1=[3, 5], s2=[3, 2], &
!                 tshape1=2, tshape2=2, ans=s, tsize=tsize)
!   CALL OK(tsize .EQ. 3, "vector space, vector time: tsize")
!   CALL OK(ALL(s(1:tsize) .EQ. [3, 5, 2]), "vector space, vector time")
!
!   ! scalar spaceTime [5, 2] and matrix constant [3, 3]: [3, 3, 5, 2]
!   CALL GetShape(rankCase=02, varCase=30, s1=[5, 2], s2=[3, 3], &
!                 tshape1=2, tshape2=2, ans=s, tsize=tsize)
!   CALL OK(tsize .EQ. 4, "scalar spaceTime, matrix constant: tsize")
!   CALL OK(ALL(s(1:tsize) .EQ. [3, 3, 5, 2]), &
!           "scalar spaceTime, matrix constant")
!
!   ! matrix time [3, 3, 2] and vector space [3, 5]: [3, 3, 5, 2]
!   CALL GetShape(rankCase=21, varCase=21, s1=[3, 3, 2], s2=[3, 5], &
!                 tshape1=3, tshape2=2, ans=s, tsize=tsize)
!   CALL OK(tsize .EQ. 4, "matrix time, vector space: tsize")
!   CALL OK(ALL(s(1:tsize) .EQ. [3, 3, 5, 2]), "matrix time, vector space")
! END BLOCK
!
! END PROGRAM main
!```
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
! Returns the variable type that results from a binary operation between
! two variables with variable case `varCase` (see [[GetVarCase]]):
!
! - 00: constant
! - 01, 10, 11: space
! - 02, 20, 22: time
! - 03, 13, 23, 30, 31, 32, 33, 12, 21: spaceTime
!
! Examples
!
! See [[fevar_vartype]] (Test 2).
!
! See also: [[GetVarCase]], [[GetRank]]

INTERFACE GetVarType
  MODULE PURE FUNCTION GetVarTypeFromVarCase(varCase) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: varCase
    !! variable case of two variables, see GetVarCase
    INTEGER(I4B) :: ans
    !! variable type, TypeFEVariableOpt%constant, space, time or spacetime
  END FUNCTION GetVarTypeFromVarCase
END INTERFACE GetVarType

!----------------------------------------------------------------------------
!                                                          GetRank@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-02
! summary: Rank of the result of a binary operation
!
! Returns the rank that results from a binary operation between two
! variables with rank case `rankCase` (see [[GetRankCase]]):
!
! - 00: scalar
! - 01, 10, 11: vector
! - 02, 20, 22, 12, 21: matrix
!
! Changes:
!   - 2026-11-14: rank cases 12 and 21 (vector and matrix) give matrix,
!     as in GetShape and GetTotalShape
!
! Examples
!
! See [[fevar_rank]] (Test 2).
!
! See also: [[GetRankCase]], [[GetVarType]]

INTERFACE GetRank
  MODULE PURE FUNCTION GetRankFromRankCase(rankCase) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: rankCase
    !! rank case of two variables, see GetRankCase
    INTEGER(I4B) :: ans
    !! rank, TypeFEVariableOpt%scalar, vector or matrix
  END FUNCTION GetRankFromRankCase
END INTERFACE GetRank

!----------------------------------------------------------------------------
!                                                      GetRankCase@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-29
! summary: Rank case of two variables
!
! Returns `ans = 10 * a + b`, where `a` and `b` are 0 for scalar, 1 for
! vector and 2 for matrix, for `rank1` and `rank2` respectively. For
! example, a vector and a scalar give 10.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE FEVariable_Method, ONLY: GetRankCase
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: rankCase = 10 * a + b, with scalar 0, vector 1, matrix 2
! BLOCK
!   CALL OK(GetRankCase(fevaropt%scalar, fevaropt%scalar) .EQ. 00, "00")
!   CALL OK(GetRankCase(fevaropt%scalar, fevaropt%vector) .EQ. 01, "01")
!   CALL OK(GetRankCase(fevaropt%scalar, fevaropt%matrix) .EQ. 02, "02")
!   CALL OK(GetRankCase(fevaropt%vector, fevaropt%scalar) .EQ. 10, "10")
!   CALL OK(GetRankCase(fevaropt%vector, fevaropt%vector) .EQ. 11, "11")
!   CALL OK(GetRankCase(fevaropt%vector, fevaropt%matrix) .EQ. 12, "12")
!   CALL OK(GetRankCase(fevaropt%matrix, fevaropt%scalar) .EQ. 20, "20")
!   CALL OK(GetRankCase(fevaropt%matrix, fevaropt%vector) .EQ. 21, "21")
!   CALL OK(GetRankCase(fevaropt%matrix, fevaropt%matrix) .EQ. 22, "22")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[GetRank]], [[GetVarCase]]

INTERFACE
  MODULE PURE FUNCTION GetRankCase(rank1, rank2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: rank1
    !! rank of the first variable, TypeFEVariableOpt%scalar, ...
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
! Returns `ans = 10 * a + b`, where `a` and `b` are 0 for constant, 1 for
! space, 2 for time and 3 for spaceTime, for `vartype1` and `vartype2`
! respectively. For example, a space and a time variable give 12.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE FEVariable_Method, ONLY: GetVarCase
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: varCase = 10 * a + b, constant 0, space 1, time 2, spaceTime 3
! BLOCK
!   CALL OK(GetVarCase(fevaropt%constant, fevaropt%constant) .EQ. 00, "00")
!   CALL OK(GetVarCase(fevaropt%constant, fevaropt%spacetime) .EQ. 03, "03")
!   CALL OK(GetVarCase(fevaropt%space, fevaropt%time) .EQ. 12, "12")
!   CALL OK(GetVarCase(fevaropt%time, fevaropt%space) .EQ. 21, "21")
!   CALL OK(GetVarCase(fevaropt%time, fevaropt%time) .EQ. 22, "22")
!   CALL OK(GetVarCase(fevaropt%spacetime, fevaropt%space) .EQ. 31, "31")
!   CALL OK(GetVarCase(fevaropt%spacetime, fevaropt%spacetime) .EQ. 33, &
!           "33")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[GetVarType]], [[GetRankCase]]

INTERFACE
  MODULE PURE FUNCTION GetVarCase(vartype1, vartype2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: vartype1
    !! variable type of the first variable, TypeFEVariableOpt%space, ...
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
! `lambda` gets the shape, rank and variable type of `youngsModulus`.
! Each value is computed from the values at the same position in
! `youngsModulus` and `shearModulus`.
!
! Limitation: `youngsModulus` and `shearModulus` must have the same
! shape, rank, variable type and `len`. Mixing types, for example a
! constant `youngsModulus` with a `shearModulus` that varies in space,
! is not supported; this is not checked and reads out of bounds when
! `shearModulus` is shorter.
!
! Formula:
!
! $$ \lambda = \frac{\mu (E - 2 \mu)}{3 \mu - E} $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: Constant => TypeFEVariableConstant
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE FEVariable_Method, ONLY: GetLambdaFromYoungsModulus
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! ! Test 1: constant moduli, E = 2 mu (1 + nu), nu = 0.25
! BLOCK
!   TYPE(FEVariable_) :: E, mu, lambda
!   REAL(DFP) :: nu, Ev, muv, ans
!
!   nu = 0.25_DFP
!   Ev = 100.0_DFP
!   muv = Ev / (2.0_DFP * (1.0_DFP + nu))
!   ans = Ev * nu / ((1.0_DFP + nu) * (1.0_DFP - 2.0_DFP * nu))
!
!   E = QuadratureVariable(Ev, Scalar, Constant)
!   mu = QuadratureVariable(muv, Scalar, Constant)
!   CALL GetLambdaFromYoungsModulus(youngsModulus=E, shearModulus=mu, &
!                                   lambda=lambda)
!   CALL OK(ABS(lambda%val(1) - ans) .LT. tol, "constant: lambda = 40")
!   CALL OK(lambda%rank .EQ. fevaropt%scalar, "constant: rank of lambda")
!   CALL OK(lambda%varType .EQ. fevaropt%constant, "constant: vartype")
! END BLOCK
!
! ! Test 2: moduli at quadrature points
! BLOCK
!   TYPE(FEVariable_) :: E, mu, lambda
!   REAL(DFP) :: Ev(3), muv(3), ans(3)
!
!   Ev = [100.0_DFP, 200.0_DFP, 300.0_DFP]
!   muv = Ev / 2.5_DFP
!   ans = Ev * 0.25_DFP / (1.25_DFP * 0.5_DFP)
!
!   E = QuadratureVariable(Ev, Scalar, Space)
!   mu = QuadratureVariable(muv, Scalar, Space)
!   CALL GetLambdaFromYoungsModulus(youngsModulus=E, shearModulus=mu, &
!                                   lambda=lambda)
!   CALL OK(lambda%len .EQ. 3, "space: len of lambda = 3")
!   CALL OK(ALL(ABS(lambda%val(1:3) - ans) .LT. tol), "space: lambda")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE GetLambdaFromYoungsModulus
  MODULE PURE SUBROUTINE fevar_GetLambdaFromYoungsModulus( &
    youngsModulus, shearModulus, lambda)
    TYPE(FEVariable_), INTENT(IN) :: youngsModulus
    !! Young's modulus E
    TYPE(FEVariable_), INTENT(IN) :: shearModulus
    !! shear modulus mu, same shape, rank, vartype as youngsModulus
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
! Returns "Scalar", "Vector" or "Matrix" for `name` equal to
! TypeFEVariableOpt%scalar, vector or matrix. Any other value gives
! "Scalar". If `isUpper` is true, the name is in upper case.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE FEVariable_Method, ONLY: FEVariable_ToChar
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: names of scalar, vector and matrix
! BLOCK
!   CALL OK(FEVariable_ToChar(fevaropt%scalar) .EQ. "Scalar", "Scalar")
!   CALL OK(FEVariable_ToChar(fevaropt%vector) .EQ. "Vector", "Vector")
!   CALL OK(FEVariable_ToChar(fevaropt%matrix) .EQ. "Matrix", "Matrix")
! END BLOCK
!
! ! Test 2: upper case names
! BLOCK
!   CALL OK(FEVariable_ToChar(fevaropt%vector, isUpper=.TRUE.) .EQ. &
!           "VECTOR", "VECTOR")
!   CALL OK(FEVariable_ToChar(fevaropt%matrix, isUpper=.FALSE.) .EQ. &
!           "Matrix", "isUpper false: Matrix")
! END BLOCK
!
! ! Test 3: unknown rank gives Scalar
! BLOCK
!   CALL OK(FEVariable_ToChar(-1) .EQ. "Scalar", "unknown: Scalar")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[FEVariable_ToInteger]]

INTERFACE
  MODULE PURE FUNCTION FEVariable_ToChar(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    !! rank, TypeFEVariableOpt%scalar, vector or matrix
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
! Only the first character of `name` is used, in any case: "s" gives
! TypeFEVariableOpt%scalar, "v" vector and "m" matrix. Any other first
! character gives scalar. `name` must not be empty.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE FEVariable_Method, ONLY: FEVariable_ToInteger
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: names in any case, only the first letter is used
! BLOCK
!   CALL OK(FEVariable_ToInteger("Scalar") .EQ. fevaropt%scalar, "Scalar")
!   CALL OK(FEVariable_ToInteger("VECTOR") .EQ. fevaropt%vector, "VECTOR")
!   CALL OK(FEVariable_ToInteger("matrix") .EQ. fevaropt%matrix, "matrix")
!   CALL OK(FEVariable_ToInteger("v") .EQ. fevaropt%vector, "v")
! END BLOCK
!
! ! Test 2: unknown name gives scalar
! BLOCK
!   CALL OK(FEVariable_ToInteger("tensor") .EQ. fevaropt%scalar, &
!           "unknown: scalar")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[FEVariable_ToChar]]

INTERFACE
  MODULE PURE FUNCTION FEVariable_ToInteger(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    !! name of the rank, "Scalar", "Vector", "Matrix"; LEN(name) >= 1
    INTEGER(I4B) :: ans
    !! rank, TypeFEVariableOpt%scalar, vector or matrix
  END FUNCTION FEVariable_ToInteger
END INTERFACE

!----------------------------------------------------------------------------
!                                                             SIZE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-12
! summary: Returns the size of variable
!
! If `dim` is present, returns `obj%s(dim)`. Otherwise returns `obj%len`,
! the total number of values. `dim` is not checked; it must be at most
! the total shape of `obj` (see [[GetTotalShape]]):
!
! - scalar: 1 (constant, space, time), 2 (spaceTime)
! - vector: 1 (constant), 2 (space, time), 3 (spaceTime)
! - matrix: 2 (constant), 3 (space, time), 4 (spaceTime)
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Constant => TypeFEVariableConstant
! USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE FEVariable_Method, ONLY: NodalVariable
! USE FEVariable_Method, ONLY: SIZE
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: scalar, space
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   obj = NodalVariable(tsize=4, rank=Scalar, vartype=Space)
!   CALL OK(SIZE(obj) .EQ. 4, "scalar space: SIZE = len = 4")
!   CALL OK(SIZE(obj, 1) .EQ. 4, "scalar space: SIZE(dim=1) = 4")
! END BLOCK
!
! ! Test 2: matrix, spaceTime gives len without dim, s(dim) with dim
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   obj = NodalVariable(dim1=2, dim2=3, dim3=4, dim4=5, rank=Matrix, &
!                       vartype=SpaceTime)
!   CALL OK(SIZE(obj) .EQ. 120, "matrix spaceTime: SIZE = len = 120")
!   CALL OK(SIZE(obj, 1) .EQ. 2, "matrix spaceTime: SIZE(dim=1) = 2")
!   CALL OK(SIZE(obj, 2) .EQ. 3, "matrix spaceTime: SIZE(dim=2) = 3")
!   CALL OK(SIZE(obj, 3) .EQ. 4, "matrix spaceTime: SIZE(dim=3) = 4")
!   CALL OK(SIZE(obj, 4) .EQ. 5, "matrix spaceTime: SIZE(dim=4) = 5")
! END BLOCK
!
! ! Test 3: scalar, constant
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   obj = NodalVariable(2.0_DFP, Scalar, Constant)
!   CALL OK(SIZE(obj) .EQ. 1, "scalar constant: SIZE = 1")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[GetLen]], [[SHAPE]]

INTERFACE Size
  MODULE PURE FUNCTION fevar_Size(obj, Dim) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Dim
    !! dimension, 1 <= dim <= obj%tshape
    INTEGER(I4B) :: ans
    !! obj%s(dim) if dim is present, else obj%len
  END FUNCTION fevar_Size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                            SHAPE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-12
! summary: Returns the shape of data
!
! Returns `obj%s(1:obj%tshape)`. The shape depends on the rank and the
! variable type:
!
!| rank | vartype | ans |
!| --- | --- | ---  |
!| Scalar | Constant | [1] |
!| Scalar | Space, Time | [obj%s(1)] |
!| Scalar | SpaceTime | [obj%s(1), obj%s(2)] |
!| Vector | Constant | [obj%s(1)] |
!| Vector | Space, Time | [obj%s(1), obj%s(2)] |
!| Vector | SpaceTime | [obj%s(1), obj%s(2), obj%s(3)] |
!| Matrix | Constant | [obj%s(1), obj%s(2)] |
!| Matrix | Space, Time | [obj%s(1), obj%s(2), obj%s(3)] |
!| Matrix | SpaceTime | [obj%s(1), obj%s(2), obj%s(3), obj%s(4)] |
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Constant => TypeFEVariableConstant
! USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
! USE BaseType, ONLY: Vector => TypeFEVariableVector
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE FEVariable_Method, ONLY: SHAPE
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: scalar, constant
! BLOCK
!   TYPE(FEVariable_) :: obj
!   INTEGER(I4B), ALLOCATABLE :: s(:)
!
!   obj = QuadratureVariable(1.0_DFP, Scalar, Constant)
!   s = SHAPE(obj)
!   CALL OK(SIZE(s) .EQ. 1, "scalar constant: SIZE(shape) = 1")
!   CALL OK(s(1) .EQ. 1, "scalar constant: shape = [1]")
! END BLOCK
!
! ! Test 2: vector, space
! BLOCK
!   TYPE(FEVariable_) :: obj
!   INTEGER(I4B), ALLOCATABLE :: s(:)
!
!   obj = QuadratureVariable(nrow=3, ncol=4, rank=Vector, vartype=Space)
!   s = SHAPE(obj)
!   CALL OK(SIZE(s) .EQ. 2, "vector space: SIZE(shape) = 2")
!   CALL OK(ALL(s .EQ. [3, 4]), "vector space: shape = [3, 4]")
! END BLOCK
!
! ! Test 3: matrix, spaceTime
! BLOCK
!   TYPE(FEVariable_) :: obj
!   INTEGER(I4B), ALLOCATABLE :: s(:)
!
!   obj = QuadratureVariable(dim1=2, dim2=3, dim3=4, dim4=5, rank=Matrix, &
!                            vartype=SpaceTime)
!   s = SHAPE(obj)
!   CALL OK(SIZE(s) .EQ. 4, "matrix spaceTime: SIZE(shape) = 4")
!   CALL OK(ALL(s .EQ. [2, 3, 4, 5]), "matrix spaceTime: shape")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[fevar_GetShape]], [[GetTotalShape]]

INTERFACE Shape
  MODULE PURE FUNCTION fevar_Shape(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! shape of data, SIZE(ans) = obj%tshape
  END FUNCTION fevar_Shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                         GetShape@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary: Returns the shape of data without allocation
!
! Same as [[fevar_Shape]], but writes the shape in `ans(1:tsize)` with
! `tsize = obj%tshape`.
!
! Examples
!
! See [[GetShapeFromRankVarCase]] (Test 1).
!
! See also: [[fevar_Shape]]

INTERFACE GetShape
  MODULE PURE SUBROUTINE fevar_GetShape(obj, ans, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! shape of data, SIZE(ans) >= obj%tshape
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans, obj%tshape
  END SUBROUTINE fevar_GetShape
END INTERFACE GetShape

!----------------------------------------------------------------------------
!                                                    GetTotalShape@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-03
! summary: Returns the total size of shape of data
!
! Returns `obj%tshape`, which depends on the rank and vartype:
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
! See [[GetTotalShapeFromRankVarCase]] (Test 1).
!
! See also: [[SHAPE]]

INTERFACE GetTotalShape
  MODULE PURE FUNCTION fevar_GetTotalShape(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B) :: ans
    !! total shape, obj%tshape
  END FUNCTION fevar_GetTotalShape
END INTERFACE GetTotalShape

!----------------------------------------------------------------------------
!                                                          GetRank@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the rank of FEVariable
!
! Returns `obj%rank`: TypeFEVariableOpt%scalar, vector or matrix.
! Also available as the operator `.rank.`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: Constant => TypeFEVariableConstant
! USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Vector => TypeFEVariableVector
! USE FEVariable_Method, ONLY: GetRank
! USE FEVariable_Method, ONLY: NodalVariable
! USE FEVariable_Method, ONLY: OPERATOR(.rank.)
! USE GlobalData, ONLY: DFP
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: rank of scalar, vector and matrix variables
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   obj = NodalVariable(1.0_DFP, Scalar, Constant)
!   CALL OK(GetRank(obj) .EQ. fevaropt%scalar, "GetRank = scalar")
!   CALL OK((.rank.obj) .EQ. fevaropt%scalar, ".rank. = scalar")
!
!   obj = NodalVariable(tsize=3, rank=Vector, vartype=Constant)
!   CALL OK(GetRank(obj) .EQ. fevaropt%vector, "GetRank = vector")
!
!   obj = NodalVariable(nrow=3, ncol=3, rank=Matrix, vartype=Constant)
!   CALL OK((.rank.obj) .EQ. fevaropt%matrix, ".rank. = matrix")
! END BLOCK
!
! ! Test 2: rank of the result from a rank case (see GetRankCase)
! BLOCK
!   CALL OK(GetRank(rankCase=00) .EQ. fevaropt%scalar, "rankCase 00: scalar")
!   CALL OK(GetRank(rankCase=01) .EQ. fevaropt%vector, "rankCase 01: vector")
!   CALL OK(GetRank(rankCase=10) .EQ. fevaropt%vector, "rankCase 10: vector")
!   CALL OK(GetRank(rankCase=11) .EQ. fevaropt%vector, "rankCase 11: vector")
!   CALL OK(GetRank(rankCase=02) .EQ. fevaropt%matrix, "rankCase 02: matrix")
!   CALL OK(GetRank(rankCase=20) .EQ. fevaropt%matrix, "rankCase 20: matrix")
!   CALL OK(GetRank(rankCase=22) .EQ. fevaropt%matrix, "rankCase 22: matrix")
!   CALL OK(GetRank(rankCase=12) .EQ. fevaropt%matrix, "rankCase 12: matrix")
!   CALL OK(GetRank(rankCase=21) .EQ. fevaropt%matrix, "rankCase 21: matrix")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(.RANK.)
  MODULE PURE FUNCTION fevar_rank(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B) :: ans
    !! rank of obj
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
! Returns `obj%varType`: TypeFEVariableOpt%constant, space, time or
! spacetime. Also available as the operator `.vartype.`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
! USE BaseType, ONLY: Time => TypeFEVariableTime
! USE FEVariable_Method, ONLY: GetVarType
! USE FEVariable_Method, ONLY: NodalVariable
! USE FEVariable_Method, ONLY: OPERATOR(.vartype.)
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: variable type of space, time and spaceTime variables
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   obj = NodalVariable(tsize=3, rank=Scalar, vartype=Space)
!   CALL OK(GetVarType(obj) .EQ. fevaropt%space, "GetVarType = space")
!   CALL OK((.vartype.obj) .EQ. fevaropt%space, ".vartype. = space")
!
!   obj = NodalVariable(tsize=2, rank=Scalar, vartype=Time)
!   CALL OK(GetVarType(obj) .EQ. fevaropt%time, "GetVarType = time")
!
!   obj = NodalVariable(nrow=3, ncol=2, rank=Scalar, vartype=SpaceTime)
!   CALL OK((.vartype.obj) .EQ. fevaropt%spacetime, ".vartype. = spaceTime")
! END BLOCK
!
! ! Test 2: variable type of the result from a var case (see GetVarCase)
! BLOCK
!   CALL OK(GetVarType(varCase=00) .EQ. fevaropt%constant, "varCase 00")
!   CALL OK(GetVarType(varCase=01) .EQ. fevaropt%space, "varCase 01")
!   CALL OK(GetVarType(varCase=11) .EQ. fevaropt%space, "varCase 11")
!   CALL OK(GetVarType(varCase=20) .EQ. fevaropt%time, "varCase 20")
!   CALL OK(GetVarType(varCase=22) .EQ. fevaropt%time, "varCase 22")
!   CALL OK(GetVarType(varCase=12) .EQ. fevaropt%spacetime, "varCase 12")
!   CALL OK(GetVarType(varCase=21) .EQ. fevaropt%spacetime, "varCase 21")
!   CALL OK(GetVarType(varCase=03) .EQ. fevaropt%spacetime, "varCase 03")
!   CALL OK(GetVarType(varCase=33) .EQ. fevaropt%spacetime, "varCase 33")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(.vartype.)
  MODULE PURE FUNCTION fevar_vartype(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B) :: ans
    !! variable type of obj
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
! Returns `obj%defineOn`: TypeFEVariableOpt%nodal or quadrature.
! Also available as the operator `.defineon.`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE FEVariable_Method, ONLY: GetDefineOn
! USE FEVariable_Method, ONLY: NodalVariable
! USE FEVariable_Method, ONLY: OPERATOR(.defineon.)
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: nodal and quadrature variables
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   obj = NodalVariable(tsize=3, rank=Scalar, vartype=Space)
!   CALL OK(GetDefineOn(obj) .EQ. fevaropt%nodal, "GetDefineOn = nodal")
!   CALL OK((.defineon.obj) .EQ. fevaropt%nodal, ".defineon. = nodal")
!
!   obj = QuadratureVariable(tsize=3, rank=Scalar, vartype=Space)
!   CALL OK(GetDefineOn(obj) .EQ. fevaropt%quadrature, &
!           "GetDefineOn = quadrature")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(.defineon.)
  MODULE PURE FUNCTION fevar_defineon(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B) :: ans
    !! where obj is defined, nodal or quadrature
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
! Returns `obj%len`, the number of values stored in `obj%val`.
! Also available as the operator `.len.`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE FEVariable_Method, ONLY: GetLen
! USE FEVariable_Method, ONLY: NodalVariable
! USE FEVariable_Method, ONLY: OPERATOR(.len.)
! USE FEVariable_Method, ONLY: SetLen
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: len is the product of the shape
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   obj = NodalVariable(dim1=2, dim2=2, dim3=3, rank=Matrix, vartype=Space)
!   CALL OK(GetLen(obj) .EQ. 12, "GetLen = 12")
!   CALL OK((.len.obj) .EQ. 12, ".len. = 12")
! END BLOCK
!
! ! Test 2: len follows SetLen, and is zero for an empty object
! BLOCK
!   TYPE(FEVariable_) :: obj, empty
!
!   obj = NodalVariable(dim1=2, dim2=2, dim3=3, rank=Matrix, vartype=Space)
!   CALL SetLen(obj, 5)
!   CALL OK(GetLen(obj) .EQ. 5, "GetLen = 5 after SetLen")
!   CALL OK(GetLen(empty) .EQ. 0, "GetLen = 0 for empty object")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE OPERATOR(.len.)
  MODULE PURE FUNCTION fevar_len(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    INTEGER(I4B) :: ans
    !! length of data, obj%len
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
! Returns `obj%defineOn .EQ. TypeFEVariableOpt%nodal`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE FEVariable_Method, ONLY: IsNodalVariable
! USE FEVariable_Method, ONLY: NodalVariable
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: nodal variable is nodal, quadrature variable is not
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   obj = NodalVariable(tsize=3, rank=Scalar, vartype=Space)
!   CALL OK(IsNodalVariable(obj), "nodal variable: true")
!
!   obj = QuadratureVariable(tsize=3, rank=Scalar, vartype=Space)
!   CALL OK(.NOT. IsNodalVariable(obj), "quadrature variable: false")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[IsQuadratureVariable]]

INTERFACE IsNodalVariable
  MODULE PURE FUNCTION fevar_IsNodalVariable(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    LOGICAL(LGT) :: ans
    !! true if obj is a nodal variable
  END FUNCTION fevar_IsNodalVariable
END INTERFACE IsNodalVariable

!----------------------------------------------------------------------------
!                                             IsQuadratureVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns true if FEVariable is defined on quadrature points
!
! Returns `obj%defineOn .NE. TypeFEVariableOpt%nodal`, so it is also
! true for an object that is not initiated.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE FEVariable_Method, ONLY: IsQuadratureVariable
! USE FEVariable_Method, ONLY: NodalVariable
! USE FEVariable_Method, ONLY: QuadratureVariable
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: quadrature variable is quadrature, nodal variable is not
! BLOCK
!   TYPE(FEVariable_) :: obj
!
!   obj = QuadratureVariable(tsize=3, rank=Scalar, vartype=Space)
!   CALL OK(IsQuadratureVariable(obj), "quadrature variable: true")
!
!   obj = NodalVariable(tsize=3, rank=Scalar, vartype=Space)
!   CALL OK(.NOT. IsQuadratureVariable(obj), "nodal variable: false")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[IsNodalVariable]]

INTERFACE IsQuadratureVariable
  MODULE PURE FUNCTION fevar_IsQuadratureVariable(obj) RESULT(ans)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    LOGICAL(LGT) :: ans
    !! true if obj is not a nodal variable
  END FUNCTION fevar_IsQuadratureVariable
END INTERFACE IsQuadratureVariable

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is scalar, constant
!
! Returns the value `obj%val(1)`.
! `obj` must be a scalar, constant variable; this is not checked.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Constant => TypeFEVariableConstant
! USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
! USE BaseType, ONLY: Scalar => TypeFEVariableScalar
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
! USE BaseType, ONLY: Time => TypeFEVariableTime
! USE BaseType, ONLY: Vector => TypeFEVariableVector
! USE FEVariable_Method, ONLY: Get
! USE FEVariable_Method, ONLY: NodalVariable
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: ii
! REAL(DFP) :: v(24)
!
! v = [(REAL(ii, DFP), ii=1, 24)]
!
! ! Test 1: scalar variables
! BLOCK
!   TYPE(FEVariable_) :: obj
!   REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :)
!
!   obj = NodalVariable(3.0_DFP, Scalar, Constant)
!   CALL OK(Get(obj, Scalar, Constant) .EQ. 3.0_DFP, "scalar constant")
!
!   obj = NodalVariable(v(1:4), Scalar, Space)
!   r1 = Get(obj, Scalar, Space)
!   CALL OK(ALL(r1 .EQ. v(1:4)), "scalar space")
!
!   obj = NodalVariable(v(1:2), Scalar, Time)
!   r1 = Get(obj, Scalar, Time)
!   CALL OK(ALL(r1 .EQ. v(1:2)), "scalar time")
!
!   obj = NodalVariable(RESHAPE(v(1:6), [3, 2]), Scalar, SpaceTime)
!   r2 = Get(obj, Scalar, SpaceTime)
!   CALL OK(ALL(SHAPE(r2) .EQ. [3, 2]), "scalar spaceTime: shape")
!   CALL OK(ALL(r2 .EQ. RESHAPE(v(1:6), [3, 2])), "scalar spaceTime")
! END BLOCK
!
! ! Test 2: vector variables
! BLOCK
!   TYPE(FEVariable_) :: obj
!   REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), r3(:, :, :)
!
!   obj = NodalVariable(v(1:3), Vector, Constant)
!   r1 = Get(obj, Vector, Constant)
!   CALL OK(ALL(r1 .EQ. v(1:3)), "vector constant")
!
!   obj = NodalVariable(RESHAPE(v(1:6), [2, 3]), Vector, Space)
!   r2 = Get(obj, Vector, Space)
!   CALL OK(ALL(r2 .EQ. RESHAPE(v(1:6), [2, 3])), "vector space")
!
!   obj = NodalVariable(RESHAPE(v(1:6), [3, 2]), Vector, Time)
!   r2 = Get(obj, Vector, Time)
!   CALL OK(ALL(r2 .EQ. RESHAPE(v(1:6), [3, 2])), "vector time")
!
!   obj = NodalVariable(RESHAPE(v(1:12), [3, 2, 2]), Vector, SpaceTime)
!   r3 = Get(obj, Vector, SpaceTime)
!   CALL OK(ALL(r3 .EQ. RESHAPE(v(1:12), [3, 2, 2])), "vector spaceTime")
! END BLOCK
!
! ! Test 3: matrix variables
! BLOCK
!   TYPE(FEVariable_) :: obj
!   REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :), r4(:, :, :, :)
!
!   obj = NodalVariable(RESHAPE(v(1:6), [2, 3]), Matrix, Constant)
!   r2 = Get(obj, Matrix, Constant)
!   CALL OK(ALL(r2 .EQ. RESHAPE(v(1:6), [2, 3])), "matrix constant")
!
!   obj = NodalVariable(RESHAPE(v(1:12), [2, 2, 3]), Matrix, Space)
!   r3 = Get(obj, Matrix, Space)
!   CALL OK(ALL(r3 .EQ. RESHAPE(v(1:12), [2, 2, 3])), "matrix space")
!
!   obj = NodalVariable(RESHAPE(v(1:8), [2, 2, 2]), Matrix, Time)
!   r3 = Get(obj, Matrix, Time)
!   CALL OK(ALL(r3 .EQ. RESHAPE(v(1:8), [2, 2, 2])), "matrix time")
!
!   obj = NodalVariable(RESHAPE(v, [2, 2, 3, 2]), Matrix, SpaceTime)
!   r4 = Get(obj, Matrix, SpaceTime)
!   CALL OK(ALL(SHAPE(r4) .EQ. [2, 2, 3, 2]), "matrix spaceTime: shape")
!   CALL OK(ALL(r4 .EQ. RESHAPE(v, [2, 2, 3, 2])), "matrix spaceTime")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[Get_]]

INTERFACE Get
  MODULE PURE FUNCTION Scalar_Constant(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP) :: val
    !! value, obj%val(1)
  END FUNCTION Scalar_Constant
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is scalar, space
!
! Returns the values of `obj` in an array of shape
! `[nns]` = `obj%s(1:1)`.
! `obj` must be a scalar, space variable; this is not checked.
!
! Examples
!
!```fortran
! val = Get(obj, TypeFEVariableScalar, TypeFEVariableSpace)
!```
!
! Full example: see [[Scalar_Constant]].
!
! See also: [[Scalar_Space_]]

INTERFACE Get
  MODULE PURE FUNCTION Scalar_Space(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), ALLOCATABLE :: val(:)
    !! values, SHAPE(val) = [nns]
  END FUNCTION Scalar_Space
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is scalar, space without allocation
!
! Same as [[Scalar_Space]], but writes the values in
! `val(1:tsize)`.
! `val` must be large enough; this is not checked.
!
! Examples
!
!```fortran
! CALL Get_(obj, TypeFEVariableScalar, TypeFEVariableSpace, val, &
!           tsize)
!```
!
! Full example: src/modules/FEVariable/examples/Get__test_1.F90
!
! See also: [[Scalar_Space]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Scalar_Space_(obj, rank, vartype, val, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), INTENT(INOUT) :: val(:)
    !! values, SHAPE(val) >= [nns]
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written, obj%len
  END SUBROUTINE Scalar_Space_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is scalar, time
!
! Returns the values of `obj` in an array of shape
! `[nnt]` = `obj%s(1:1)`.
! `obj` must be a scalar, time variable; this is not checked.
!
! Examples
!
!```fortran
! val = Get(obj, TypeFEVariableScalar, TypeFEVariableTime)
!```
!
! Full example: see [[Scalar_Constant]].
!
! See also: [[Scalar_Time_]]

INTERFACE Get
  MODULE PURE FUNCTION Scalar_Time(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
    REAL(DFP), ALLOCATABLE :: val(:)
    !! values, SHAPE(val) = [nnt]
  END FUNCTION Scalar_Time
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is scalar, time without allocation
!
! Same as [[Scalar_Time]], but writes the values in
! `val(1:tsize)`.
! `val` must be large enough; this is not checked.
!
! Examples
!
!```fortran
! CALL Get_(obj, TypeFEVariableScalar, TypeFEVariableTime, val, &
!           tsize)
!```
!
! Full example: src/modules/FEVariable/examples/Get__test_1.F90
!
! See also: [[Scalar_Time]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Scalar_Time_(obj, rank, vartype, val, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
    REAL(DFP), INTENT(INOUT) :: val(:)
    !! values, SHAPE(val) >= [nnt]
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written, obj%len
  END SUBROUTINE Scalar_Time_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is scalar, spaceTime
!
! Returns the values of `obj` in an array of shape
! `[nns, nnt]` = `obj%s(1:2)`.
! `obj` must be a scalar, spaceTime variable; this is not checked.
!
! Examples
!
!```fortran
! val = Get(obj, TypeFEVariableScalar, TypeFEVariableSpaceTime)
!```
!
! Full example: see [[Scalar_Constant]].
!
! See also: [[Scalar_SpaceTime_]]

INTERFACE Get
  MODULE PURE FUNCTION Scalar_SpaceTime(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), ALLOCATABLE :: val(:, :)
    !! values, SHAPE(val) = [nns, nnt]
  END FUNCTION Scalar_SpaceTime
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is scalar, spaceTime without allocation
!
! Same as [[Scalar_SpaceTime]], but writes the values in
! `val(1:nrow, 1:ncol)`.
! `val` must be large enough; this is not checked.
!
! Examples
!
!```fortran
! CALL Get_(obj, TypeFEVariableScalar, TypeFEVariableSpaceTime, val, &
!           nrow, ncol)
!```
!
! Full example: src/modules/FEVariable/examples/Get__test_1.F90
!
! See also: [[Scalar_SpaceTime]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Scalar_SpaceTime_( &
    obj, rank, vartype, val, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    !! TypeFEVariableScalar
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    !! values, SHAPE(val) >= [nns, nnt]
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! nns
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! nnt
  END SUBROUTINE Scalar_SpaceTime_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is vector, constant
!
! Returns the values of `obj` in an array of shape
! `[ncomp]` = `obj%s(1:1)`.
! `obj` must be a vector, constant variable; this is not checked.
!
! Examples
!
!```fortran
! val = Get(obj, TypeFEVariableVector, TypeFEVariableConstant)
!```
!
! Full example: see [[Scalar_Constant]].
!
! See also: [[Vector_Constant_]]

INTERFACE Get
  MODULE PURE FUNCTION Vector_Constant(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP), ALLOCATABLE :: val(:)
    !! values, SHAPE(val) = [ncomp]
  END FUNCTION Vector_Constant
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is vector, constant without allocation
!
! Same as [[Vector_Constant]], but writes the values in
! `val(1:tsize)`.
! `val` must be large enough; this is not checked.
!
! Examples
!
!```fortran
! CALL Get_(obj, TypeFEVariableVector, TypeFEVariableConstant, val, &
!           tsize)
!```
!
! Full example: src/modules/FEVariable/examples/Get__test_1.F90
!
! See also: [[Vector_Constant]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Vector_Constant_(obj, rank, vartype, val, tsize)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP), INTENT(INOUT) :: val(:)
    !! values, SHAPE(val) >= [ncomp]
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written, obj%len
  END SUBROUTINE Vector_Constant_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is vector, space
!
! Returns the values of `obj` in an array of shape
! `[ncomp, nns]` = `obj%s(1:2)`.
! `obj` must be a vector, space variable; this is not checked.
!
! Examples
!
!```fortran
! val = Get(obj, TypeFEVariableVector, TypeFEVariableSpace)
!```
!
! Full example: see [[Scalar_Constant]].
!
! See also: [[Vector_Space_]]

INTERFACE Get
  MODULE PURE FUNCTION Vector_Space(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), ALLOCATABLE :: val(:, :)
    !! values, SHAPE(val) = [ncomp, nns]
  END FUNCTION Vector_Space
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is vector, space without allocation
!
! Same as [[Vector_Space]], but writes the values in
! `val(1:nrow, 1:ncol)`.
! `val` must be large enough; this is not checked.
!
! Examples
!
!```fortran
! CALL Get_(obj, TypeFEVariableVector, TypeFEVariableSpace, val, &
!           nrow, ncol)
!```
!
! Full example: src/modules/FEVariable/examples/Get__test_1.F90
!
! See also: [[Vector_Space]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Vector_Space_(obj, rank, vartype, val, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    !! values, SHAPE(val) >= [ncomp, nns]
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! ncomp
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! nns
  END SUBROUTINE Vector_Space_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is vector, time
!
! Returns the values of `obj` in an array of shape
! `[ncomp, nnt]` = `obj%s(1:2)`.
! `obj` must be a vector, time variable; this is not checked.
!
! Examples
!
!```fortran
! val = Get(obj, TypeFEVariableVector, TypeFEVariableTime)
!```
!
! Full example: see [[Scalar_Constant]].
!
! See also: [[Vector_Time_]]

INTERFACE Get
  MODULE PURE FUNCTION Vector_Time(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
    REAL(DFP), ALLOCATABLE :: val(:, :)
    !! values, SHAPE(val) = [ncomp, nnt]
  END FUNCTION Vector_Time
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is vector, time without allocation
!
! Same as [[Vector_Time]], but writes the values in
! `val(1:nrow, 1:ncol)`.
! `val` must be large enough; this is not checked.
!
! Examples
!
!```fortran
! CALL Get_(obj, TypeFEVariableVector, TypeFEVariableTime, val, &
!           nrow, ncol)
!```
!
! Full example: src/modules/FEVariable/examples/Get__test_1.F90
!
! See also: [[Vector_Time]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Vector_Time_(obj, rank, vartype, val, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    !! values, SHAPE(val) >= [ncomp, nnt]
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! ncomp
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! nnt
  END SUBROUTINE Vector_Time_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is vector, spaceTime
!
! Returns the values of `obj` in an array of shape
! `[ncomp, nns, nnt]` = `obj%s(1:3)`.
! `obj` must be a vector, spaceTime variable; this is not checked.
!
! Examples
!
!```fortran
! val = Get(obj, TypeFEVariableVector, TypeFEVariableSpaceTime)
!```
!
! Full example: see [[Scalar_Constant]].
!
! See also: [[Vector_SpaceTime_]]

INTERFACE Get
  MODULE PURE FUNCTION Vector_SpaceTime(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), ALLOCATABLE :: val(:, :, :)
    !! values, SHAPE(val) = [ncomp, nns, nnt]
  END FUNCTION Vector_SpaceTime
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is vector, spaceTime without allocation
!
! Same as [[Vector_SpaceTime]], but writes the values in
! `val(1:dim1, 1:dim2, 1:dim3)`.
! `val` must be large enough; this is not checked.
!
! Examples
!
!```fortran
! CALL Get_(obj, TypeFEVariableVector, TypeFEVariableSpaceTime, val, &
!           dim1, dim2, dim3)
!```
!
! Full example: src/modules/FEVariable/examples/Get__test_1.F90
!
! See also: [[Vector_SpaceTime]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Vector_SpaceTime_( &
    obj, rank, vartype, val, dim1, dim2, dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    !! TypeFEVariableVector
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), INTENT(INOUT) :: val(:, :, :)
    !! values, SHAPE(val) >= [ncomp, nns, nnt]
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! ncomp
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! nns
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! nnt
  END SUBROUTINE Vector_SpaceTime_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is matrix, constant
!
! Returns the values of `obj` in an array of shape
! `[nrow, ncol]` = `obj%s(1:2)`.
! `obj` must be a matrix, constant variable; this is not checked.
!
! Examples
!
!```fortran
! val = Get(obj, TypeFEVariableMatrix, TypeFEVariableConstant)
!```
!
! Full example: see [[Scalar_Constant]].
!
! See also: [[Matrix_Constant_]]

INTERFACE Get
  MODULE PURE FUNCTION Matrix_Constant(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP), ALLOCATABLE :: val(:, :)
    !! values, SHAPE(val) = [nrow, ncol]
  END FUNCTION Matrix_Constant
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is matrix, constant without allocation
!
! Same as [[Matrix_Constant]], but writes the values in
! `val(1:nrow, 1:ncol)`.
! `val` must be large enough; this is not checked.
!
! Examples
!
!```fortran
! CALL Get_(obj, TypeFEVariableMatrix, TypeFEVariableConstant, val, &
!           nrow, ncol)
!```
!
! Full example: src/modules/FEVariable/examples/Get__test_1.F90
!
! See also: [[Matrix_Constant]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Matrix_Constant_(obj, rank, vartype, val, nrow, ncol)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    !! TypeFEVariableConstant
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    !! values, SHAPE(val) >= [nrow, ncol]
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! nrow
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! ncol
  END SUBROUTINE Matrix_Constant_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is matrix, space
!
! Returns the values of `obj` in an array of shape
! `[nrow, ncol, nns]` = `obj%s(1:3)`.
! `obj` must be a matrix, space variable; this is not checked.
!
! Examples
!
!```fortran
! val = Get(obj, TypeFEVariableMatrix, TypeFEVariableSpace)
!```
!
! Full example: see [[Scalar_Constant]].
!
! See also: [[Matrix_Space_]]

INTERFACE Get
  MODULE PURE FUNCTION Matrix_Space(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), ALLOCATABLE :: val(:, :, :)
    !! values, SHAPE(val) = [nrow, ncol, nns]
  END FUNCTION Matrix_Space
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is matrix, space without allocation
!
! Same as [[Matrix_Space]], but writes the values in
! `val(1:dim1, 1:dim2, 1:dim3)`.
! `val` must be large enough; this is not checked.
!
! Examples
!
!```fortran
! CALL Get_(obj, TypeFEVariableMatrix, TypeFEVariableSpace, val, &
!           dim1, dim2, dim3)
!```
!
! Full example: src/modules/FEVariable/examples/Get__test_1.F90
!
! See also: [[Matrix_Space]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Matrix_Space_( &
    obj, rank, vartype, val, dim1, dim2, dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    !! TypeFEVariableSpace
    REAL(DFP), INTENT(INOUT) :: val(:, :, :)
    !! values, SHAPE(val) >= [nrow, ncol, nns]
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! nrow
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! ncol
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! nns
  END SUBROUTINE Matrix_Space_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is matrix, time
!
! Returns the values of `obj` in an array of shape
! `[nrow, ncol, nnt]` = `obj%s(1:3)`.
! `obj` must be a matrix, time variable; this is not checked.
!
! Examples
!
!```fortran
! val = Get(obj, TypeFEVariableMatrix, TypeFEVariableTime)
!```
!
! Full example: see [[Scalar_Constant]].
!
! See also: [[Matrix_Time_]]

INTERFACE Get
  MODULE PURE FUNCTION Matrix_Time(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
    REAL(DFP), ALLOCATABLE :: val(:, :, :)
    !! values, SHAPE(val) = [nrow, ncol, nnt]
  END FUNCTION Matrix_Time
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is matrix, time without allocation
!
! Same as [[Matrix_Time]], but writes the values in
! `val(1:dim1, 1:dim2, 1:dim3)`.
! `val` must be large enough; this is not checked.
!
! Examples
!
!```fortran
! CALL Get_(obj, TypeFEVariableMatrix, TypeFEVariableTime, val, &
!           dim1, dim2, dim3)
!```
!
! Full example: src/modules/FEVariable/examples/Get__test_1.F90
!
! See also: [[Matrix_Time]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Matrix_Time_( &
    obj, rank, vartype, val, dim1, dim2, dim3)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    !! TypeFEVariableTime
    REAL(DFP), INTENT(INOUT) :: val(:, :, :)
    !! values, SHAPE(val) >= [nrow, ncol, nnt]
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! nrow
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! ncol
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! nnt
  END SUBROUTINE Matrix_Time_
END INTERFACE Get_

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-02
! summary: Returns value which is matrix, spaceTime
!
! Returns the values of `obj` in an array of shape
! `[nrow, ncol, nns, nnt]` = `obj%s(1:4)`.
! `obj` must be a matrix, spaceTime variable; this is not checked.
!
! Examples
!
!```fortran
! val = Get(obj, TypeFEVariableMatrix, TypeFEVariableSpaceTime)
!```
!
! Full example: see [[Scalar_Constant]].
!
! See also: [[Matrix_SpaceTime_]]

INTERFACE Get
  MODULE PURE FUNCTION Matrix_SpaceTime(obj, rank, vartype) RESULT(val)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), ALLOCATABLE :: val(:, :, :, :)
    !! values, SHAPE(val) = [nrow, ncol, nns, nnt]
  END FUNCTION Matrix_SpaceTime
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                             Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-03-04
! summary: Returns value which is matrix, spaceTime without allocation
!
! Same as [[Matrix_SpaceTime]], but writes the values in
! `val(1:dim1, 1:dim2, 1:dim3, 1:dim4)`.
! `val` must be large enough; this is not checked.
!
! Examples
!
!```fortran
! CALL Get_(obj, TypeFEVariableMatrix, TypeFEVariableSpaceTime, val, &
!           dim1, dim2, dim3, dim4)
!```
!
! Full example: src/modules/FEVariable/examples/Get__test_1.F90
!
! See also: [[Matrix_SpaceTime]]

INTERFACE Get_
  MODULE PURE SUBROUTINE Matrix_SpaceTime_( &
    obj, rank, vartype, val, dim1, dim2, dim3, dim4)
    CLASS(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    !! TypeFEVariableMatrix
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    !! TypeFEVariableSpaceTime
    REAL(DFP), INTENT(INOUT) :: val(:, :, :, :)
    !! values, SHAPE(val) >= [nrow, ncol, nns, nnt]
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! nrow
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! ncol
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! nns
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! nnt
  END SUBROUTINE Matrix_SpaceTime_
END INTERFACE Get_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEVariable_GetMethod
