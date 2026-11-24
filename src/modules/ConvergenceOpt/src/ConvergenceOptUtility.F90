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
!

MODULE ConvergenceOptUtility
USE GlobalData, ONLY: DFP, I4B, LGT
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE

PUBLIC :: ConvergenceType_ToInt
PUBLIC :: ConvergenceType_ToChar
PUBLIC :: ConvergenceType_ToString
PUBLIC :: ConvergenceIn_ToInt
PUBLIC :: ConvergenceIn_ToChar
PUBLIC :: ConvergenceIn_ToString
PUBLIC :: NormType_ToInt
PUBLIC :: NormType_ToChar
PUBLIC :: NormType_ToString

!----------------------------------------------------------------------------
!                                                       ConvergenceType_ToInt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from character to integer

INTERFACE
  MODULE FUNCTION ConvergenceType_ToInt(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION ConvergenceType_ToInt
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ConvergenceType_ToChar
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from integer to character

INTERFACE
  MODULE FUNCTION ConvergenceType_ToChar(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION ConvergenceType_ToChar
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ConvergenceType_ToString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from integer to character

INTERFACE
  MODULE FUNCTION ConvergenceType_ToString(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
    TYPE(String) :: ans
  END FUNCTION ConvergenceType_ToString
END INTERFACE

!----------------------------------------------------------------------------
!                                                        ConvergenceIn_ToInt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Value of convergenceIn from its name
!
! Description:
!   Converts the name of the quantity that is checked for convergence
!   (for example `convergenceIn` of a toml file) to its integer value.
!   The name is not case sensitive.
!
!   | name | value |
!   |---|---|
!   | RESIDUAL | TypeConvergenceOpt%res (convergenceInRes) |
!   | SOLUTION | TypeConvergenceOpt%sol (convergenceInSol) |
!   | BOTH | TypeConvergenceOpt%both (convergenceInResSol) |
!
!   TypeConvergenceOpt%both has the same value as the convergence type
!   "both" (see ConvergenceType_ToInt).
!
!   An unknown name is an error in a debug build; in a release build the
!   result is undefined.
!
! Arguments:
!   name : CHARACTER(*) - RESIDUAL, SOLUTION or BOTH, any case
!
! Returns:
!   ans : INTEGER(I4B) - TypeConvergenceOpt%res, %sol or %both
!
! Changes:
!   - 2026-11-17: returns TypeConvergenceOpt%res and %sol instead of
!     %relative and %absolute (same values), easifem/base#357
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeConvergenceOpt
! USE ConvergenceOptUtility, ONLY: ConvergenceIn_ToInt
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: upper case names
! BLOCK
!   CALL OK(ConvergenceIn_ToInt("RESIDUAL") .EQ. TypeConvergenceOpt%res, &
!           "RESIDUAL")
!   CALL OK(ConvergenceIn_ToInt("SOLUTION") .EQ. TypeConvergenceOpt%sol, &
!           "SOLUTION")
!   CALL OK(ConvergenceIn_ToInt("BOTH") .EQ. TypeConvergenceOpt%both, &
!           "BOTH")
! END BLOCK
!
! ! Test 2: the case of the name does not matter
! BLOCK
!   CALL OK(ConvergenceIn_ToInt("residual") .EQ. TypeConvergenceOpt%res, &
!           "residual")
!   CALL OK(ConvergenceIn_ToInt("Solution") .EQ. TypeConvergenceOpt%sol, &
!           "Solution")
!   CALL OK(ConvergenceIn_ToInt("both") .EQ. TypeConvergenceOpt%both, &
!           "both")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[ConvergenceIn_ToChar]], [[ConvergenceIn_ToString]]

INTERFACE
  MODULE FUNCTION ConvergenceIn_ToInt(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    !! name of convergenceIn, RESIDUAL, SOLUTION or BOTH
    INTEGER(I4B) :: ans
    !! TypeConvergenceOpt%res, %sol or %both
  END FUNCTION ConvergenceIn_ToInt
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ConvergenceIn_ToChar
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Name of a convergenceIn value
!
! Description:
!   Inverse of [[ConvergenceIn_ToInt]]: TypeConvergenceOpt%res, %sol and
!   %both give "Residual", "Solution" and "Both", or "RESIDUAL",
!   "SOLUTION" and "BOTH" when `isUpper` is true. An unknown value is an
!   error in a debug build; in a release build the result is undefined.
!
! Arguments:
!   name : INTEGER(I4B) - TypeConvergenceOpt%res, %sol or %both
!   isUpper : LOGICAL(LGT), OPTIONAL - upper case name, default false
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE - name
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeConvergenceOpt
! USE ConvergenceOptUtility, ONLY: ConvergenceIn_ToChar
! USE ConvergenceOptUtility, ONLY: ConvergenceIn_ToInt
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: default names
! BLOCK
!   CALL OK(ConvergenceIn_ToChar(TypeConvergenceOpt%res) .EQ. "Residual", &
!           "res: Residual")
!   CALL OK(ConvergenceIn_ToChar(TypeConvergenceOpt%sol) .EQ. "Solution", &
!           "sol: Solution")
!   CALL OK(ConvergenceIn_ToChar(TypeConvergenceOpt%both) .EQ. "Both", &
!           "both: Both")
! END BLOCK
!
! ! Test 2: upper case names
! BLOCK
!   CALL OK(ConvergenceIn_ToChar(TypeConvergenceOpt%res, isUpper=.TRUE.) &
!           .EQ. "RESIDUAL", "res: RESIDUAL")
!   CALL OK(ConvergenceIn_ToChar(TypeConvergenceOpt%sol, isUpper=.TRUE.) &
!           .EQ. "SOLUTION", "sol: SOLUTION")
!   CALL OK(ConvergenceIn_ToChar(TypeConvergenceOpt%both, isUpper=.TRUE.) &
!           .EQ. "BOTH", "both: BOTH")
! END BLOCK
!
! ! Test 3: round trip through ConvergenceIn_ToInt
! BLOCK
!   INTEGER(I4B) :: values(3), ii
!
!   values = [TypeConvergenceOpt%res, TypeConvergenceOpt%sol, &
!             TypeConvergenceOpt%both]
!   DO ii = 1, 3
!     CALL OK(ConvergenceIn_ToInt(ConvergenceIn_ToChar(values(ii))) .EQ. &
!             values(ii), "round trip: "//ConvergenceIn_ToChar(values(ii)))
!   END DO
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[ConvergenceIn_ToInt]], [[ConvergenceIn_ToString]]

INTERFACE
  MODULE FUNCTION ConvergenceIn_ToChar(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    !! TypeConvergenceOpt%res, %sol or %both
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
    !! upper case name, default false
    CHARACTER(:), ALLOCATABLE :: ans
    !! name of the value
  END FUNCTION ConvergenceIn_ToChar
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ConvergenceIn_ToString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: Name of a convergenceIn value as a String
!
! Description:
!   Same as [[ConvergenceIn_ToChar]], but the result is a String.
!
! Arguments:
!   name : INTEGER(I4B) - TypeConvergenceOpt%res, %sol or %both
!   isUpper : LOGICAL(LGT), OPTIONAL - upper case name, default false
!
! Returns:
!   ans : String - name
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: TypeConvergenceOpt
! USE ConvergenceOptUtility, ONLY: ConvergenceIn_ToString
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: same text as ConvergenceIn_ToChar, as a String
! BLOCK
!   TYPE(String) :: astr
!
!   astr = ConvergenceIn_ToString(TypeConvergenceOpt%res)
!   CALL OK(astr%Chars() .EQ. "Residual", "res: Residual")
!   astr = ConvergenceIn_ToString(TypeConvergenceOpt%sol, isUpper=.TRUE.)
!   CALL OK(astr%Chars() .EQ. "SOLUTION", "sol: SOLUTION")
!   astr = ConvergenceIn_ToString(TypeConvergenceOpt%both)
!   CALL OK(astr%Chars() .EQ. "Both", "both: Both")
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[ConvergenceIn_ToChar]], [[ConvergenceIn_ToInt]]

INTERFACE
  MODULE FUNCTION ConvergenceIn_ToString(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    !! TypeConvergenceOpt%res, %sol or %both
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
    !! upper case name, default false
    TYPE(String) :: ans
    !! name of the value
  END FUNCTION ConvergenceIn_ToString
END INTERFACE

!----------------------------------------------------------------------------
!                                                             NormType_ToInt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from character to integer

INTERFACE
  MODULE FUNCTION NormType_ToInt(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION NormType_ToInt
END INTERFACE

!----------------------------------------------------------------------------
!                                                            NormType_ToChar
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from integer to character

INTERFACE
  MODULE FUNCTION NormType_ToChar(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION NormType_ToChar
END INTERFACE

!----------------------------------------------------------------------------
!                                                           NormType_ToString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-14
! summary: convert convergence from integer to character

INTERFACE
  MODULE FUNCTION NormType_ToString(name, isUpper) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: name
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
    TYPE(String) :: ans
  END FUNCTION NormType_ToString
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ConvergenceOptUtility
