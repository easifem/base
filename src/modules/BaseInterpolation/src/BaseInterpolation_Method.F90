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
!

MODULE BaseInterpolation_Method
USE ErrorHandling, ONLY: Errormsg
USE GlobalData, ONLY: DFP, I4B, LGT, stdout, stderr
USE String_Class, ONLY: String
USE StringUtility, ONLY: UpperCase
USE Display_Method, ONLY: Tostring
USE BaseType, ONLY: poly => TypePolynomialOpt, &
                    ip => TypeQuadratureOpt, &
                    BaseInterpolation_, &
                    LagrangeInterpolation_, &
                    SerendipityInterpolation_, &
                    HermitInterpolation_, &
                    HierarchyInterpolation_, &
                    OrthogonalInterpolation_

IMPLICIT NONE

PRIVATE

PUBLIC :: ASSIGNMENT(=)
PUBLIC :: BaseInterpolation_ToInteger
PUBLIC :: BaseInterpolation_FromInteger
PUBLIC :: BaseInterpolation_FromString
PUBLIC :: BaseInterpolationPointer_FromString
PUBLIC :: BaseType_ToInteger

PUBLIC :: BaseInterpolation_ToString
PUBLIC :: BaseType_ToChar
PUBLIC :: BaseInterpolation_ToChar

INTERFACE BaseInterpolation_ToInteger
  MODULE PROCEDURE BaseInterpolation_ToInteger1
  MODULE PROCEDURE BaseInterpolation_ToInteger2
END INTERFACE BaseInterpolation_ToInteger

INTERFACE BaseType_ToInteger
  MODULE PROCEDURE BaseInterpolation_ToInteger1
  MODULE PROCEDURE BaseType_ToInteger1
END INTERFACE BaseType_ToInteger

INTERFACE BaseInterpolation_ToString
  MODULE PROCEDURE BaseInterpolation_ToString1
  MODULE PROCEDURE BaseInterpolation_ToString2
END INTERFACE BaseInterpolation_ToString

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE BaseInterpolation_Copy
END INTERFACE

CONTAINS

!----------------------------------------------------------------------------
!                                       BaseInterpolationPointer_FromString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-18
! summary: This routine returns a pointer to a child of BaseInterpolation_

FUNCTION BaseInterpolationPointer_FromString(name) RESULT(Ans)
  CHARACTER(*), INTENT(IN) :: name
  CLASS(BaseInterpolation_), POINTER :: ans

  CHARACTER(LEN=4) :: astr

  astr = UpperCase(name(1:4))

  SELECT CASE (astr)

  CASE ("LAGR")
    ALLOCATE (LagrangeInterpolation_ :: ans)

  CASE ("SERE")
    ALLOCATE (SerendipityInterpolation_ :: ans)

  CASE ("HERM")
    ALLOCATE (HermitInterpolation_ :: ans)

  CASE ("HIER", "HEIR")
    ALLOCATE (HierarchyInterpolation_ :: ans)

  CASE ("ORTH")
    ALLOCATE (OrthogonalInterpolation_ :: ans)

  CASE DEFAULT
    CALL ErrorMsg(msg="NO CASE FOUND for type of name="//astr, &
                  routine="BaseInterpolationPointer_FromString()", &
                  unitno=stdout, line=__LINE__, file=__FILE__)
    STOP
  END SELECT

END FUNCTION BaseInterpolationPointer_FromString

!----------------------------------------------------------------------------
!                                                 BaseInterpolation_Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Copy BaseInterpolation

SUBROUTINE BaseInterpolation_Copy(obj1, obj2)
  CLASS(BaseInterpolation_), ALLOCATABLE, INTENT(INOUT) :: obj1
  CLASS(BaseInterpolation_), INTENT(IN) :: obj2

  IF (ALLOCATED(obj1)) THEN
    DEALLOCATE (obj1)
  END IF

  ALLOCATE (obj1, source=obj2)

END SUBROUTINE BaseInterpolation_Copy

!----------------------------------------------------------------------------
!                                                BaseInterpolation_toInteger
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

FUNCTION BaseInterpolation_ToInteger1(obj) RESULT(ans)
  CLASS(BaseInterpolation_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans

  SELECT TYPE (obj)
  CLASS IS (LagrangeInterpolation_)
    ans = poly%lagrange

  CLASS IS (SerendipityInterpolation_)
    ans = poly%serendipity

  CLASS IS (HermitInterpolation_)
    ans = poly%hermit

  CLASS IS (HierarchyInterpolation_)
    ans = poly%hierarchical

  CLASS IS (OrthogonalInterpolation_)
    ans = poly%orthogonal

  CLASS DEFAULT
    CALL ErrorMsg(msg="NO CASE FOUND for type of obj2", &
                  routine="BaseInterpolation_toInteger()", &
                  line=__LINE__, unitno=stdout, file=__FILE__)

    STOP

  END SELECT
END FUNCTION BaseInterpolation_ToInteger1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION BaseType_ToInteger1(name) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans

  CHARACTER(4) :: astr

  astr = UpperCase(name(1:4))

  SELECT CASE (astr)
  CASE ("MONO")
    ans = poly%monomial

  CASE ("LAGR")
    ans = poly%lagrange

  CASE ("SERE")
    ans = poly%serendipity

  CASE ("HERM")
    ans = poly%hermit

  CASE ("HIER", "HEIR")
    ans = poly%hierarchical

  CASE ("ORTH")
    ans = poly%orthogonal

  CASE ("LEGE")
    ans = poly%legendre

  CASE ("JACO")
    ans = poly%jacobi

  CASE ("ULTR")
    ans = poly%ultraspherical

  CASE ("CHEB")
    ans = poly%chebyshev

  CASE DEFAULT
    CALL ErrorMsg(msg="NO CASE FOUND for name: "//astr, &
                  routine="BaseType_ToInteger1()", &
                  line=__LINE__, unitno=stdout, file=__FILE__)
    STOP

  END SELECT
END FUNCTION BaseType_ToInteger1

!----------------------------------------------------------------------------
!                                                BaseInterpolation_toInteger
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

FUNCTION BaseInterpolation_ToInteger2(name) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans

  CHARACTER(:), ALLOCATABLE :: astr

  astr = UpperCase(name)

  SELECT CASE (astr)

  CASE ("EQUIDISTANCE")
    ans = ip%equidistance

  CASE ("GAUSSLEGENDRE")
    ans = ip%GaussLegendre

  CASE ("GAUSSLEGENDRELOBATTO")
    ans = ip%GaussLegendreLobatto

  CASE ("GAUSSLEGENDRERADAU")
    ans = ip%GaussLegendreRadau

  CASE ("GAUSSLEGENDRERADAULEFT")
    ans = ip%GaussLegendreRadauLeft

  CASE ("GAUSSLEGENDRERADAURIGHT")
    ans = ip%GaussLegendreRadauRight

  CASE ("GAUSSCHEBYSHEV")
    ans = ip%GaussChebyshev

  CASE ("GAUSSCHEBYSHEVLOBATTO")
    ans = ip%GaussChebyshevLobatto

  CASE ("GAUSSCHEBYSHEVRADAU")
    ans = ip%GaussChebyshevRadau

  CASE ("GAUSSCHEBYSHEVRADAULEFT")
    ans = ip%GaussChebyshevRadauLeft

  CASE ("GAUSSCHEBYSHEVRADAURIGHT")
    ans = ip%GaussChebyshevRadauRight

  CASE ("GAUSSJACOBI")
    ans = ip%GaussJacobi

  CASE ("GAUSSJACOBILOBATTO")
    ans = ip%GaussJacobiLobatto

  CASE ("GAUSSJACOBIRADAU")
    ans = ip%GaussJacobiRadau

  CASE ("GAUSSJACOBIRADAULEFT")
    ans = ip%GaussJacobiRadauLeft

  CASE ("GAUSSJACOBIRADAURIGHT")
    ans = ip%GaussJacobiRadauRight

  CASE ("GAUSSULTRASPHERICAL")
    ans = ip%GaussUltraspherical

  CASE ("GAUSSULTRASPHERICALLOBATTO")
    ans = ip%GaussUltrasphericalLobatto

  CASE ("GAUSSULTRASPHERICALRADAU")
    ans = ip%GaussUltrasphericalRadau

  CASE ("GAUSSULTRASPHERICALRADAULEFT")
    ans = ip%GaussUltrasphericalRadauLeft

  CASE ("GAUSSULTRASPHERICALRADAURIGHT")
    ans = ip%GaussUltrasphericalRadauRight

  CASE DEFAULT

    ans = -1_I4B
    ! CALL Errormsg(msg="No case found for baseInterpolation ="//name, &
    !               routine="BaseInterpolation_ToInteger2()", &
    !               file=__FILE__, line=__LINE__, unitno=stderr)
    ! STOP
  END SELECT

  astr = ""
END FUNCTION BaseInterpolation_ToInteger2

!----------------------------------------------------------------------------
!                                             BaseInterpolation_fromInteger
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

SUBROUTINE BaseInterpolation_FromInteger(obj, name)
  CLASS(BaseInterpolation_), ALLOCATABLE, INTENT(OUT) :: obj
  INTEGER(I4B), INTENT(IN) :: name

  SELECT CASE (name)
  CASE (poly%lagrange)
    ALLOCATE (LagrangeInterpolation_ :: obj)

  CASE (poly%serendipity)
    ALLOCATE (SerendipityInterpolation_ :: obj)

  CASE (poly%hermit)
    ALLOCATE (HermitInterpolation_ :: obj)

  CASE (poly%orthogonal)
    ALLOCATE (OrthogonalInterpolation_ :: obj)

  CASE (poly%hierarchical)
    ALLOCATE (HierarchyInterpolation_ :: obj)

  CASE DEFAULT
    CALL ErrorMsg(msg="NO CASE FOUND for given name="//tostring(name), &
                  routine="BaseInterpolation_fromInteger()", &
                  line=__LINE__, unitno=stdout, file=__FILE__)
    STOP
  END SELECT

END SUBROUTINE BaseInterpolation_FromInteger

!----------------------------------------------------------------------------
!                                               BaseInterpolation_fromString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

SUBROUTINE BaseInterpolation_FromString(obj, name)
  CLASS(BaseInterpolation_), ALLOCATABLE, INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: name

  CHARACTER(4) :: ans

  ans = UpperCase(name(1:4))

  IF (ALLOCATED(obj)) DEALLOCATE (obj)

  SELECT CASE (ans)

  CASE ("LAGR")
    ALLOCATE (LagrangeInterpolation_ :: obj)

  CASE ("SERE")
    ALLOCATE (SerendipityInterpolation_ :: obj)

  CASE ("HERM")
    ALLOCATE (HermitInterpolation_ :: obj)

  CASE ("HIER", "HEIR")
    ALLOCATE (HierarchyInterpolation_ :: obj)

  CASE ("ORTH")
    ALLOCATE (OrthogonalInterpolation_ :: obj)

  CASE DEFAULT
    CALL ErrorMsg(msg="NO CASE FOUND for type of name="//name, &
                  routine="BaseInterpolation_fromString()", &
                  line=__LINE__, unitno=stderr, file=__FILE__)
    STOP
  END SELECT

END SUBROUTINE BaseInterpolation_FromString

!----------------------------------------------------------------------------
!                                                 BaseInterpolation_toString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

FUNCTION BaseInterpolation_ToString1(obj) RESULT(ans)
  CLASS(BaseInterpolation_), INTENT(IN) :: obj
  TYPE(String) :: ans

  SELECT TYPE (obj)
  CLASS IS (LagrangeInterpolation_)
    ans = "LagrangeInterpolation"

  CLASS IS (SerendipityInterpolation_)
    ans = "SerendipityInterpolation"

  CLASS IS (HermitInterpolation_)
    ans = "HermitInterpolation"

  CLASS IS (HierarchyInterpolation_)
    ans = "HierarchyInterpolation"

  CLASS IS (OrthogonalInterpolation_)
    ans = "OrthogonalInterpolation"

  CLASS DEFAULT
    CALL ErrorMsg(msg="No Case Found For Type of obj2", &
                  routine="BaseInterpolation_ToString1()", &
                  line=__LINE__, unitno=stdout, file=__FILE__)
    STOP
  END SELECT

END FUNCTION BaseInterpolation_ToString1

!----------------------------------------------------------------------------
!                                                           BaseType_ToChar
!----------------------------------------------------------------------------

FUNCTION BaseType_ToChar(name) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: name
  CHARACTER(:), ALLOCATABLE :: ans

  SELECT CASE (name)
  CASE (poly%monomial)
    ans = "Monomial"

  CASE (poly%lagrange)
    ans = "LagrangeInterpolation"

  CASE (poly%serendipity)
    ans = "SerendipityInterpolation"

  CASE (poly%hermit)
    ans = "HermitInterpolation"

  CASE (poly%hierarchical)
    ans = "HierarchyInterpolation"

  CASE (poly%orthogonal)
    ans = "OrthogonalInterpolation"

  CASE (poly%legendre)
    ans = "LegendreInterpolation"

  CASE (poly%jacobi)
    ans = "JacobiInterpolation"

  CASE (poly%ultraspherical)
    ans = "UltrasphericalInterpolation"

  CASE (poly%chebyshev)
    ans = "ChebyshevInterpolation"

  CASE DEFAULT
    CALL ErrorMsg(msg="No Case Found For name "//tostring(name), &
                  routine="BaseType_ToChar()", &
                  line=__LINE__, unitno=stdout, file=__FILE__)
    STOP
  END SELECT

END FUNCTION BaseType_ToChar

!----------------------------------------------------------------------------
!                                                   QuadraturePointIDToName
!----------------------------------------------------------------------------

FUNCTION BaseInterpolation_ToString2(name) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: name
  TYPE(String) :: ans
  ans = BaseInterpolation_ToChar(name)
END FUNCTION BaseInterpolation_ToString2

!----------------------------------------------------------------------------
!                                                   BaseInterpolation_ToChar
!----------------------------------------------------------------------------

FUNCTION BaseInterpolation_ToChar(name) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: name
  CHARACTER(:), ALLOCATABLE :: ans

  SELECT CASE (name)
  CASE (ip%equidistance)
    ans = "Equidistance"

  CASE (ip%GaussLegendre)
    ans = "GaussLegendre"

  CASE (ip%GaussLegendreLobatto)
    ans = "GaussLegendreLobatto"

  CASE (ip%GaussLegendreRadau)
    ans = "GaussLegendreRadau"

  CASE (ip%GaussLegendreRadauLeft)
    ans = "GaussLegendreRadauLeft"

  CASE (ip%GaussLegendreRadauRight)
    ans = "GaussLegendreRadauRight"

  CASE (ip%GaussChebyshev)
    ans = "GaussChebyshev"

  CASE (ip%GaussChebyshevLobatto)
    ans = "GaussChebyshevLobatto"

  CASE (ip%GaussChebyshevRadau)
    ans = "GaussChebyshevRadau"

  CASE (ip%GaussChebyshevRadauLeft)
    ans = "GaussChebyshevRadauLeft"

  CASE (ip%GaussChebyshevRadauRight)
    ans = "GaussChebyshevRadauRight"

  CASE (ip%GaussJacobi)
    ans = "GaussJacobi"

  CASE (ip%GaussJacobiLobatto)
    ans = "GaussJacobiLobatto"

  CASE (ip%GaussJacobiRadau)
    ans = "GaussJacobiRadau"

  CASE (ip%GaussJacobiRadauLeft)
    ans = "GaussJacobiRadauLeft"

  CASE (ip%GaussJacobiRadauRight)
    ans = "GaussJacobiRadauRight"

  CASE (ip%GaussUltraspherical)
    ans = "GaussUltraspherical"

  CASE (ip%GaussUltrasphericalLobatto)
    ans = "GaussUltrasphericalLobatto"

  CASE (ip%GaussUltrasphericalRadau)
    ans = "GaussUltrasphericalRadau"

  CASE (ip%GaussUltrasphericalRadauLeft)
    ans = "GaussUltrasphericalRadauLeft"

  CASE (ip%GaussUltrasphericalRadauRight)
    ans = "GaussUltrasphericalRadauRight"

  CASE DEFAULT
    CALL Errormsg(msg="No case found for given quadratureType name", &
                  routine="BaseInterpolation_ToChar()", &
                  file=__FILE__, line=__LINE__, unitno=stderr)
    ans = ""
    STOP
  END SELECT

END FUNCTION BaseInterpolation_ToChar

END MODULE BaseInterpolation_Method
