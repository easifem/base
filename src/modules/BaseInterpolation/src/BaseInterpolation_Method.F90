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
PUBLIC :: BaseInterpolation_ToString
PUBLIC :: BaseInterpolation_ToChar

PUBLIC :: BaseType_ToChar
PUBLIC :: BaseType_ToInteger

PUBLIC :: InterpolationPoint_ToChar
PUBLIC :: InterpolationPoint_ToString
PUBLIC :: InterpolationPoint_ToInteger

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

FUNCTION BaseInterpolation_ToInteger(obj) RESULT(ans)
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
                  routine="BaseInterpolation_ToInteger()", &
                  line=__LINE__, unitno=stdout, file=__FILE__)

    STOP

  END SELECT
END FUNCTION BaseInterpolation_ToInteger

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION BaseType_ToInteger(name) RESULT(ans)
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
                  routine="BaseType_ToInteger()", &
                  line=__LINE__, unitno=stdout, file=__FILE__)
    STOP

  END SELECT
END FUNCTION BaseType_ToInteger

!----------------------------------------------------------------------------
!                                                BaseInterpolation_toInteger
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

FUNCTION InterpolationPoint_ToInteger(name) RESULT(ans)
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
END FUNCTION InterpolationPoint_ToInteger

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

FUNCTION BaseInterpolation_ToString(obj, isUpper) RESULT(ans)
  CLASS(BaseInterpolation_), INTENT(IN) :: obj
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
  TYPE(String) :: ans
  ans = BaseInterpolation_ToChar(obj=obj, isUpper=isUpper)
END FUNCTION BaseInterpolation_ToString

!----------------------------------------------------------------------------
!                                                   BaseInterpolation_ToChar
!----------------------------------------------------------------------------

FUNCTION BaseInterpolation_ToChar(obj, isUpper) RESULT(ans)
  CLASS(BaseInterpolation_), INTENT(IN) :: obj
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
  CHARACTER(:), ALLOCATABLE :: ans

  ! internal variables
  LOGICAL(LGT) :: isUpper0

  isUpper0 = .FALSE.
  IF (PRESENT(isUpper)) isUpper0 = isUpper

  SELECT TYPE (obj)
  CLASS IS (LagrangeInterpolation_)
    IF (isUpper0) THEN
      ans = "LAGRANGEINTERPOLATION"
    ELSE
      ans = "LagrangeInterpolation"
    END IF

  CLASS IS (SerendipityInterpolation_)
    IF (isUpper0) THEN
      ans = "SERENDIPITYINTERPOLATION"
    ELSE
      ans = "SerendipityInterpolation"
    END IF

  CLASS IS (HermitInterpolation_)
    IF (isUpper0) THEN
      ans = "HERMITINTERPOLATION"
    ELSE
      ans = "HermitInterpolation"
    END IF

  CLASS IS (HierarchyInterpolation_)
    IF (isUpper0) THEN
      ans = "HIERARCHYINTERPOLATION"
    ELSE
      ans = "HierarchyInterpolation"
    END IF

  CLASS IS (OrthogonalInterpolation_)
    IF (isUpper0) THEN
      ans = "ORTHOGONALINTERPOLATION"
    ELSE
      ans = "OrthogonalInterpolation"
    END IF

  CLASS DEFAULT
    ans = ""
    CALL ErrorMsg(msg="No Case Found For Type of obj2", &
                  routine="BaseInterpolation_ToString()", &
                  line=__LINE__, unitno=stdout, file=__FILE__)
    STOP
  END SELECT

END FUNCTION BaseInterpolation_ToChar

!----------------------------------------------------------------------------
!                                                           BaseType_ToChar
!----------------------------------------------------------------------------

FUNCTION BaseType_ToChar(name, isUpper) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: name
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
  CHARACTER(:), ALLOCATABLE :: ans

  ! internal variable
  LOGICAL(LGT) :: isUpper0

  isUpper0 = .FALSE.
  IF (PRESENT(isUpper)) isUpper0 = isUpper

  SELECT CASE (name)
  CASE (poly%monomial)
    IF (isUpper0) THEN
      ans = "MONOMIAL"
    ELSE
      ans = "Monomial"
    END IF

  CASE (poly%lagrange)
    IF (isUpper0) THEN
      ans = "LAGRANGEINTERPOLATION"
    ELSE
      ans = "LagrangeInterpolation"
    END IF

  CASE (poly%serendipity)
    IF (isUpper0) THEN
      ans = "SERENDIPITYINTERPOLATION"
    ELSE
      ans = "SerendipityInterpolation"
    END IF

  CASE (poly%hermit)
    IF (isUpper0) THEN
      ans = "HERMITINTERPOLATION"
    ELSE
      ans = "HermitInterpolation"
    END IF

  CASE (poly%hierarchical)
    IF (isUpper0) THEN
      ans = "HIERARCHYINTERPOLATION"
    ELSE
      ans = "HierarchyInterpolation"
    END IF

  CASE (poly%orthogonal)
    IF (isUpper0) THEN
      ans = "ORTHOGONALINTERPOLATION"
    ELSE
      ans = "OrthogonalInterpolation"
    END IF

  CASE (poly%legendre)
    IF (isUpper0) THEN
      ans = "LEGENDREINTERPOLATION"
    ELSE
      ans = "LegendreInterpolation"
    END IF

  CASE (poly%jacobi)
    IF (isUpper0) THEN
      ans = "JACOBIINTERPOLATION"
    ELSE
      ans = "JacobiInterpolation"
    END IF

  CASE (poly%ultraspherical)
    IF (isUpper0) THEN
      ans = "ULTRASPHERICALINTERPOLATION"
    ELSE
      ans = "UltrasphericalInterpolation"
    END IF

  CASE (poly%chebyshev)
    IF (isUpper0) THEN
      ans = "CHEBYSHEVINTERPOLATION"
    ELSE
      ans = "ChebyshevInterpolation"
    END IF

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

FUNCTION InterpolationPoint_ToString(name, isUpper) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: name
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
  TYPE(String) :: ans
  ans = InterpolationPoint_ToChar(name=name, isUpper=isUpper)
END FUNCTION InterpolationPoint_ToString

!----------------------------------------------------------------------------
!                                                   BaseInterpolation_ToChar
!----------------------------------------------------------------------------

FUNCTION InterpolationPoint_ToChar(name, isUpper) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: name
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isUpper
  CHARACTER(:), ALLOCATABLE :: ans

  ! internal varibles
  LOGICAL(LGT) :: isUpper0

  isUpper0 = .FALSE.
  IF (PRESENT(isUpper)) isUpper0 = isUpper

  SELECT CASE (name)
  CASE (ip%equidistance)
    IF (isUpper0) THEN
      ans = "EQUIDISTANCE"
    ELSE
      ans = "Equidistance"
    END IF

  CASE (ip%GaussLegendre)
    IF (isUpper0) THEN
      ans = "GAUSSLEGENDRE"
    ELSE
      ans = "GaussLegendre"
    END IF

  CASE (ip%GaussLegendreLobatto)
    IF (isUpper0) THEN
      ans = "GAUSSLEGENDRELOBATTO"
    ELSE
      ans = "GaussLegendreLobatto"
    END IF

  CASE (ip%GaussLegendreRadau)
    IF (isUpper0) THEN
      ans = "GAUSSLEGENDRERADAU"
    ELSE
      ans = "GaussLegendreRadau"
    END IF

  CASE (ip%GaussLegendreRadauLeft)
    IF (isUpper0) THEN
      ans = "GAUSSLEGENDRERADAULEFT"
    ELSE
      ans = "GaussLegendreRadauLeft"
    END IF

  CASE (ip%GaussLegendreRadauRight)
    IF (isUpper0) THEN
      ans = "GAUSSLEGENDRERADAURIGHT"
    ELSE
      ans = "GaussLegendreRadauRight"
    END IF

  CASE (ip%GaussChebyshev)
    IF (isUpper0) THEN
      ans = "GAUSSCHEBYSHEV"
    ELSE
      ans = "GaussChebyshev"
    END IF

  CASE (ip%GaussChebyshevLobatto)
    IF (isUpper0) THEN
      ans = "GAUSSCHEBYSHEVLOBATTO"
    ELSE
      ans = "GaussChebyshevLobatto"
    END IF

  CASE (ip%GaussChebyshevRadau)
    IF (isUpper0) THEN
      ans = "GAUSSCHEBYSHEVRADAU"
    ELSE
      ans = "GaussChebyshevRadau"
    END IF

  CASE (ip%GaussChebyshevRadauLeft)
    IF (isUpper0) THEN
      ans = "GAUSSCHEBYSHEVRADAULEFT"
    ELSE
      ans = "GaussChebyshevRadauLeft"
    END IF

  CASE (ip%GaussChebyshevRadauRight)
    IF (isUpper0) THEN
      ans = "GAUSSCHEBYSHEVRADAURIGHT"
    ELSE
      ans = "GaussChebyshevRadauRight"
    END IF

  CASE (ip%GaussJacobi)
    IF (isUpper0) THEN
      ans = "GAUSSJACOBI"
    ELSE
      ans = "GaussJacobi"
    END IF

  CASE (ip%GaussJacobiLobatto)
    IF (isUpper0) THEN
      ans = "GAUSSJACOBILOBATTO"
    ELSE
      ans = "GaussJacobiLobatto"
    END IF

  CASE (ip%GaussJacobiRadau)
    IF (isUpper0) THEN
      ans = "GAUSSJACOBIRADAU"
    ELSE
      ans = "GaussJacobiRadau"
    END IF

  CASE (ip%GaussJacobiRadauLeft)
    IF (isUpper0) THEN
      ans = "GAUSSJACOBIRADAULEFT"
    ELSE
      ans = "GaussJacobiRadauLeft"
    END IF

  CASE (ip%GaussJacobiRadauRight)
    IF (isUpper0) THEN
      ans = "GAUSSJACOBIRADAURIGHT"
    ELSE
      ans = "GaussJacobiRadauRight"
    END IF

  CASE (ip%GaussUltraspherical)
    IF (isUpper0) THEN
      ans = "GAUSSULTRASPHERICAL"
    ELSE
      ans = "GaussUltraspherical"
    END IF

  CASE (ip%GaussUltrasphericalLobatto)
    IF (isUpper0) THEN
      ans = "GAUSSULTRASPHERICALLOBATTO"
    ELSE
      ans = "GaussUltrasphericalLobatto"
    END IF

  CASE (ip%GaussUltrasphericalRadau)
    IF (isUpper0) THEN
      ans = "GAUSSULTRASPHERICALRADAU"
    ELSE
      ans = "GaussUltrasphericalRadau"
    END IF

  CASE (ip%GaussUltrasphericalRadauLeft)
    IF (isUpper0) THEN
      ans = "GAUSSULTRASPHERICALRADAULEFT"
    ELSE
      ans = "GaussUltrasphericalRadauLeft"
    END IF

  CASE (ip%GaussUltrasphericalRadauRight)
    IF (isUpper0) THEN
      ans = "GAUSSULTRASPHERICALRADAURIGHT"
    ELSE
      ans = "GaussUltrasphericalRadauRight"
    END IF

  CASE DEFAULT
    CALL Errormsg(msg="No case found for given quadratureType name", &
                  routine="BaseInterpolation_ToChar()", &
                  file=__FILE__, line=__LINE__, unitno=stderr)
    ans = ""
    STOP
  END SELECT

END FUNCTION InterpolationPoint_ToChar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BaseInterpolation_Method
