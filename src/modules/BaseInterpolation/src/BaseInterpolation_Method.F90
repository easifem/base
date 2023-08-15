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
USE GlobalData
USE String_Class, ONLY: String
USE BaseType
USE Utility, ONLY: UpperCase
USE Display_Method, ONLY: Tostring
IMPLICIT NONE
PRIVATE
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: BaseInterpolation_toInteger
PUBLIC :: BaseInterpolation_fromInteger
PUBLIC :: BaseInterpolation_toString
PUBLIC :: BaseInterpolation_fromString

INTERFACE BaseInterpolation_toInteger
  MODULE PROCEDURE BaseInterpolation_toInteger1
  MODULE PROCEDURE BaseInterpolation_toInteger2
END INTERFACE BaseInterpolation_toInteger

INTERFACE BaseInterpolation_toString
  MODULE PROCEDURE BaseInterpolation_toString1
  MODULE PROCEDURE BaseInterpolation_toString2
END INTERFACE BaseInterpolation_toString

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE BaseInterpolation_Copy
END INTERFACE

CONTAINS

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

  SELECT TYPE (obj2)
  CLASS IS (LagrangeInterpolation_)
    ALLOCATE (LagrangeInterpolation_ :: obj1)
  CLASS IS (SerendipityInterpolation_)
    ALLOCATE (SerendipityInterpolation_ :: obj1)
  CLASS IS (HermitInterpolation_)
    ALLOCATE (HermitInterpolation_ :: obj1)
  CLASS IS (HierarchyInterpolation_)
    ALLOCATE (HierarchyInterpolation_ :: obj1)
  CLASS IS (OrthogonalInterpolation_)
    ALLOCATE (OrthogonalInterpolation_ :: obj1)
  CLASS DEFAULT
    CALL ErrorMsg(&
    & msg="NO CASE FOUND for type of obj2", &
    & line=__LINE__,  &
    & unitno=stdout, &
    & routine="BaseInterpolation_Copy()",  &
    & file=__FILE__ &
    & )

  END SELECT
END SUBROUTINE BaseInterpolation_Copy

!----------------------------------------------------------------------------
!                                                 BaseInterpolation_toString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

FUNCTION BaseInterpolation_toString1(obj) RESULT(ans)
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
    CALL ErrorMsg(&
    & msg="NO CASE FOUND for type of obj2", &
    & line=__LINE__,  &
    & unitno=stdout, &
    & routine="BaseInterpolation_tostring()",  &
    & file=__FILE__ &
    & )
  END SELECT
END FUNCTION BaseInterpolation_toString1

!----------------------------------------------------------------------------
!                                                BaseInterpolation_toInteger
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

FUNCTION BaseInterpolation_toInteger1(obj) RESULT(ans)
  CLASS(BaseInterpolation_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  SELECT TYPE (obj)
  CLASS IS (LagrangeInterpolation_)
    ans = LagrangePolynomial
  CLASS IS (SerendipityInterpolation_)
    ans = SerendipityPolynomial
  CLASS IS (HermitInterpolation_)
    ans = HermitPolynomial
  CLASS IS (HierarchyInterpolation_)
    ans = HeirarchicalPolynomial
  CLASS IS (OrthogonalInterpolation_)
    ans = OrthogonalPolynomial
  CLASS DEFAULT
    CALL ErrorMsg(&
    & msg="NO CASE FOUND for type of obj2", &
    & line=__LINE__,  &
    & unitno=stdout, &
    & routine="BaseInterpolation_toInteger()",  &
    & file=__FILE__ &
    & )
  END SELECT
END FUNCTION BaseInterpolation_toInteger1

!----------------------------------------------------------------------------
!                                                BaseInterpolation_toInteger
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

FUNCTION BaseInterpolation_toInteger2(name) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans

  SELECT CASE (TRIM(UpperCase(name)))
  CASE ("EQUIDISTANCE")
    ans = Equidistance

  CASE ("GAUSSLEGENDRE")
    ans = GaussLegendre

  CASE ("GAUSSLEGENDRELOBATTO")
    ans = GaussLegendreLobatto

  CASE ("GAUSSLEGENDRERADAU")
    ans = GaussLegendreRadau

  CASE ("GAUSSLEGENDRERADAULEFT")
    ans = GaussLegendreRadauLeft

  CASE ("GAUSSLEGENDRERADAURIGHT")
    ans = GaussLegendreRadauRight

  CASE ("GAUSSCHEBYSHEV")
    ans = GaussChebyshev

  CASE ("GAUSSCHEBYSHEVLOBATTO")
    ans = GaussChebyshevLobatto

  CASE ("GAUSSCHEBYSHEVRADAU")
    ans = GaussChebyshevRadau

  CASE ("GAUSSCHEBYSHEVRADAULEFT")
    ans = GaussChebyshevRadauLeft

  CASE ("GAUSSCHEBYSHEVRADAURIGHT")
    ans = GaussChebyshevRadauRight

  CASE ("GAUSSJACOBI")
    ans = GaussJacobi

  CASE ("GAUSSJACOBILOBATTO")
    ans = GaussJacobiLobatto

  CASE ("GAUSSJACOBIRADAU")
    ans = GaussJacobiRadau

  CASE ("GAUSSJACOBIRADAULEFT")
    ans = GaussJacobiRadauLeft

  CASE ("GAUSSJACOBIRADAURIGHT")
    ans = GaussJacobiRadauRight

  CASE ("GAUSSULTRASPHERICAL")
    ans = GaussUltraspherical

  CASE ("GAUSSULTRASPHERICALLOBATTO")
    ans = GaussUltrasphericalLobatto

  CASE ("GAUSSULTRASPHERICALRADAU")
    ans = GaussUltrasphericalRadau

  CASE ("GAUSSULTRASPHERICALRADAULEFT")
    ans = GaussUltrasphericalRadauLeft

  CASE ("GAUSSULTRASPHERICALRADAURIGHT")
    ans = GaussUltrasphericalRadauRight

  CASE DEFAULT
    ans = -1_I4B
    CALL Errormsg(&
      & msg="No case found for given quadratureType name", &
      & file=__FILE__, &
      & line=__LINE__,&
      & routine="QuadraturePointNameToID()", &
      & unitno=stderr)
    RETURN
  END SELECT
END FUNCTION BaseInterpolation_toInteger2

!----------------------------------------------------------------------------
!                                                 BaseInterpolation_fromString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

SUBROUTINE BaseInterpolation_fromString(obj, name)
  CLASS(BaseInterpolation_), ALLOCATABLE, INTENT(OUT) :: obj
  CHARACTER(*), INTENT(IN) :: name
  TYPE(String) :: ans

  ans = UpperCase(name)
  IF (ALLOCATED(obj)) DEALLOCATE (obj)

  SELECT CASE (ans%chars())
  CASE ("LAGRANGEPOLYNOMIAL", "LAGRANGE", "LAGRANGEINTERPOLATION")
    ALLOCATE (LagrangeInterpolation_ :: obj)
  CASE ("SERENDIPITYPOLYNOMIAL", "SERENDIPITY", "SERENDIPITYINTERPOLATION")
    ALLOCATE (SerendipityInterpolation_ :: obj)
  CASE ("HERMITPOLYNOMIAL", "HERMIT", "HERMITINTERPOLATION")
    ALLOCATE (HermitInterpolation_ :: obj)
  CASE ( &
    & "HIERARCHICALPOLYNOMIAL", &
    & "HIERARCHY", &
    & "HEIRARCHICALPOLYNOMIAL", &
    & "HEIRARCHY", &
    & "HIERARCHYINTERPOLATION", &
    & "HEIRARCHYINTERPOLATION")
    ALLOCATE (HierarchyInterpolation_ :: obj)
  CASE ("ORTHOGONALPOLYNOMIAL", "ORTHOGONAL", "ORTHOGONALINTERPOLATION")
    ALLOCATE (OrthogonalInterpolation_ :: obj)
  CASE DEFAULT
    CALL ErrorMsg(&
    & msg="NO CASE FOUND for type of name="//TRIM(name), &
    & line=__LINE__,  &
    & unitno=stdout, &
    & routine="BaseInterpolation_fromString()",  &
    & file=__FILE__ &
    & )
  END SELECT
END SUBROUTINE BaseInterpolation_fromString

!----------------------------------------------------------------------------
!                                                BaseInterpolation_fromInteger
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

SUBROUTINE BaseInterpolation_fromInteger(obj, name)
  CLASS(BaseInterpolation_), ALLOCATABLE, INTENT(OUT) :: obj
  INTEGER(I4B), INTENT(IN) :: name

  SELECT CASE (name)
  CASE (LagrangePolynomial)
    ALLOCATE (LagrangeInterpolation_ :: obj)
  CASE (SerendipityPolynomial)
    ALLOCATE (SerendipityInterpolation_ :: obj)
  CASE (HermitPolynomial)
    ALLOCATE (HermitInterpolation_ :: obj)
  CASE (OrthogonalPolynomial)
    ALLOCATE (OrthogonalInterpolation_ :: obj)
  CASE (HeirarchicalPolynomial)
    ALLOCATE (HierarchyInterpolation_ :: obj)
  CASE DEFAULT
    CALL ErrorMsg(&
    & msg="NO CASE FOUND for given name="//tostring(name), &
    & line=__LINE__,  &
    & unitno=stdout, &
    & routine="BaseInterpolation_fromInteger()",  &
    & file=__FILE__ &
    & )
  END SELECT

END SUBROUTINE BaseInterpolation_fromInteger

!----------------------------------------------------------------------------
!                                                  QuadraturePointIDToName
!----------------------------------------------------------------------------

FUNCTION BaseInterpolation_toString2(name) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: name
  TYPE(String) :: ans

  SELECT CASE (name)
  CASE (Equidistance)
    ans = "EQUIDISTANCE"

  CASE (GaussLegendre)
    ans = "GAUSSLEGENDRE"

  CASE (GaussLegendreLobatto)
    ans = "GAUSSLEGENDRELOBATTO"

  CASE (GaussLegendreRadau)
    ans = "GAUSSLEGENDRERADAU"

  CASE (GaussLegendreRadauLeft)
    ans = "GAUSSLEGENDRERADAULEFT"

  CASE (GaussLegendreRadauRight)
    ans = "GAUSSLEGENDRERADAURIGHT"

  CASE (GaussChebyshev)
    ans = "GAUSSCHEBYSHEV"

  CASE (GaussChebyshevLobatto)
    ans = "GAUSSCHEBYSHEVLOBATTO"

  CASE (GaussChebyshevRadau)
    ans = "GAUSSCHEBYSHEVRADAU"

  CASE (GaussChebyshevRadauLeft)
    ans = "GAUSSCHEBYSHEVRADAULEFT"

  CASE (GaussChebyshevRadauRight)
    ans = "GAUSSCHEBYSHEVRADAURIGHT"

  CASE (GaussJacobi)
    ans = "GAUSSJACOBI"

  CASE (GaussJacobiLobatto)
    ans = "GAUSSJACOBILOBATTO"

  CASE (GaussJacobiRadau)
    ans = "GAUSSJACOBIRADAU"

  CASE (GaussJacobiRadauLeft)
    ans = "GAUSSJACOBIRADAULEFT"

  CASE (GaussJacobiRadauRight)
    ans = "GAUSSJACOBIRADAURIGHT"

  CASE (GaussUltraspherical)
    ans = "GAUSSULTRASPHERICAL"

  CASE (GaussUltrasphericalLobatto)
    ans = "GAUSSULTRASPHERICALLOBATTO"

  CASE (GaussUltrasphericalRadau)
    ans = "GAUSSULTRASPHERICALRADAU"

  CASE (GaussUltrasphericalRadauLeft)
    ans = "GAUSSULTRASPHERICALRADAULEFT"

  CASE (GaussUltrasphericalRadauRight)
    ans = "GAUSSULTRASPHERICALRADAURIGHT"

  CASE DEFAULT
    CALL Errormsg(&
      & msg="No case found for given quadratureType name", &
      & file=__FILE__, &
      & line=__LINE__,&
      & routine="QuadraturePointIDToName()", &
      & unitno=stderr)
    RETURN
  END SELECT
END FUNCTION BaseInterpolation_toString2

END MODULE BaseInterpolation_Method
