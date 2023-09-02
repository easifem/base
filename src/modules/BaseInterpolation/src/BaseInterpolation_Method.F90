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
PUBLIC :: BaseInterpolation_ToInteger
PUBLIC :: BaseInterpolation_FromInteger
PUBLIC :: BaseInterpolation_ToString
PUBLIC :: BaseInterpolation_FromString
PUBLIC :: BaseInterpolationPointer_FromString

INTERFACE BaseInterpolation_ToInteger
  MODULE PROCEDURE BaseInterpolation_ToInteger1
  MODULE PROCEDURE BaseInterpolation_ToInteger2
END INTERFACE BaseInterpolation_ToInteger

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
  !!
  TYPE(String) :: astr
  astr = TRIM(UpperCase(name))

  SELECT CASE (astr%chars())
  CASE ("LAGRANGEPOLYNOMIAL", "LAGRANGE", "LAGRANGEINTERPOLATION")
    ALLOCATE (LagrangeInterpolation_ :: ans)
  CASE ("SERENDIPITYPOLYNOMIAL", "SERENDIPITY", "SERENDIPITYINTERPOLATION")
    ALLOCATE (SerendipityInterpolation_ :: ans)
  CASE ("HERMITPOLYNOMIAL", "HERMIT", "HERMITINTERPOLATION")
    ALLOCATE (HermitInterpolation_ :: ans)
  CASE ( &
    & "HIERARCHICALPOLYNOMIAL", &
    & "HIERARCHY", &
    & "HEIRARCHICALPOLYNOMIAL", &
    & "HEIRARCHY", &
    & "HIERARCHYINTERPOLATION", &
    & "HEIRARCHYINTERPOLATION")
    ALLOCATE (HierarchyInterpolation_ :: ans)
  CASE ("ORTHOGONALPOLYNOMIAL", "ORTHOGONAL", "ORTHOGONALINTERPOLATION")
    ALLOCATE (OrthogonalInterpolation_ :: ans)
  CASE DEFAULT
    CALL ErrorMsg(&
    & msg="NO CASE FOUND for type of name="//astr, &
    & line=__LINE__,  &
    & unitno=stdout, &
    & routine="BaseInterpolationPointer_FromString()",  &
    & file=__FILE__ &
    & )
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
    CALL ErrorMsg(&
    & msg="NO CASE FOUND for type of obj2", &
    & line=__LINE__,  &
    & unitno=stdout, &
    & routine="BaseInterpolation_tostring()",  &
    & file=__FILE__ &
    & )
  END SELECT
END FUNCTION BaseInterpolation_ToString1

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
END FUNCTION BaseInterpolation_ToInteger1

!----------------------------------------------------------------------------
!                                                BaseInterpolation_toInteger
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

FUNCTION BaseInterpolation_ToInteger2(name) RESULT(ans)
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
END FUNCTION BaseInterpolation_ToInteger2

!----------------------------------------------------------------------------
!                                                 BaseInterpolation_fromString
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

SUBROUTINE BaseInterpolation_FromString(obj, name)
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
END SUBROUTINE BaseInterpolation_FromString

!----------------------------------------------------------------------------
!                                                BaseInterpolation_fromInteger
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-09
! summary:  Returns a string name of base interpolation type

SUBROUTINE BaseInterpolation_FromInteger(obj, name)
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

END SUBROUTINE BaseInterpolation_FromInteger

!----------------------------------------------------------------------------
!                                                  QuadraturePointIDToName
!----------------------------------------------------------------------------

FUNCTION BaseInterpolation_ToString2(name) RESULT(ans)
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
END FUNCTION BaseInterpolation_ToString2

END MODULE BaseInterpolation_Method
