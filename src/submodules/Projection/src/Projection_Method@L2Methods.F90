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

SUBMODULE(Projection_Method) L2Methods
USE BaseType, ONLY: math => TypeMathOpt
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: ToString
USE MassMatrix_Method, ONLY: MassMatrix_
USE ForceVector_Method, ONLY: ForceVector_
USE Lapack_Method, ONLY: GetLU, LUSolve, GetInvMat

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "Projection_Method@L2Methods"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                       GetL2ProjectionDOFValueFromQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetL2ProjectionDOFValueFromQuadrature1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetDOFValueFromQuadrature1()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: n1
#endif

INTEGER(I4B) :: info, nrow, ncol

#ifdef DEBUG_VER
n1 = SIZE(func)
isok = n1 .GE. elemsd%nips
CALL AssertError1(isok, myName, modName, __LINE__, &
             'Size of func='//ToString(n1)//' is lesser than elemsd%nips='// &
                  ToString(elemsd%nips))
#endif

CALL MassMatrix_( &
  N=elemsd%N, M=elemsd%N, js=elemsd%js, ws=elemsd%ws, &
  thickness=elemsd%thickness, nips=elemsd%nips, nns1=elemsd%nns, &
  nns2=elemsd%nns, skipVertices=skipVertices, tVertices=tVertices, &
  ans=massMat, nrow=nrow, ncol=ncol)

CALL ForceVector_( &
  N=elemsd%N, js=elemsd%js, ws=elemsd%ws, thickness=elemsd%thickness, &
  nips=elemsd%nips, nns=elemsd%nns, skipVertices=skipVertices, &
  tVertices=tVertices, ans=ans, tsize=tsize, c=func)

CALL GetLU(A=massMat(1:nrow, 1:ncol), IPIV=ipiv(1:tsize), info=info)

CALL LUSolve(A=massMat(1:nrow, 1:ncol), B=ans(1:tsize), &
             IPIV=ipiv(1:tsize), info=info)

END PROCEDURE obj_GetL2ProjectionDOFValueFromQuadrature1

!----------------------------------------------------------------------------
!                                       GetL2ProjectionDOFValueFromQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetL2ProjectionDOFValueFromQuadrature2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetDOFValueFromQuadrature2()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: n1
#endif

INTEGER(I4B) :: nns, nnt, tdof, i1, i2, j1, j2, info

nrow = 0
ncol = 0

#ifdef DEBUG_VER
n1 = SIZE(func, 1)
isok = n1 .GE. elemsd%nips
CALL AssertError1(isok, myName, modName, __LINE__, &
             'Size of func='//ToString(n1)//' is lesser than elemsd%nips='// &
                  ToString(elemsd%nips))
#endif

#ifdef DEBUG_VER
n1 = SIZE(func, 2)
isok = n1 .GE. timeElemsd%nips
CALL AssertError1(isok, myName, modName, __LINE__, &
         'Size of func='//ToString(n1)//' is lesser than timeElemsd%nips='// &
                  ToString(timeElemsd%nips))
#endif

nns = elemsd%nns
nnt = timeElemsd%nns
tdof = nns * nnt

massMat(1:tdof, 1:tdof) = 0.0_DFP

i1 = 1; i2 = nnt
j1 = 1; j2 = nnt

! Make space-time mass matrix and force vector

CALL GetLU(A=massMat(1:tdof, 1:tdof), IPIV=ipiv(1:tdof), info=info)

! CALL LUSolve(A=massMat(1:tdof, 1:tdof), B=ans(1:tdof), &
!              IPIV=ipiv(1:tdof), info=info)

END PROCEDURE obj_GetL2ProjectionDOFValueFromQuadrature2

!----------------------------------------------------------------------------
!                                                            Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE L2Methods

