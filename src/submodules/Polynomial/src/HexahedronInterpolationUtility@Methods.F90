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

SUBMODULE(HexahedronInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                LagrangeDegree_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Hexahedron

! TODO #163 Implement LagrangeDegree_Hexahedron

END PROCEDURE LagrangeDegree_Hexahedron

!----------------------------------------------------------------------------
!                                                    LagrangeDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Hexahedron
ans = (order + 1)**3
END PROCEDURE LagrangeDOF_Hexahedron

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Hexahedron
ans = (order - 1)**3
END PROCEDURE LagrangeInDOF_Hexahedron

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Hexahedron
! ALLOCATE (nodecoord(3, 8))
! nodecoord(:, 1) = [-1, -1, -1]
! nodecoord(:, 2) = [1, -1, -1]
! nodecoord(:, 3) = [1, 1, -1]
! nodecoord(:, 4) = [-1, 1, -1]
! nodecoord(:, 5) = [-1, -1, 1]
! nodecoord(:, 6) = [1, -1, 1]
! nodecoord(:, 7) = [1, 1, 1]
! nodecoord(:, 8) = [-1, 1, 1]
! TODO #159 Implement EquidistancePoint_Hexahedron routine
END PROCEDURE EquidistancePoint_Hexahedron

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Hexahedron

! TODO #159 Implement EquidistanceInPoint_Hexahedron routine

END PROCEDURE EquidistanceInPoint_Hexahedron

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Hexahedron
SELECT CASE (ipType)
CASE (Equidistance)
  nodecoord = EquidistancePoint_Hexahedron(xij=xij, order=order)
CASE (GaussLegendre)
CASE (GaussLegendreLobatto)
CASE (GaussChebyshev)
CASE (GaussChebyshevLobatto)
END SELECT
END PROCEDURE InterpolationPoint_Hexahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron1
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info
!!
ipiv = 0_I4B; ans = 0.0_DFP; ans(i) = 1.0_DFP
V = LagrangeVandermonde(order=order, xij=xij, elemType=Hexahedron)
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL LUSolve(A=V, B=ans, IPIV=ipiv, info=info)
!!
END PROCEDURE LagrangeCoeff_Hexahedron1

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron2
!!
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info
!!
vtemp = v; ans = 0.0_DFP; ans(i) = 1.0_DFP; ipiv = 0_I4B
CALL GetLU(A=vtemp, IPIV=ipiv, info=info)
CALL LUSolve(A=vtemp, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Hexahedron2

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron3
INTEGER(I4B) :: info
ans = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Hexahedron3

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Hexahedron4
ans = LagrangeVandermonde(order=order, xij=xij, elemType=Hexahedron)
CALL GetInvMat(ans)
END PROCEDURE LagrangeCoeff_Hexahedron4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
