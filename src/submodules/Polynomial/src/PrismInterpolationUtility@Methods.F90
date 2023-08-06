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

SUBMODULE(PrismInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     LagrangeDegree_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Prism

! TODO #164 Implement LagrangeDegree_Prism

END PROCEDURE LagrangeDegree_Prism

!----------------------------------------------------------------------------
!                                                        LagrangeDOF_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Prism
ans = (order + 1)**2 * (order + 2) / 2_I4B
END PROCEDURE LagrangeDOF_Prism

!----------------------------------------------------------------------------
!                                                    LagrangeInDOF_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Prism
ans = (order - 1)**2 * (order - 2) / 2_I4B
END PROCEDURE LagrangeInDOF_Prism

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Prism

! nodecoord( :, 1 ) = [0,0,-1]
! nodecoord( :, 2 ) = [1,0,-1]
! nodecoord( :, 3 ) = [0,1,-1]
! nodecoord( :, 4 ) = [0,0,1]
! nodecoord( :, 5 ) = [1,0,1]
! nodecoord( :, 6 ) = [0,1,1]
! TODO #160 Implement EquidistancePoint_Prism routine

END PROCEDURE EquidistancePoint_Prism

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Prism

! TODO Implement EquidistanceInPoint_Prism routine

END PROCEDURE EquidistanceInPoint_Prism

!----------------------------------------------------------------------------
!                                                  InterpolationPoint_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Prism
SELECT CASE (ipType)
CASE (Equidistance)
  nodecoord = EquidistancePoint_Prism(xij=xij, order=order)
CASE (GaussLegendre)
CASE (GaussLegendreLobatto)
CASE (GaussChebyshev)
CASE (GaussChebyshevLobatto)
END SELECT
END PROCEDURE InterpolationPoint_Prism

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Prism1
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info
!!
ipiv = 0_I4B; ans = 0.0_DFP; ans(i) = 1.0_DFP
V = LagrangeVandermonde(order=order, xij=xij, elemType=Prism)
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL LUSolve(A=V, B=ans, IPIV=ipiv, info=info)
!!
END PROCEDURE LagrangeCoeff_Prism1

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Prism2
!!
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info
!!
vtemp = v; ans = 0.0_DFP; ans(i) = 1.0_DFP; ipiv = 0_I4B
CALL GetLU(A=vtemp, IPIV=ipiv, info=info)
CALL LUSolve(A=vtemp, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Prism2

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Prism3
INTEGER(I4B) :: info
ans = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans, IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Prism3

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Prism4
ans = LagrangeVandermonde(order=order, xij=xij, elemType=Prism)
CALL GetInvMat(ans)
END PROCEDURE LagrangeCoeff_Prism4

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Prism1
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="QuadraturePoint_Prism1()", &
& file=__FILE__)
END PROCEDURE QuadraturePoint_Prism1

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Prism2
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="QuadraturePoint_Prism2()", &
& file=__FILE__)
END PROCEDURE QuadraturePoint_Prism2

!----------------------------------------------------------------------------
!                                             TensorQuadraturePoint_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorQuadraturePoint_Prism1
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="TensorQuadraturePoint_Prism1()", &
& file=__FILE__)
END PROCEDURE TensorQuadraturePoint_Prism1

!----------------------------------------------------------------------------
!                                             TensorQuadraturePoint_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorQuadraturePoint_Prism2
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="TensorQuadraturePoint_Prism2()", &
& file=__FILE__)
END PROCEDURE TensorQuadraturePoint_Prism2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
