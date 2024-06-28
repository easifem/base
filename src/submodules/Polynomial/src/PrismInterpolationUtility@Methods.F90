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
!                                                     EdgeConnectivity_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeConnectivity_Prism
CALL GetEdgeConnectivity_Prism(con=ans)
END PROCEDURE EdgeConnectivity_Prism

!----------------------------------------------------------------------------
!                                                 FacetConnectivity_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetConnectivity_Prism
!! ans
ans(:, 1) = [3, Triangle3, 1, 3, 2, 0]
ans(:, 2) = [4, Quadrangle4, 2, 3, 6, 5]
ans(:, 3) = [4, Quadrangle4, 1, 2, 5, 4]
ans(:, 4) = [4, Quadrangle4, 1, 4, 6, 3]
ans(:, 5) = [3, Triangle3, 4, 5, 6, 0]
END PROCEDURE FacetConnectivity_Prism

!----------------------------------------------------------------------------
!                                                       RefElemDomain_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemDomain_Prism
!FIX: Implement RefElemDomain_Prism
CALL Errormsg(&
  & msg="[WORK IN PROGRESS] We are working on it", &
  & file=__FILE__, &
  & line=__LINE__,&
  & routine="RefElemDomain_Prism()", &
  & unitno=stderr)
END PROCEDURE RefElemDomain_Prism

!----------------------------------------------------------------------------
!                                                     LagrangeDegree_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Prism

!ISSUE: #164 Implement LagrangeDegree_Prism

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
!                                                        GetTotalDOF_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalDOF_Prism
ans = (order + 1)**2 * (order + 2) / 2_I4B
END PROCEDURE GetTotalDOF_Prism

!----------------------------------------------------------------------------
!                                                    GetTotalInDOF_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalInDOF_Prism
ans = (order - 1)**2 * (order - 2) / 2_I4B
END PROCEDURE GetTotalInDOF_Prism

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
!ISSUE: #160 Implement EquidistancePoint_Prism routine
END PROCEDURE EquidistancePoint_Prism

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Prism
!FIX: Implement EquidistanceInPoint_Prism routine
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
!                                                  InterpolationPoint_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Prism_
CALL ErrorMsg(&
  & msg="InterpolationPoint_Prism_ is not implemented", &
  & file=__FILE__, &
  & routine="InterpolationPoint_Prism_", &
  & line=__LINE__, &
  & unitno=stderr)
END PROCEDURE InterpolationPoint_Prism_

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Prism1
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Prism1_(order=order, i=i, xij=xij, ans=ans, &
                           tsize=tsize)
END PROCEDURE LagrangeCoeff_Prism1

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Prism2
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Prism2_(order=order, i=i, v=v, &
                           isVandermonde=.TRUE., ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Prism2

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Prism3
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Prism3_(order=order, i=i, v=v, ipiv=ipiv, &
                           ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Prism3

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Prism4
INTEGER(I4B) :: nrow, ncol

CALL LagrangeCoeff_Prism4_(order=order, xij=xij, basisType=basisType, &
                   refPrism=refPrism, alpha=alpha, beta=beta, lambda=lambda, &
                           ans=ans, nrow=nrow, ncol=ncol)

END PROCEDURE LagrangeCoeff_Prism4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Prism1_
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info, nrow, ncol

tsize = SIZE(xij, 2)

ipiv = 0_I4B; ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP

CALL LagrangeVandermonde_(order=order, xij=xij, elemType=Prism, &
                          ans=V, nrow=nrow, ncol=ncol)

CALL GetLU(A=V, IPIV=ipiv, info=info)

CALL LUSolve(A=V, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Prism1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Prism2_
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info

tsize = SIZE(v, 1)

vtemp = v; ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP; ipiv = 0_I4B
CALL GetLU(A=vtemp, IPIV=ipiv, info=info)
CALL LUSolve(A=vtemp, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Prism2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Prism3_
INTEGER(I4B) :: info

tsize = SIZE(v, 1)

ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Prism3_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Prism4_
CALL LagrangeVandermonde_(order=order, xij=xij, ans=ans, nrow=nrow, &
                          ncol=ncol, elemType=Prism)
CALL GetInvMat(ans(1:nrow, 1:ncol))
END PROCEDURE LagrangeCoeff_Prism4_

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Prism1
! FIX: Implement QuadraturePoint_Prism1
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
! FIX: Implement QuadraturePoint_Prism2
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
!FIX: Implement TensorQuadraturePoint_Prism1
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
!FIX: Implement TensorQuadraturePoint_Prism2
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="TensorQuadraturePoint_Prism2()", &
& file=__FILE__)
END PROCEDURE TensorQuadraturePoint_Prism2

!----------------------------------------------------------------------------
!                                             LagrangeEvalAll_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Prism1
! FIX: Implement LagrangeEvalAll_Prism1
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="LagrangeEvalAll_Prism1()", &
& file=__FILE__)
END PROCEDURE LagrangeEvalAll_Prism1

!----------------------------------------------------------------------------
!                                             LagrangeEvalAll_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Prism2
! FIX: Implement LagrangeEvalAll_Prism2
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="LagrangeEvalAll_Prism2()", &
& file=__FILE__)
END PROCEDURE LagrangeEvalAll_Prism2

!----------------------------------------------------------------------------
!                                             LagrangeGradientEvalAll_Prism
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Prism1
!FIX: Implement LagrangeGradientEvalAll_Prism1
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="LagrangeGradientEvalAll_Prism1()", &
& file=__FILE__)
END PROCEDURE LagrangeGradientEvalAll_Prism1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
