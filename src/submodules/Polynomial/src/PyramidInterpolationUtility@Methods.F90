! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This programris free software: you can redistribute it and/or modify
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

SUBMODULE(PyramidInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     EdgeConnectivity_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeConnectivity_Pyramid
ans(:, 1) = [1, 2]
ans(:, 2) = [1, 4]
ans(:, 3) = [1, 5]
ans(:, 4) = [2, 3]
ans(:, 5) = [2, 5]
ans(:, 6) = [3, 4]
ans(:, 7) = [3, 5]
ans(:, 8) = [4, 5]
END PROCEDURE EdgeConnectivity_Pyramid

!----------------------------------------------------------------------------
!                                                 FacetConnectivity_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetConnectivity_Pyramid
ans(:, 1) = [4, Quadrangle4, 1, 4, 3, 2]
ans(:, 2) = [3, Triangle3, 2, 3, 5, 0]
ans(:, 3) = [3, Triangle3, 3, 4, 5, 0]
ans(:, 4) = [3, Triangle3, 1, 5, 4, 0]
ans(:, 5) = [3, Triangle3, 1, 2, 5, 0]
END PROCEDURE FacetConnectivity_Pyramid

!----------------------------------------------------------------------------
!                                                     RefElemDomain_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemDomain_Pyramid
!FIX: Implement RefElemDomain
CALL Errormsg(&
  & msg="[WORK IN PROGRESS] We are working on it", &
  & file=__FILE__, &
  & line=__LINE__,&
  & routine="RefElemDomain_Pyramid()", &
  & unitno=stderr)
END PROCEDURE RefElemDomain_Pyramid

!----------------------------------------------------------------------------
!                                                LagrangeDegree_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Pyramid
! ISSUE: #165 Implement LagrangeDegree_Pyramid
END PROCEDURE LagrangeDegree_Pyramid

!----------------------------------------------------------------------------
!                                                        LagrangeDOF_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Pyramid
ans = (order + 1) * (order + 2) * (2 * order + 3) / 6
END PROCEDURE LagrangeDOF_Pyramid

!----------------------------------------------------------------------------
!                                                    LagrangeInDOF_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Pyramid
ans = (order - 1) * (order - 2) * (2 * order - 3) / 6
END PROCEDURE LagrangeInDOF_Pyramid

!----------------------------------------------------------------------------
!                                                        GetTotalDOF_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalDOF_Pyramid
ans = (order + 1) * (order + 2) * (2 * order + 3) / 6
END PROCEDURE GetTotalDOF_Pyramid

!----------------------------------------------------------------------------
!                                                    GetTotalInDOF_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalInDOF_Pyramid
ans = (order - 1) * (order - 2) * (2 * order - 3) / 6
END PROCEDURE GetTotalInDOF_Pyramid

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Pyramid
!FIX: Implement EquidistancePoint_Pyramid
!ISSUE: #161 Implement EquidistancePoint_Pyramid routine
! nodecoord(:, 1) = [-1, -1, 0]
! nodecoord(:, 2) = [1, -1, 0]
! nodecoord(:, 3) = [1, 1, 0]
! nodecoord(:, 4) = [-1, 1, 0]
! nodecoord(:, 5) = [0, 0, 1]
END PROCEDURE EquidistancePoint_Pyramid

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Pyramid
! FIX: Implement EquidistanceInPoint_Pyramid
! ISSUE: #161 Implement EquidistanceInPoint_Pyramid routine

END PROCEDURE EquidistanceInPoint_Pyramid

!----------------------------------------------------------------------------
!                                                InterpolationPoint_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Pyramid
! FIX: Implement EquidistancePoint_Pyramid
SELECT CASE (ipType)
CASE (Equidistance)
  nodecoord = EquidistancePoint_Pyramid(xij=xij, order=order)
CASE (GaussLegendre)
CASE (GaussLegendreLobatto)
CASE (GaussChebyshev)
CASE (GaussChebyshevLobatto)
END SELECT
END PROCEDURE InterpolationPoint_Pyramid

!----------------------------------------------------------------------------
!                                                 InterpolationPoint_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Pyramid_
CALL ErrorMsg(&
  & msg="InterpolationPoint_Pyramid_ is not implemented", &
  & file=__FILE__, &
  & routine="InterpolationPoint_Pyramid_", &
  & line=__LINE__, &
  & unitno=stderr)
END PROCEDURE InterpolationPoint_Pyramid_

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Pyramid1
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Pyramid1_(order=order, i=i, xij=xij, ans=ans, &
                             tsize=tsize)
END PROCEDURE LagrangeCoeff_Pyramid1

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Pyramid2
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Pyramid2_(order=order, i=i, v=v, &
                             isVandermonde=.TRUE., ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Pyramid2

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Pyramid3
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Pyramid3_(order=order, i=i, v=v, ipiv=ipiv, &
                             ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Pyramid3

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Pyramid4
INTEGER(I4B) :: nrow, ncol

CALL LagrangeCoeff_Pyramid4_(order=order, xij=xij, basisType=basisType, &
               refPyramid=refPyramid, alpha=alpha, beta=beta, lambda=lambda, &
                             ans=ans, nrow=nrow, ncol=ncol)

END PROCEDURE LagrangeCoeff_Pyramid4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Pyramid1_
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info, nrow, ncol

tsize = SIZE(xij, 2)

ipiv = 0_I4B; ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP

CALL LagrangeVandermonde_(order=order, xij=xij, elemType=Pyramid, &
                          ans=V, nrow=nrow, ncol=ncol)

CALL GetLU(A=V, IPIV=ipiv, info=info)

CALL LUSolve(A=V, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Pyramid1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Pyramid2_
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info

tsize = SIZE(v, 1)

vtemp = v; ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP; ipiv = 0_I4B
CALL GetLU(A=vtemp, IPIV=ipiv, info=info)
CALL LUSolve(A=vtemp, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Pyramid2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Pyramid3_
INTEGER(I4B) :: info

tsize = SIZE(v, 1)

ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Pyramid3_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Pyramid4_
CALL LagrangeVandermonde_(order=order, xij=xij, ans=ans, nrow=nrow, &
                          ncol=ncol, elemType=Pyramid)
CALL GetInvMat(ans(1:nrow, 1:ncol))
END PROCEDURE LagrangeCoeff_Pyramid4_

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Pyramid1
!FIX: QuadraturePoint_Pyramid1
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="QuadraturePoint_Pyramid1()", &
& file=__FILE__)
END PROCEDURE QuadraturePoint_Pyramid1

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Pyramid2
!FIX: QuadraturePoint_Pyramid2
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="QuadraturePoint_Pyramid2()", &
& file=__FILE__)
END PROCEDURE QuadraturePoint_Pyramid2

!----------------------------------------------------------------------------
!                                             TensorQuadraturePoint_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorQuadraturePoint_Pyramid1
!FIX: TensorQuadraturePoint_Pyramid1
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="TensorQuadraturePoint_Pyramid1()", &
& file=__FILE__)
END PROCEDURE TensorQuadraturePoint_Pyramid1

!----------------------------------------------------------------------------
!                                             TensorQuadraturePoint_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorQuadraturePoint_Pyramid2
!FIX: TensorQuadraturePoint_Pyramid2
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="TensorQuadraturePoint_Pyramid2()", &
& file=__FILE__)
END PROCEDURE TensorQuadraturePoint_Pyramid2

!----------------------------------------------------------------------------
!                                             LagrangeEvalAll_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Pyramid1
!FIX: LagrangeEvalAll_Pyramid1
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="LagrangeEvalAll_Pyramid1()", &
& file=__FILE__)
END PROCEDURE LagrangeEvalAll_Pyramid1

!----------------------------------------------------------------------------
!                                             LagrangeEvalAll_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Pyramid2
!FIX: LagrangeEvalAll_Pyramid2
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="LagrangeEvalAll_Pyramid2()", &
& file=__FILE__)
END PROCEDURE LagrangeEvalAll_Pyramid2

!----------------------------------------------------------------------------
!                                          LagrangeGradientEvalAll_Pyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Pyramid1
!FIX: LagrangeGradientEvalAll_Pyramid1
CALL ErrorMsg(&
& msg="Work in progress",  &
& unitno=stdout,  &
& line=__LINE__,  &
& routine="LagrangeGradientEvalAll_Pyramid1()", &
& file=__FILE__)
END PROCEDURE LagrangeGradientEvalAll_Pyramid1

END SUBMODULE Methods
