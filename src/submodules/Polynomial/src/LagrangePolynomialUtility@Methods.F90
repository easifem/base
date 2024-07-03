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

SUBMODULE(LagrangePolynomialUtility) Methods
USE GlobalData, ONLY: stdout, stderr, Point, Line, Triangle, Quadrangle, &
                      Tetrahedron, Hexahedron, Prism, Pyramid, Monomial

USE ErrorHandling, ONLY: Errormsg

USE ReferenceElement_Method, ONLY: ElementTopology, XiDimension

USE LineInterpolationUtility, ONLY: LagrangeDOF_Line, &
                                    LagrangeInDOF_Line, &
                                    LagrangeDegree_Line, &
                                    EquidistancePoint_Line, &
                                    EquidistancePoint_Line_, &
                                    InterpolationPoint_Line, &
                                    InterpolationPoint_Line_, &
                                    LagrangeCoeff_Line, &
                                    LagrangeCoeff_Line_, &
                                    LagrangeEvalAll_Line_, &
                                    LagrangeGradientEvalAll_Line_

USE TriangleInterpolationUtility, ONLY: LagrangeDOF_Triangle, &
                                        LagrangeInDOF_Triangle, &
                                        LagrangeDegree_Triangle, &
                                        EquidistancePoint_Triangle, &
                                        EquidistancePoint_Triangle_, &
                                        InterpolationPoint_Triangle, &
                                        InterpolationPoint_Triangle_, &
                                        LagrangeCoeff_Triangle, &
                                        LagrangeCoeff_Triangle_, &
                                        LagrangeEvalAll_Triangle_, &
                                        LagrangeGradientEvalAll_Triangle_

USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle, &
                                          LagrangeInDOF_Quadrangle, &
                                          LagrangeDegree_Quadrangle, &
                                          EquidistancePoint_Quadrangle, &
                                          EquidistancePoint_Quadrangle_, &
                                          InterpolationPoint_Quadrangle, &
                                          InterpolationPoint_Quadrangle_, &
                                          LagrangeCoeff_Quadrangle, &
                                          LagrangeCoeff_Quadrangle_, &
                                          LagrangeEvalAll_Quadrangle_, &
                                          LagrangeGradientEvalAll_Quadrangle_

USE TetrahedronInterpolationUtility, ONLY: LagrangeDOF_Tetrahedron, &
                                           LagrangeInDOF_Tetrahedron, &
                                           LagrangeDegree_Tetrahedron, &
                                           EquidistancePoint_Tetrahedron, &
                                           EquidistancePoint_Tetrahedron_, &
                                           InterpolationPoint_Tetrahedron, &
                                           InterpolationPoint_Tetrahedron_, &
                                           LagrangeCoeff_Tetrahedron, &
                                           LagrangeCoeff_Tetrahedron_, &
                                           LagrangeEvalAll_Tetrahedron_, &
                                          LagrangeGradientEvalAll_Tetrahedron_

USE HexahedronInterpolationUtility, ONLY: LagrangeDOF_Hexahedron, &
                                          LagrangeInDOF_Hexahedron, &
                                          LagrangeDegree_Hexahedron, &
                                          EquidistancePoint_Hexahedron, &
                                          EquidistancePoint_Hexahedron_, &
                                          InterpolationPoint_Hexahedron, &
                                          InterpolationPoint_Hexahedron_, &
                                          LagrangeCoeff_Hexahedron, &
                                          LagrangeCoeff_Hexahedron_, &
                                          LagrangeEvalAll_Hexahedron_, &
                                          LagrangeGradientEvalAll_Hexahedron_

USE PrismInterpolationUtility, ONLY: LagrangeDOF_Prism, &
                                     LagrangeInDOF_Prism, &
                                     LagrangeDegree_Prism, &
                                     EquidistancePoint_Prism, &
                                     EquidistancePoint_Prism_, &
                                     InterpolationPoint_Prism, &
                                     InterpolationPoint_Prism_, &
                                     LagrangeCoeff_Prism, &
                                     LagrangeCoeff_Prism_, &
                                     LagrangeEvalAll_Prism_, &
                                     LagrangeGradientEvalAll_Prism_

USE PyramidInterpolationUtility, ONLY: LagrangeDOF_Pyramid, &
                                       LagrangeInDOF_Pyramid, &
                                       LagrangeDegree_Pyramid, &
                                       EquidistancePoint_Pyramid, &
                                       EquidistancePoint_Pyramid_, &
                                       InterpolationPoint_Pyramid, &
                                       InterpolationPoint_Pyramid_, &
                                       LagrangeCoeff_Pyramid, &
                                       LagrangeCoeff_Pyramid_, &
                                       LagrangeEvalAll_Pyramid_, &
                                       LagrangeGradientEvalAll_Pyramid_

USE ReallocateUtility, ONLY: Reallocate

USE Display_Method, ONLY: ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               LagrangeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (Point)
  ans = 1
CASE (Line)
  ans = LagrangeDOF_Line(order=order)
CASE (Triangle)
  ans = LagrangeDOF_Triangle(order=order)
CASE (Quadrangle)
  ans = LagrangeDOF_Quadrangle(order=order)
CASE (Tetrahedron)
  ans = LagrangeDOF_Tetrahedron(order=order)
CASE (Hexahedron)
  ans = LagrangeDOF_Hexahedron(order=order)
CASE (Prism)
  ans = LagrangeDOF_Prism(order=order)
CASE (Pyramid)
  ans = LagrangeDOF_Pyramid(order=order)
END SELECT
END PROCEDURE LagrangeDOF

!----------------------------------------------------------------------------
!                                                             LagrangeInDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (Point)
  ans = 0
CASE (Line)
  ans = LagrangeInDOF_Line(order=order)
CASE (Triangle)
  ans = LagrangeInDOF_Triangle(order=order)
CASE (Quadrangle)
  ans = LagrangeInDOF_Quadrangle(order=order)
CASE (Tetrahedron)
  ans = LagrangeInDOF_Tetrahedron(order=order)
CASE (Hexahedron)
  ans = LagrangeInDOF_Hexahedron(order=order)
CASE (Prism)
  ans = LagrangeInDOF_Prism(order=order)
CASE (Pyramid)
  ans = LagrangeInDOF_Pyramid(order=order)
END SELECT

END PROCEDURE LagrangeInDOF

!----------------------------------------------------------------------------
!                                                             LagrangeDegree
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree
INTEGER(I4B) :: topo
topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (Point)
  ALLOCATE (ans(0, 0))
CASE (Line)
  ans = LagrangeDegree_Line(order=order)
CASE (Triangle)
  ans = LagrangeDegree_Triangle(order=order)
CASE (Quadrangle)
  ans = LagrangeDegree_Quadrangle(order=order)
CASE (Tetrahedron)
  ans = LagrangeDegree_Tetrahedron(order=order)
CASE (Hexahedron)
  ans = LagrangeDegree_Hexahedron(order=order)
CASE (Prism)
  ans = LagrangeDegree_Prism(order=order)
CASE (Pyramid)
  ans = LagrangeDegree_Pyramid(order=order)
END SELECT
END PROCEDURE LagrangeDegree

!----------------------------------------------------------------------------
!                                                       LagrangeVandermonde
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeVandermonde
INTEGER(I4B) :: nrow, ncol
nrow = SIZE(xij, 2)
ncol = LagrangeDOF(order=order, elemType=elemType)
CALL Reallocate(ans, nrow, ncol)
CALL LagrangeVandermonde_(xij=xij, order=order, elemType=elemType, ans=ans, &
                          nrow=nrow, ncol=ncol)
END PROCEDURE LagrangeVandermonde

!----------------------------------------------------------------------------
!                                                       LagrangeVandermonde_
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeVandermonde_
INTEGER(I4B), ALLOCATABLE :: degree(:, :)
INTEGER(I4B) :: jj, nsd, ii

degree = LagrangeDegree(order=order, elemType=elemType)
nrow = SIZE(xij, 2)
nsd = SIZE(degree, 2)
ncol = SIZE(degree, 1)

SELECT CASE (nsd)
CASE (1)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    ans(ii, jj) = xij(1, ii)**degree(jj, 1)
  END DO

CASE (2)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    ans(ii, jj) = xij(1, ii)**degree(jj, 1) * xij(2, ii)**degree(jj, 2)
  END DO

CASE (3)

  DO CONCURRENT(jj=1:ncol, ii=1:nrow)
    ans(ii, jj) = (xij(1, ii)**degree(jj, 1)) * (xij(2, ii)**degree(jj, 2))  &
                 & * (xij(3, ii)**degree(jj, 3))
  END DO

END SELECT

IF (ALLOCATED(degree)) DEALLOCATE (degree)
END PROCEDURE LagrangeVandermonde_

!----------------------------------------------------------------------------
!                                                          EquidistancePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint
INTEGER(I4B) :: nrow, ncol

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = XiDimension(elemType)
END IF

ncol = LagrangeDOF(order=order, elemType=elemType)

ALLOCATE (ans(nrow, ncol))

CALL EquidistancePoint_(order=order, elemType=elemType, ans=ans, nrow=nrow, &
                        ncol=ncol, xij=xij)

END PROCEDURE EquidistancePoint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = XiDimension(topo)
END IF

ncol = LagrangeDOF(order=order, elemType=elemType)

SELECT CASE (topo)

CASE (Point)

  IF (PRESENT(xij)) THEN
    ncol = 1
    ans(1:nrow, 1) = xij(1:nrow, 1)
  ELSE
    nrow = 0
    ncol = 0
    ! ALLOCATE (ans(0, 0))
  END IF

CASE (Line)
  ! ans(1:nrow, 1:ncol) = EquidistancePoint_Line(order=order, xij=xij)
  CALL EquidistancePoint_Line_(order=order, xij=xij, ans=ans, nrow=nrow, &
                               ncol=ncol)

CASE (Triangle)
  ! ans(1:nrow, 1:ncol) = EquidistancePoint_Triangle(order=order, xij=xij)
  CALL EquidistancePoint_Triangle_(order=order, xij=xij, nrow=nrow, &
                                   ncol=ncol, ans=ans)

CASE (Quadrangle)
  ! ans(1:nrow, 1:ncol) = EquidistancePoint_Quadrangle(order=order, xij=xij)
  CALL EquidistancePoint_Quadrangle_(order=order, xij=xij, ans=ans, &
                                     nrow=nrow, ncol=ncol)

CASE (Tetrahedron)
  ! ans(1:nrow, 1:ncol) = EquidistancePoint_Tetrahedron(order=order, xij=xij)
  CALL EquidistancePoint_Tetrahedron_(order=order, xij=xij, ans=ans, &
                                      nrow=nrow, ncol=ncol)

CASE (Hexahedron)
  CALL EquidistancePoint_Hexahedron_(order=order, xij=xij, ans=ans, &
                                     nrow=nrow, ncol=ncol)

CASE (Prism)
  CALL EquidistancePoint_Prism_(order=order, xij=xij, ans=ans, &
                                nrow=nrow, ncol=ncol)

CASE (Pyramid)
  CALL EquidistancePoint_Pyramid_(order=order, xij=xij, ans=ans, nrow=nrow, &
                                  ncol=ncol)

CASE DEFAULT
  CALL Errormsg(msg="No CASE FOUND: elemType="//ToString(elemType), &
                routine="EquidistancePoint()", &
                unitno=stdout, line=__LINE__, file=__FILE__)
  RETURN
END SELECT

END PROCEDURE EquidistancePoint_

!----------------------------------------------------------------------------
!                                                         InterpolationPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint
INTEGER(I4B) :: nrow, ncol

IF (PRESENT(xij)) THEN
  nrow = SIZE(Xij, 1)
ELSE
  nrow = XiDimension(elemType)
END IF

ncol = LagrangeDOF(order=order, elemType=elemType)
ALLOCATE (ans(nrow, ncol))

CALL InterpolationPoint_(order=order, elemType=elemType, ipType=ipType, &
     xij=xij, layout=layout, alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
                         nrow=nrow, ncol=ncol)

END PROCEDURE InterpolationPoint

!----------------------------------------------------------------------------
!                                                         InterpolationPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)

CASE (Point)

  IF (PRESENT(xij)) THEN
    nrow = SIZE(xij, 1)
    ncol = SIZE(xij, 2)
    ans(1:nrow, 1:ncol) = xij(1:nrow, 1:ncol)
    RETURN
  END IF

  nrow = 0
  ncol = 0

CASE (Line)
  CALL InterpolationPoint_Line_(order=order, ipType=ipType, ans=ans, &
       nrow=nrow, ncol=ncol, xij=xij, layout=layout, alpha=alpha, beta=beta, &
                                lambda=lambda)

CASE (Triangle)
  CALL InterpolationPoint_Triangle_(order=order, ipType=ipType, ans=ans, &
       nrow=nrow, ncol=ncol, xij=xij, layout=layout, alpha=alpha, beta=beta, &
                                    lambda=lambda)

CASE (Quadrangle)
  CALL InterpolationPoint_Quadrangle_(order=order, ipType=ipType, ans=ans, &
                  nrow=nrow, ncol=ncol, xij=xij, layout=layout, alpha=alpha, &
                                      beta=beta, lambda=lambda)

CASE (Tetrahedron)
  CALL InterpolationPoint_Tetrahedron_(order=order, ipType=ipType, ans=ans, &
                  nrow=nrow, ncol=ncol, xij=xij, layout=layout, alpha=alpha, &
                                       beta=beta, lambda=lambda)

CASE (Hexahedron)
  CALL InterpolationPoint_Hexahedron_(order=order, ipType=ipType, xij=xij, &
                               ans=ans, nrow=nrow, ncol=ncol, layout=layout, &
                                      alpha=alpha, beta=beta, lambda=lambda)

CASE (Prism)
  CALL InterpolationPoint_Prism_(order=order, ipType=ipType, xij=xij, &
                               ans=ans, nrow=nrow, ncol=ncol, layout=layout, &
                                 alpha=alpha, beta=beta, lambda=lambda)

CASE (Pyramid)
  CALL InterpolationPoint_Pyramid_(order=order, ipType=ipType, xij=xij, &
       ans=ans, nrow=nrow, ncol=ncol, layout=layout, alpha=alpha, beta=beta, &
                                   lambda=lambda)

CASE DEFAULT
  CALL Errormsg(msg="No CASE FOUND: elemType="//ToString(elemType), &
               unitno=stdout, line=__LINE__, routine="InterpolationPoint()", &
                file=__FILE__)
  RETURN
END SELECT

END PROCEDURE InterpolationPoint_

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff1_
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)

CASE (Point)

CASE (Line)
  CALL LagrangeCoeff_Line_(order=order, xij=xij, i=i, &
                           ans=ans, tsize=tsize)

CASE (Triangle)
  CALL LagrangeCoeff_Triangle_(order=order, xij=xij, i=i, &
                               ans=ans, tsize=tsize)

CASE (Quadrangle)
  CALL LagrangeCoeff_Quadrangle_(order=order, xij=xij, i=i, &
                                 ans=ans, tsize=tsize)

CASE (Tetrahedron)
  CALL LagrangeCoeff_Tetrahedron_(order=order, xij=xij, i=i, &
                                  ans=ans, tsize=tsize)

CASE (Hexahedron)
  CALL LagrangeCoeff_Hexahedron_(order=order, xij=xij, i=i, &
                                 ans=ans, tsize=tsize)

CASE (Prism)
  CALL LagrangeCoeff_Prism_(order=order, xij=xij, i=i, &
                            ans=ans, tsize=tsize)

CASE (Pyramid)
  CALL LagrangeCoeff_Pyramid_(order=order, xij=xij, i=i, &
                              ans=ans, tsize=tsize)

CASE DEFAULT
  CALL Errormsg(msg="No CASE FOUND: elemType="//ToString(elemType), &
                routine="LagrangeCoeff1_()", &
                unitno=stdout, line=__LINE__, file=__FILE__)
  RETURN
END SELECT

END PROCEDURE LagrangeCoeff1_

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff2_
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (Point)

CASE (Line)
  CALL LagrangeCoeff_Line_(order=order, xij=xij, ans=ans, &
                           nrow=nrow, ncol=ncol)

CASE (Triangle)
  CALL LagrangeCoeff_Triangle_(order=order, xij=xij, basisType=Monomial, &
                            refTriangle="UNIT", ans=ans, nrow=nrow, ncol=ncol)

CASE (Quadrangle)
  CALL LagrangeCoeff_Quadrangle_(order=order, xij=xij, &
                                 ans=ans, nrow=nrow, ncol=ncol)

CASE (Tetrahedron)
  CALL LagrangeCoeff_Tetrahedron_(order=order, xij=xij, &
                                  ans=ans, nrow=nrow, ncol=ncol)

CASE (Hexahedron)
  CALL LagrangeCoeff_Hexahedron_(order=order, xij=xij, &
                                 ans=ans, nrow=nrow, ncol=ncol)

CASE (Prism)
  CALL LagrangeCoeff_Prism_(order=order, xij=xij, &
                            ans=ans, nrow=nrow, ncol=ncol)

CASE (Pyramid)
  CALL LagrangeCoeff_Pyramid_(order=order, xij=xij, &
                              ans=ans, nrow=nrow, ncol=ncol)

CASE DEFAULT
  CALL Errormsg(msg="No CASE FOUND: elemType="//ToString(elemType), &
                unitno=stdout, line=__LINE__, routine="LagrangeCoeff2_()", &
                file=__FILE__)
END SELECT
END PROCEDURE LagrangeCoeff2_

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff3_
INTEGER(I4B) :: topo
topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (Point)

CASE (Line)
  CALL LagrangeCoeff_Line_(order=order, i=i, v=v, isVandermonde=.TRUE., &
                           ans=ans, tsize=tsize)

CASE (Triangle)
  CALL LagrangeCoeff_Triangle_(order=order, i=i, v=v, isVandermonde=.TRUE., &
                               ans=ans, tsize=tsize)

CASE (Quadrangle)
 CALL LagrangeCoeff_Quadrangle_(order=order, i=i, v=v, isVandermonde=.TRUE., &
                                 ans=ans, tsize=tsize)

CASE (Tetrahedron)
CALL LagrangeCoeff_Tetrahedron_(order=order, i=i, v=v, isVandermonde=.TRUE., &
                                  ans=ans, tsize=tsize)

CASE (Hexahedron)
 CALL LagrangeCoeff_Hexahedron_(order=order, i=i, v=v, isVandermonde=.TRUE., &
                                 ans=ans, tsize=tsize)

CASE (Prism)
  CALL LagrangeCoeff_Prism_(order=order, i=i, v=v, isVandermonde=.TRUE., &
                            ans=ans, tsize=tsize)

CASE (Pyramid)
  CALL LagrangeCoeff_Pyramid_(order=order, i=i, v=v, isVandermonde=.TRUE., &
                              ans=ans, tsize=tsize)

CASE DEFAULT
  CALL Errormsg(msg="No CASE FOUND: elemType="//ToString(elemType), &
                routine="LagrangeCoeff3_()", unitno=stdout, line=__LINE__, &
                file=__FILE__)
  RETURN
END SELECT

END PROCEDURE LagrangeCoeff3_

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff4_
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (Point)

CASE (Line)
  CALL LagrangeCoeff_Line_(order=order, i=i, v=v, ipiv=ipiv, &
                           ans=ans, tsize=tsize)

CASE (Triangle)
  CALL LagrangeCoeff_Triangle_(order=order, i=i, v=v, ipiv=ipiv, &
                               ans=ans, tsize=tsize)

CASE (Quadrangle)
  CALL LagrangeCoeff_Quadrangle_(order=order, i=i, v=v, ipiv=ipiv, &
                                 ans=ans, tsize=tsize)

CASE (Tetrahedron)
  CALL LagrangeCoeff_Tetrahedron_(order=order, i=i, v=v, ipiv=ipiv, &
                                  ans=ans, tsize=tsize)

CASE (Hexahedron)
  CALL LagrangeCoeff_Hexahedron_(order=order, i=i, v=v, ipiv=ipiv, &
                                 ans=ans, tsize=tsize)

CASE (Prism)
  CALL LagrangeCoeff_Prism_(order=order, i=i, v=v, ipiv=ipiv, &
                            ans=ans, tsize=tsize)

CASE (Pyramid)
  CALL LagrangeCoeff_Pyramid_(order=order, i=i, v=v, ipiv=ipiv, &
                              ans=ans, tsize=tsize)

CASE DEFAULT
  CALL Errormsg( &
    msg="No CASE FOUND: elemType="//ToString(elemType), &
    routine="LagrangeCoeff4_()", &
    unitno=stdout, line=__LINE__, file=__FILE__)
  RETURN
END SELECT

END PROCEDURE LagrangeCoeff4_

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff1
INTEGER(I4B) :: tsize
CALL LagrangeCoeff1_(order=order, elemType=elemType, i=i, xij=xij, ans=ans, &
                     tsize=tsize)

END PROCEDURE LagrangeCoeff1

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff2
INTEGER(I4B) :: nrow, ncol
CALL LagrangeCoeff2_(order=order, elemType=elemType, xij=xij, ans=ans, &
                     nrow=nrow, ncol=ncol)

END PROCEDURE LagrangeCoeff2

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff3
INTEGER(I4B) :: tsize
CALL LagrangeCoeff3_(order=order, elemType=elemType, i=i, v=v, &
                     isVandermonde=isVandermonde, ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff3

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff4
INTEGER(I4B) :: tsize
CALL LagrangeCoeff4_(order=order, elemType=elemType, i=i, v=v, ipiv=ipiv, &
                     ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff4

!----------------------------------------------------------------------------
!                                                           LagrangeEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll1
INTEGER(I4B) :: nrow, ncol
CALL LagrangeEvalAll1_(order=order, elemType=elemType, x=x, xij=xij, &
          ans=ans, nrow=nrow, ncol=ncol, domainName=domainName, coeff=coeff, &
           firstCall=firstCall, basisType=basisType, alpha=alpha, beta=beta, &
                       lambda=lambda)
END PROCEDURE LagrangeEvalAll1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll1_
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (Point)

CASE (Line)
  ! ans = LagrangeEvalAll_Line( &
  CALL LagrangeEvalAll_Line_(order=order, xij=xij, x=x, coeff=coeff, &
           firstCall=firstCall, basisType=basisType, alpha=alpha, beta=beta, &
                             lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)

CASE (Triangle)
  ! ans = LagrangeEvalAll_Triangle( &
  CALL LagrangeEvalAll_Triangle_(order=order, x=x, xij=xij, &
                   refTriangle=domainName, coeff=coeff, firstCall=firstCall, &
        basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
                                 nrow=nrow, ncol=ncol)

CASE (Quadrangle)
  ! ans = LagrangeEvalAll_Quadrangle( &
  CALL LagrangeEvalAll_Quadrangle_(order=order, x=x, xij=xij, &
         coeff=coeff, firstCall=firstCall, basisType=basisType, alpha=alpha, &
                      beta=beta, lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)

CASE (Tetrahedron)
  ! ans = LagrangeEvalAll_Tetrahedron( &
  CALL LagrangeEvalAll_Tetrahedron_(order=order, x=x, xij=xij, &
                refTetrahedron=domainName, coeff=coeff, firstCall=firstCall, &
        basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
                                    nrow=nrow, ncol=ncol)

CASE (Hexahedron)

  ! ans = LagrangeEvalAll_Hexahedron( &
  CALL LagrangeEvalAll_Hexahedron_(order=order, x=x, xij=xij, &
         coeff=coeff, firstCall=firstCall, basisType=basisType, alpha=alpha, &
                      beta=beta, lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)

CASE (Prism)
  ! ans = LagrangeEvalAll_Prism( &
  CALL LagrangeEvalAll_Prism_(order=order, x=x, xij=xij, &
                      refPrism=domainName, coeff=coeff, firstCall=firstCall, &
        basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
                              nrow=nrow, ncol=ncol)

CASE (Pyramid)
  ! ans = LagrangeEvalAll_Pyramid( &
  CALL LagrangeEvalAll_Pyramid_(order=order, x=x, xij=xij, &
                    refPyramid=domainName, coeff=coeff, firstCall=firstCall, &
        basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
                                nrow=nrow, ncol=ncol)

CASE DEFAULT
  CALL Errormsg(msg="No CASE FOUND: elemType="//ToString(elemType), &
                routine="LagrangeEvalAll2()", &
                unitno=stdout, line=__LINE__, file=__FILE__)
  RETURN
END SELECT

END PROCEDURE LagrangeEvalAll1_

!----------------------------------------------------------------------------
!                                                 LagrangeGradientEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll1
INTEGER(I4B) :: dim1, dim2, dim3
CALL LagrangeGradientEvalAll1_(order=order, elemType=elemType, x=x, xij=xij, &
            ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, domainName=domainName, &
         coeff=coeff, firstCall=firstCall, basisType=basisType, alpha=alpha, &
                               beta=beta, lambda=lambda)
END PROCEDURE LagrangeGradientEvalAll1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll1_
INTEGER(I4B) :: topo

dim1 = SIZE(x, 2)
dim2 = SIZE(xij, 2)
dim3 = SIZE(x, 1)

topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (Point)

CASE (Line)

#ifdef DEBUG_VER

  IF (SIZE(x, 1) .NE. 1 .OR. SIZE(xij, 1) .NE. 1) THEN
    CALL Errormsg(msg="SIZE(x, 1) or SIZE(xij, 1) .NE. 1", &
                  routine="LagrangeGradientEvalAll1", unitno=stderr, &
                  line=__LINE__, file=__FILE__)
    RETURN
  END IF

#endif

  ! ans(1:dim1, 1:dim2, 1:1) = LagrangeGradientEvalAll_Line(order=order, &
  CALL LagrangeGradientEvalAll_Line_(order=order, x=x, xij=xij, coeff=coeff, &
           firstCall=firstCall, basisType=basisType, alpha=alpha, beta=beta, &
                      lambda=lambda, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

CASE (Triangle)

#ifdef DEBUG_VER

  IF (SIZE(x, 1) .NE. 2 .OR. SIZE(xij, 1) .NE. 2) THEN
    CALL Errormsg(msg="SIZE(x, 1) or SIZE(xij, 1) .NE. 2", &
                  routine="LagrangeGradientEvalAll1", &
                  unitno=stderr, line=__LINE__, file=__FILE__)
    RETURN
  END IF

#endif

  ! ans(1:dim1, 1:dim2, 1:2) = LagrangeGradientEvalAll_Triangle(order=order, &
  CALL LagrangeGradientEvalAll_Triangle_(order=order, ans=ans, dim1=dim1, &
    dim2=dim2, dim3=dim3, x=x, xij=xij, refTriangle=domainName, coeff=coeff, &
           firstCall=firstCall, basisType=basisType, alpha=alpha, beta=beta, &
                                         lambda=lambda)

CASE (Quadrangle)

#ifdef DEBUG_VER
  IF (SIZE(x, 1) .NE. 2 .OR. SIZE(xij, 1) .NE. 2) THEN
    CALL Errormsg( &
      msg="SIZE(x, 1) or SIZE(xij, 1) .NE. 2", &
      routine="LagrangeGradientEvalAll1", &
      unitno=stderr, line=__LINE__, file=__FILE__)
    RETURN
  END IF

#endif

  ! ans(1:dim1, 1:dim2, 1:2) = LagrangeGradientEvalAll_Quadrangle( &
  CALL LagrangeGradientEvalAll_Quadrangle_(order=order, x=x, xij=xij, &
         coeff=coeff, firstCall=firstCall, basisType=basisType, alpha=alpha, &
           beta=beta, lambda=lambda, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

CASE (Tetrahedron)

#ifdef DEBUG_VER

  IF (SIZE(x, 1) .NE. 3 .OR. SIZE(xij, 1) .NE. 3) THEN
    CALL Errormsg(msg="SIZE(x, 1) or SIZE(xij, 1) .NE. 3", &
                  routine="LagrangeGradientEvalAll1", &
                  unitno=stderr, line=__LINE__, file=__FILE__)
    RETURN
  END IF

#endif

  ! ans(1:dim1, 1:dim2, 1:3) = LagrangeGradientEvalAll_Tetrahedron( &
  CALL LagrangeGradientEvalAll_Tetrahedron_(order=order, x=x, xij=xij, &
                refTetrahedron=domainName, coeff=coeff, firstCall=firstCall, &
        basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
                                            dim1=dim1, dim2=dim2, dim3=dim3)

CASE (Hexahedron)

#ifdef DEBUG_VER

  IF (SIZE(x, 1) .NE. 3 .OR. SIZE(xij, 1) .NE. 3) THEN
    CALL Errormsg(msg="SIZE(x, 1) or SIZE(xij, 1) .NE. 3", &
                  routine="LagrangeGradientEvalAll1", &
                  unitno=stderr, line=__LINE__, file=__FILE__)
    RETURN
  END IF

#endif

  ! ans(1:dim1, 1:dim2, 1:3) = LagrangeGradientEvalAll_Hexahedron( &
  CALL LagrangeGradientEvalAll_Hexahedron_(order=order, x=x, xij=xij, &
         coeff=coeff, firstCall=firstCall, basisType=basisType, alpha=alpha, &
           beta=beta, lambda=lambda, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

CASE (Prism)

#ifdef DEBUG_VER
  IF (SIZE(x, 1) .NE. 3 .OR. SIZE(xij, 1) .NE. 3) THEN
    CALL Errormsg(msg="SIZE(x, 1) or SIZE(xij, 1) .NE. 3", &
                  routine="LagrangeGradientEvalAll1", &
                  unitno=stderr, line=__LINE__, file=__FILE__)
    RETURN
  END IF
#endif

  ! ans(1:dim1, 1:dim2, 1:3) = LagrangeGradientEvalAll_Prism(order=order, &
  CALL LagrangeGradientEvalAll_Prism_(order=order, x=x, xij=xij, &
                      refPrism=domainName, coeff=coeff, firstCall=firstCall, &
        basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
                                      dim1=dim1, dim2=dim2, dim3=dim3)

CASE (Pyramid)

#ifdef DEBUG_VER

  IF (SIZE(x, 1) .NE. 3 .OR. SIZE(xij, 1) .NE. 3) THEN
    CALL Errormsg(msg="SIZE(x, 1) or SIZE(xij, 1) .NE. 3", &
                  routine="LagrangeGradientEvalAll1", &
                  unitno=stderr, line=__LINE__, file=__FILE__)
    RETURN
  END IF

#endif

  ! ans(1:dim1, 1:dim2, 1:3) = LagrangeGradientEvalAll_Pyramid(order=order, &
  CALL LagrangeGradientEvalAll_Pyramid_(order=order, x=x, xij=xij, &
                    refPyramid=domainName, coeff=coeff, firstCall=firstCall, &
        basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
                                        dim1=dim1, dim2=dim2, dim3=dim3)

CASE DEFAULT

  CALL Errormsg(msg="No CASE FOUND: elemType="//ToString(elemType), &
                routine="LagrangeGradientEvalAll1()", &
                unitno=stdout, line=__LINE__, file=__FILE__)
  RETURN

END SELECT
END PROCEDURE LagrangeGradientEvalAll1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
