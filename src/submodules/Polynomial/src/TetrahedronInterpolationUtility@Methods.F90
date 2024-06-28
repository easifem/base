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

SUBMODULE(TetrahedronInterpolationUtility) Methods
USE BaseMethod
USE QuadraturePoint_Tetrahedron_Solin, ONLY: &
  QuadratureNumberTetrahedronSolin, &
  QuadratureOrderTetrahedronSolin, &
  QuadraturePointTetrahedronSolin, &
  MAX_ORDER_TETRAHEDRON_SOLIN

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                 RefElemDomain_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElemDomain_Tetrahedron
SELECT CASE (UpperCase(baseContinuity))
CASE ("H1")
  SELECT CASE (UpperCase(baseInterpol))
  CASE ("LAGRANGEPOLYNOMIAL", "LAGRANGE", "LAGRANGEINTERPOLATION")
    ans = "UNIT"
  CASE ("SERENDIPITYPOLYNOMIAL", "SERENDIPITY", "SERENDIPITYINTERPOLATION")
    ans = "UNIT"
  CASE ("HERMITPOLYNOMIAL", "HERMIT", "HERMITINTERPOLATION")
    ans = "UNIT"
  CASE ( &
    & "HIERARCHICALPOLYNOMIAL", &
    & "HIERARCHY", &
    & "HEIRARCHICALPOLYNOMIAL", &
    & "HEIRARCHY", &
    & "HIERARCHYINTERPOLATION", &
    & "HEIRARCHYINTERPOLATION")
    ans = "BIUNIT"
  CASE ("ORTHOGONALPOLYNOMIAL", "ORTHOGONAL", "ORTHOGONALINTERPOLATION")
    ans = "BIUNIT"
  CASE DEFAULT
    CALL Errormsg(&
      & msg="No case found for given baseInterpol="//TRIM(baseInterpol), &
      & file=__FILE__, &
      & line=__LINE__,&
      & routine="RefElemDomain_Tetrahedron()", &
      & unitno=stderr)
  END SELECT
CASE DEFAULT
  CALL Errormsg(&
    & msg="No case found for given baseContinuity="//TRIM(baseContinuity), &
    & file=__FILE__, &
    & line=__LINE__,&
    & routine="RefElemDomain_Tetrahedron()", &
    & unitno=stderr)
END SELECT
END PROCEDURE RefElemDomain_Tetrahedron

!----------------------------------------------------------------------------
!                                                   GetVertexDOF_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetVertexDOF_Tetrahedron
ans = 4
END PROCEDURE GetVertexDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                   GetEdgeDOF_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeDOF_Tetrahedron1
ans = pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6_I4B
END PROCEDURE GetEdgeDOF_Tetrahedron1

!----------------------------------------------------------------------------
!                                                     GetEdgeDOF_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetEdgeDOF_Tetrahedron2
ans = GetEdgeDOF_Tetrahedron1(p, p, p, p, p, p)
END PROCEDURE GetEdgeDOF_Tetrahedron2

!----------------------------------------------------------------------------
!                                                   GetFacetDOF_Tetrahedron1
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFacetDOF_Tetrahedron1
ans = (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2
END PROCEDURE GetFacetDOF_Tetrahedron1

!----------------------------------------------------------------------------
!                                                     GetFacetDOF_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetFacetDOF_Tetrahedron2
ans = GetFacetDOF_Tetrahedron1(p, p, p, p)
END PROCEDURE GetFacetDOF_Tetrahedron2

!----------------------------------------------------------------------------
!                                                     GetCellDOF_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetCellDOF_Tetrahedron1
ans = (p - 1) * (p - 2) * (p - 3) / 6_I4B
END PROCEDURE GetCellDOF_Tetrahedron1

!----------------------------------------------------------------------------
!                                               EdgeConnectivity_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeConnectivity_Tetrahedron
ans(:, 1) = [1, 2]
ans(:, 2) = [1, 3]
ans(:, 3) = [1, 4]
ans(:, 4) = [2, 3]
ans(:, 5) = [2, 4]
ans(:, 6) = [3, 4]
END PROCEDURE EdgeConnectivity_Tetrahedron

!----------------------------------------------------------------------------
!                                               FacetConnectivity_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetConnectivity_Tetrahedron
TYPE(String) :: baseInterpol0
TYPE(String) :: baseContinuity0

baseInterpol0 = UpperCase(baseInterpol)
baseContinuity0 = UpperCase(baseContinuity)

SELECT CASE (baseInterpol0%chars())
CASE ( &
  & "HIERARCHYPOLYNOMIAL", &
  & "HIERARCHY", &
  & "HEIRARCHYPOLYNOMIAL", &
  & "HEIRARCHY", &
  & "HIERARCHYINTERPOLATION", &
  & "HEIRARCHYINTERPOLATION", &
  & "ORTHOGONALPOLYNOMIAL", &
  & "ORTHOGONAL", &
  & "ORTHOGONALINTERPOLATION")
  ans(:, 1) = [1, 2, 3]
  ans(:, 2) = [1, 2, 4]
  ans(:, 3) = [1, 3, 4]
  ans(:, 4) = [2, 3, 4]
CASE DEFAULT
  ans(:, 1) = [1, 3, 2]
  ans(:, 2) = [1, 2, 4]
  ans(:, 3) = [1, 4, 3]
  ans(:, 4) = [2, 3, 4]
END SELECT

END PROCEDURE FacetConnectivity_Tetrahedron

!----------------------------------------------------------------------------
!                                                LagrangeDegree_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Tetrahedron
INTEGER(I4B) :: n, ii, jj, kk, ll
n = LagrangeDOF_Tetrahedron(order=order)
ALLOCATE (ans(n, 3))
ll = 0
DO kk = 0, order
  DO jj = 0, order
    DO ii = 0, order
      IF (ii + jj + kk .LE. order) THEN
        ll = ll + 1
        ans(ll, 1) = ii
        ans(ll, 2) = jj
        ans(ll, 3) = kk
      END IF
    END DO
  END DO
END DO
END PROCEDURE LagrangeDegree_Tetrahedron

!----------------------------------------------------------------------------
!                                                    LagrangeDOF_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Tetrahedron
ans = (order + 1) * (order + 2) * (order + 3) / 6_I4B
END PROCEDURE LagrangeDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                    GetTotalDOF_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalDOF_Tetrahedron
ans = (order + 1) * (order + 2) * (order + 3) / 6_I4B
END PROCEDURE GetTotalDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                  GetTotalInDOF_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalInDOF_Tetrahedron
ans = (order - 1) * (order - 2) * (order - 3) / 6_I4B
END PROCEDURE GetTotalInDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                  LagrangeInDOF_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Tetrahedron
ans = (order - 1) * (order - 2) * (order - 3) / 6_I4B
END PROCEDURE LagrangeInDOF_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Tetrahedron_old
INTEGER(I4B), PARAMETER :: nv = 4_I4B
INTEGER(I4B) :: nsd, n, ne, nf, nc, i1, i2, ii
REAL(DFP) :: x(3, nv), xin(3, nv), e1(3), e2(3), e3(3), lam, &
  & avar, mu, delta
INTEGER(I4B), PARAMETER :: edges(6, 2) = RESHAPE( &
  & [1, 1, 1, 2, 2, 3, 2, 3, 4, 3, 4, 4], [6, 2])
INTEGER(I4B), PARAMETER :: faces(4, 3) = RESHAPE( &
  & [1, 1, 1, 2, 3, 2, 4, 3, 2, 4, 3, 4], [4, 3])

x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP
IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x(1:nsd, 1:nv) = xij(1:nsd, 1:nv)
ELSE
  nsd = 3_I4B
  x(1:nsd, 1) = [0.0, 0.0, 0.0]
  x(1:nsd, 2) = [1.0, 0.0, 0.0]
  x(1:nsd, 3) = [0.0, 1.0, 0.0]
  x(1:nsd, 4) = [0.0, 0.0, 1.0]
END IF

n = LagrangeDOF_Tetrahedron(order=order)
ALLOCATE (ans(nsd, n))
ans = 0.0_DFP

! points on vertex
ans(1:nsd, 1:nv) = x(1:nsd, 1:nv)

! points on edge
ne = LagrangeInDOF_Line(order=order)
nf = LagrangeInDOF_Triangle(order=order)
nc = LagrangeInDOF_Tetrahedron(order=order)

i2 = nv
IF (order .GT. 1_I4B) THEN
  DO ii = 1, SIZE(edges, 1)
    i1 = i2 + 1; i2 = i2 + ne
    ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
      & order=order, &
      & xij=x(1:nsd, edges(ii, 1:2)))
  END DO
END IF

! points on face
IF (order .GT. 2_I4B) THEN
  DO ii = 1, SIZE(faces, 1)
    i1 = i2 + 1; i2 = i2 + nf
    ans(1:nsd, i1:i2) = EquidistanceInPoint_Triangle( &
      & order=order, &
      & xij=x(1:nsd, faces(ii, 1:3)))
  END DO
END IF

! points on cell
IF (order .GT. 3_I4B) THEN
  IF (order .EQ. 4_I4B) THEN
    ans(1:nsd, i2 + 1) = SUM(x(1:nsd, :), dim=2_I4B) / nv
  ELSE
    e1 = x(:, 2) - x(:, 1); avar = NORM2(e1); e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 3) - x(:, 1); avar = NORM2(e2); e2 = e2 / avar
    mu = avar / order
    e3 = x(:, 4) - x(:, 1); avar = NORM2(e3); e3 = e3 / avar
    delta = avar / order
    xin(1:nsd, 1) = x(1:nsd, 1) &
      & + lam * e1(1:nsd) &
      & + mu * e2(1:nsd) &
      & + delta * e3(1:nsd)

    e1 = x(:, 1) - x(:, 2); avar = NORM2(e1); e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 3) - x(:, 2); avar = NORM2(e2); e2 = e2 / avar
    mu = avar / order
    e3 = x(:, 4) - x(:, 2); avar = NORM2(e3); e3 = e3 / avar
    delta = avar / order
    xin(1:nsd, 2) = x(1:nsd, 2) &
      & + lam * e1(1:nsd) &
      & + mu * e2(1:nsd) &
      & + delta * e3(1:nsd)

    e1 = x(:, 1) - x(:, 3); avar = NORM2(e1); e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 2) - x(:, 3); avar = NORM2(e2); e2 = e2 / avar
    mu = avar / order
    e3 = x(:, 4) - x(:, 3); avar = NORM2(e3); e3 = e3 / avar
    delta = avar / order
    xin(1:nsd, 3) = x(1:nsd, 3) &
      & + lam * e1(1:nsd) &
      & + mu * e2(1:nsd) &
      & + delta * e3(1:nsd)

    e1 = x(:, 1) - x(:, 4); avar = NORM2(e1); e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 2) - x(:, 4); avar = NORM2(e2); e2 = e2 / avar
    mu = avar / order
    e3 = x(:, 3) - x(:, 4); avar = NORM2(e3); e3 = e3 / avar
    delta = avar / order
    xin(1:nsd, 4) = x(1:nsd, 4) &
      & + lam * e1(1:nsd) &
      & + mu * e2(1:nsd) &
      & + delta * e3(1:nsd)

    i1 = i2 + 1
    ans(1:nsd, i1:) = EquidistancePoint_Tetrahedron( &
      & order=order - 4, &
      & xij=xin(1:nsd, 1:4))
  END IF
END IF
END PROCEDURE EquidistancePoint_Tetrahedron_old

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Tetrahedron_old
INTEGER(I4B), PARAMETER :: nv = 4_I4B
INTEGER(I4B) :: nsd, n, ne, nf, nc, i1, i2, ii
REAL(DFP) :: x(3, nv), xin(3, nv), e1(3), e2(3), e3(3), lam, &
  & avar, mu, delta
INTEGER(I4B), PARAMETER :: edges(6, 2) = RESHAPE( &
  & [1, 1, 1, 2, 2, 3, 2, 3, 4, 3, 4, 4], [6, 2])
INTEGER(I4B), PARAMETER :: faces(4, 3) = RESHAPE( &
  & [1, 1, 1, 2, 3, 2, 4, 3, 2, 4, 3, 4], [4, 3])

x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP

IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x(1:nsd, 1:nv) = xij(1:nsd, 1:nv)
ELSE
  nsd = 3_I4B
  x(1:nsd, 1) = [0.0, 0.0, 0.0]
  x(1:nsd, 2) = [1.0, 0.0, 0.0]
  x(1:nsd, 3) = [0.0, 1.0, 0.0]
  x(1:nsd, 4) = [0.0, 0.0, 1.0]
END IF
!
n = LagrangeInDOF_Tetrahedron(order=order)
!
! points on cell
!
IF (order .GT. 3_I4B) THEN
  ALLOCATE (ans(nsd, n))
  ans = 0.0_DFP
  IF (order .EQ. 4_I4B) THEN
    ans(1:nsd, i2 + 1) = SUM(x(1:nsd, :), dim=2_I4B) / nv
  ELSE
    !
    e1 = x(:, 2) - x(:, 1); avar = NORM2(e1); e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 3) - x(:, 1); avar = NORM2(e2); e2 = e2 / avar
    mu = avar / order
    e3 = x(:, 4) - x(:, 1); avar = NORM2(e3); e3 = e3 / avar
    delta = avar / order
    xin(1:nsd, 1) = x(1:nsd, 1) &
      & + lam * e1(1:nsd) &
      & + mu * e2(1:nsd) &
      & + delta * e3(1:nsd)
    !
    e1 = x(:, 1) - x(:, 2); avar = NORM2(e1); e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 3) - x(:, 2); avar = NORM2(e2); e2 = e2 / avar
    mu = avar / order
    e3 = x(:, 4) - x(:, 2); avar = NORM2(e3); e3 = e3 / avar
    delta = avar / order
    xin(1:nsd, 2) = x(1:nsd, 2) &
      & + lam * e1(1:nsd) &
      & + mu * e2(1:nsd) &
      & + delta * e3(1:nsd)
    !
    e1 = x(:, 1) - x(:, 3); avar = NORM2(e1); e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 2) - x(:, 3); avar = NORM2(e2); e2 = e2 / avar
    mu = avar / order
    e3 = x(:, 4) - x(:, 3); avar = NORM2(e3); e3 = e3 / avar
    delta = avar / order
    xin(1:nsd, 3) = x(1:nsd, 3) &
      & + lam * e1(1:nsd) &
      & + mu * e2(1:nsd) &
      & + delta * e3(1:nsd)
    !
    e1 = x(:, 1) - x(:, 4); avar = NORM2(e1); e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 2) - x(:, 4); avar = NORM2(e2); e2 = e2 / avar
    mu = avar / order
    e3 = x(:, 3) - x(:, 4); avar = NORM2(e3); e3 = e3 / avar
    delta = avar / order
    xin(1:nsd, 4) = x(1:nsd, 4) &
      & + lam * e1(1:nsd) &
      & + mu * e2(1:nsd) &
      & + delta * e3(1:nsd)
    !
    i1 = i2 + 1
    ans(1:nsd, i1:) = EquidistancePoint_Tetrahedron( &
      & order=order - 4, &
      & xij=xin(1:nsd, 1:4))
    !
  END IF
ELSE
  ALLOCATE (ans(0, 0))
END IF
END PROCEDURE EquidistanceInPoint_Tetrahedron_old

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Tetrahedron
ans = InterpolationPoint_Tetrahedron( &
  & order=order, &
  & ipType=Equidistance, &
  & layout="VEFC", &
  & xij=xij  &
  &)
END PROCEDURE EquidistancePoint_Tetrahedron

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Tetrahedron
INTEGER(I4B) :: ii, jj
REAL(DFP), ALLOCATABLE :: ans0(:, :)
ans0 = EquidistancePoint_Tetrahedron(order=order, xij=xij)
ii = LagrangeDOF_Tetrahedron(order)
jj = LagrangeInDOF_Tetrahedron(order)
CALL Reallocate(ans, 3, jj)
ans = ans0(1:3, ii - jj + 1:)
IF (ALLOCATED(ans0)) DEALLOCATE (ans0)
END PROCEDURE EquidistanceInPoint_Tetrahedron

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Tetrahedron
INTEGER(I4B) :: nrow, ncol
ncol = SIZE(n=order, d=3)
ALLOCATE (ans(3, ncol))
CALL InterpolationPoint_Tetrahedron_(order=order, ipType=ipType, &
              layout=layout, xij=xij, alpha=alpha, beta=beta, lambda=lambda, &
                                     ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE InterpolationPoint_Tetrahedron

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Tetrahedron_
CALL Isaac_Tetrahedron(order=order, ipType=ipType, layout=layout, xij=xij, &
         alpha=alpha, beta=beta, lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE InterpolationPoint_Tetrahedron_

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Tetrahedron1
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Tetrahedron1_(order=order, i=i, xij=xij, ans=ans, &
                                 tsize=tsize)
END PROCEDURE LagrangeCoeff_Tetrahedron1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Tetrahedron1_
REAL(DFP), DIMENSION(SIZE(xij, 2), SIZE(xij, 2)) :: V
INTEGER(I4B), DIMENSION(SIZE(xij, 2)) :: ipiv
INTEGER(I4B) :: info

tsize = SIZE(xij, 2)

ipiv = 0_I4B; ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP
V = LagrangeVandermonde(order=order, xij=xij, elemType=Tetrahedron)
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL LUSolve(A=V, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Tetrahedron1_

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Tetrahedron2
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Tetrahedron2_(order=order, i=i, v=v, &
                                 isVandermonde=.TRUE., ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Tetrahedron2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Tetrahedron2_
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: vtemp
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B) :: info

tsize = SIZE(v, 1)

vtemp = v; ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP; ipiv = 0_I4B
CALL GetLU(A=vtemp, IPIV=ipiv, info=info)
CALL LUSolve(A=vtemp, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Tetrahedron2_

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Tetrahedron3
INTEGER(I4B) :: tsize
CALL LagrangeCoeff_Tetrahedron3_(order=order, i=i, v=v, ipiv=ipiv, &
                                 ans=ans, tsize=tsize)
END PROCEDURE LagrangeCoeff_Tetrahedron3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Tetrahedron3_
INTEGER(I4B) :: info

tsize = SIZE(v, 1)

ans(1:tsize) = 0.0_DFP; ans(i) = 1.0_DFP
CALL LUSolve(A=v, B=ans(1:tsize), IPIV=ipiv, info=info)
END PROCEDURE LagrangeCoeff_Tetrahedron3_

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Tetrahedron4
INTEGER(I4B) :: nrow, ncol

CALL LagrangeCoeff_Tetrahedron4_(order=order, xij=xij, basisType=basisType, &
       refTetrahedron=refTetrahedron, alpha=alpha, beta=beta, lambda=lambda, &
                                 ans=ans, nrow=nrow, ncol=ncol)

END PROCEDURE LagrangeCoeff_Tetrahedron4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeCoeff_Tetrahedron4_
INTEGER(I4B) :: basisType0
CHARACTER(:), ALLOCATABLE :: aname

basisType0 = input(default=Monomial, option=basisType)
nrow = SIZE(xij, 2)
ncol = nrow

SELECT CASE (basisType0)
CASE (Monomial)
  ans(1:nrow, 1:ncol) = LagrangeVandermonde(order=order, xij=xij, &
                                            elemType=Tetrahedron)

  CALL LagrangeVandermonde_(order=order, xij=xij, ans=ans, nrow=nrow, &
                            ncol=ncol, elemType=Tetrahedron)

CASE (Heirarchical)
  aname = Input(default="UNIT", option=refTetrahedron)

  ans(1:nrow, 1:ncol) = HeirarchicalBasis_Tetrahedron(order=order, xij=xij, &
                                                      refTetrahedron=aname)

CASE DEFAULT
  aname = Input(default="UNIT", option=refTetrahedron)

  ans(1:nrow, 1:ncol) = OrthogonalBasis_Tetrahedron(order=order, &
                                       xij=xij, refTetrahedron=refTetrahedron)

END SELECT

CALL GetInvMat(ans(1:nrow, 1:ncol))

END PROCEDURE LagrangeCoeff_Tetrahedron4_

!----------------------------------------------------------------------------
!                                                         Isaac_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE Isaac_Tetrahedron
! CHARACTER(*), PARAMETER :: myName = "Isaac_Tetrahedron"

REAL(DFP), DIMENSION(order + 1, order + 1, order + 1) :: xi, eta, zeta

INTEGER(I4B) :: cnt, ii, jj, kk

ncol = SIZE(n=order, d=3)
nrow = 3

CALL RecursiveNode3D_(order=order, ipType=ipType, domain="UNIT", &
         alpha=alpha, beta=beta, lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)

! CALL Reallocate(ans, nsd, N)
! CALL Reallocate(temp, nrow, ncol)

!! convert from rPoints to xi and eta
cnt = 0
xi = 0.0_DFP
eta = 0.0_DFP
zeta = 0.0_DFP

DO ii = 0, order
  DO jj = 0, order
    DO kk = 0, order
      IF (ii + jj + kk .LE. order) THEN
        cnt = cnt + 1
        xi(ii + 1, jj + 1, kk + 1) = ans(1, cnt)
        eta(ii + 1, jj + 1, kk + 1) = ans(2, cnt)
        zeta(ii + 1, jj + 1, kk + 1) = ans(3, cnt)
      END IF
    END DO
  END DO
END DO

IF (layout .EQ. "VEFC") THEN
  CALL IJK2VEFC_Tetrahedron(xi=xi, eta=eta, zeta=zeta, temp=ans, &
                            order=order, N=ncol)
END IF

IF (PRESENT(xij)) THEN
  ! convert temp to ans using xij
  CALL FromUnitTetrahedron2Tetrahedron_(xin=ans(1:nrow, 1:ncol), &
            x1=xij(:, 1), x2=xij(:, 2), x3=xij(:, 3), x4=xij(:, 4), ans=ans, &
                                        nrow=nrow, ncol=ncol)
END IF

END PROCEDURE Isaac_Tetrahedron

!----------------------------------------------------------------------------
!                                                 BlythPozrikidis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BlythPozrikidis_Tetrahedron
CALL ErrorMsg( &
  & msg="This method is under development, please use Isaac_Tetrahedron()", &
  & file=__FILE__, &
  & routine="BlythPozrikidis_Tetrahedron()", &
  & line=__LINE__, &
  & unitno=stderr)
RETURN
END PROCEDURE BlythPozrikidis_Tetrahedron

!----------------------------------------------------------------------------
!                                                      IJK2VEFC_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE IJK2VEFC_Tetrahedron
INTEGER(I4B) :: indof, ii, cnt, jj, kk, ll
REAL(DFP) :: x(3)
INTEGER(I4B), PARAMETER :: nrow = 3

REAL(DFP), DIMENSION(3, (order + 1)*(order + 2)/2) :: temp_face_in
REAL(DFP), DIMENSION(order + 1, order + 1) :: xi2, eta2, zeta2

SELECT CASE (order)
CASE (0)
  x = [xi(1, 1, 1), eta(1, 1, 1), zeta(1, 1, 1)]
  temp(1:nrow, 1) = x
CASE (1)
  !  | 0 | 0 | 0 |
  !  | 0 | 0 | 1 |
  !  | 0 | 1 | 0 |
  !  | 1 | 0 | 0 |
  x = [xi(1, 1, 1), eta(1, 1, 1), zeta(1, 1, 1)]
  temp(1:nrow, 1) = x

  x = [xi(order + 1, 1, 1), eta(order + 1, 1, 1), zeta(order + 1, 1, 1)]
  temp(1:nrow, 2) = x

  x = [xi(1, order + 1, 1), eta(1, order + 1, 1), zeta(1, order + 1, 1)]
  temp(1:nrow, 3) = x

  x = [xi(1, 1, order + 1), eta(1, 1, order + 1), zeta(1, 1, order + 1)]
  temp(1:nrow, 4) = x

CASE (2)
  ! | 0 | 0 | 0 |
  ! | 0 | 0 | 0.5 |
  ! | 0 | 0 | 1 |
  ! | 0 | 0.5 | 0 |
  ! | 0 | 0.5 | 0.5 |
  ! | 0 | 1 | 0 |
  ! | 0.5 | 0 | 0 |
  ! | 0.5 | 0 | 0.5 |
  ! | 0.5 | 0.5 | 0 |
  ! | 1 | 0 | 0 |

  ! four vertex
  x = [xi(1, 1, 1), eta(1, 1, 1), zeta(1, 1, 1)]
  temp(1:nrow, 1) = x

  x = [xi(order + 1, 1, 1), eta(order + 1, 1, 1), zeta(order + 1, 1, 1)]
  temp(1:nrow, 2) = x

  x = [xi(1, order + 1, 1), eta(1, order + 1, 1), zeta(1, order + 1, 1)]
  temp(1:nrow, 3) = x

  x = [xi(1, 1, order + 1), eta(1, 1, order + 1), zeta(1, 1, order + 1)]
  temp(1:nrow, 4) = x

  ! edge1 x
  x = [xi(2, 1, 1), eta(2, 1, 1), zeta(2, 1, 1)]
  temp(1:nrow, 5) = x

  ! edge2 y
  x = [xi(1, 2, 1), eta(1, 2, 1), zeta(1, 2, 1)]
  temp(1:nrow, 6) = x

  ! edge3 z
  x = [xi(1, 1, 2), eta(1, 1, 2), zeta(1, 1, 2)]
  temp(1:nrow, 7) = x

  ! edge4 xy
  x = [xi(2, 2, 1), eta(2, 2, 1), zeta(2, 2, 1)]
  temp(1:nrow, 8) = x

  ! edge5, xz
  x = [xi(2, 1, 2), eta(2, 1, 2), zeta(2, 1, 2)]
  temp(1:nrow, 9) = x

  ! edge6, yz
  x = [xi(1, 2, 2), eta(1, 2, 2), zeta(1, 2, 2)]
  temp(1:nrow, 10) = x

CASE (3)
  ! | 0 | 0 | 0 |
  ! | 0 | 0 | 0.33333 |
  ! | 0 | 0 | 0.66667 |
  ! | 0 | 0 | 1 |
  ! | 0 | 0.33333 | 0 |
  ! | 0 | 0.33333 | 0.33333 |
  ! | 0 | 0.33333 | 0.66667 |
  ! | 0 | 0.66667 | 0 |
  ! | 0 | 0.66667 | 0.33333 |
  ! | 0 | 1 | 0 |
  ! | 0.33333 | 0 | 0 |
  ! | 0.33333 | 0 | 0.33333 |
  ! | 0.33333 | 0 | 0.66667 |
  ! | 0.33333 | 0.33333 | 0 |
  ! | 0.33333 | 0.33333 | 0.33333 |
  ! | 0.33333 | 0.66667 | 0 |
  ! | 0.66667 | 0 | 0 |
  ! | 0.66667 | 0 | 0.33333 |
  ! | 0.66667 | 0.33333 | 0 |
  ! | 1 | 0 | 0 |

  ! four vertex
  x = [xi(1, 1, 1), eta(1, 1, 1), zeta(1, 1, 1)]
  temp(1:nrow, 1) = x

  x = [xi(order + 1, 1, 1), eta(order + 1, 1, 1), zeta(order + 1, 1, 1)]
  temp(1:nrow, 2) = x

  x = [xi(1, order + 1, 1), eta(1, order + 1, 1), zeta(1, order + 1, 1)]
  temp(1:nrow, 3) = x

  x = [xi(1, 1, order + 1), eta(1, 1, order + 1), zeta(1, 1, order + 1)]
  temp(1:nrow, 4) = x

  cnt = 4
  ! edge1 x
  DO ii = 1, order - 1
    cnt = cnt + 1
    x = [xi(1 + ii, 1, 1), eta(1 + ii, 1, 1), zeta(1 + ii, 1, 1)]
    temp(1:nrow, cnt) = x
  END DO

  ! edge2 y
  DO ii = 1, order - 1
    cnt = cnt + 1
    x = [xi(1, 1 + ii, 1), eta(1, 1 + ii, 1), zeta(1, 1 + ii, 1)]
    temp(1:nrow, cnt) = x
  END DO

  ! edge3 z
  DO ii = 1, order - 1
    cnt = cnt + 1
    x = [xi(1, 1, 1 + ii), eta(1, 1, 1 + ii), zeta(1, 1, 1 + ii)]
    temp(1:nrow, cnt) = x
  END DO

  ! edge4 xy
  DO ii = 1, order - 1
    cnt = cnt + 1
    x = [xi(4 - ii, 1 + ii, 1), eta(4 - ii, 1 + ii, 1), &
         zeta(4 - ii, 1 + ii, 1)]
    temp(1:nrow, cnt) = x
  END DO

  ! edge5, xz
  DO ii = 1, order - 1
    cnt = cnt + 1
    x = [xi(4 - ii, 1, ii + 1), eta(4 - ii, 1, ii + 1), &
         zeta(4 - ii, 1, ii + 1)]
    temp(1:nrow, cnt) = x
  END DO
  ! edge6, yz
  DO ii = 1, order - 1
    cnt = cnt + 1
    x = [xi(1, 4 - ii, ii + 1), eta(1, 4 - ii, ii + 1), &
         zeta(1, 4 - ii, ii + 1)]
    temp(1:nrow, cnt) = x

  END DO

  ! facet xy
  cnt = cnt + 1
  x = [xi(2, 2, 1), eta(2, 2, 1), zeta(2, 2, 1)]
  temp(1:nrow, cnt) = x

  ! facet xz
  cnt = cnt + 1
  x = [xi(2, 1, 2), eta(2, 1, 2), zeta(2, 1, 2)]
  temp(1:nrow, cnt) = x

  ! facet yz
  cnt = cnt + 1
  x = [xi(1, 2, 2), eta(1, 2, 2), zeta(1, 2, 2)]
  temp(1:nrow, cnt) = x

  ! facet 4
  cnt = cnt + 1
  x = [xi(2, 2, 2), eta(2, 2, 2), zeta(2, 2, 2)]
  temp(1:nrow, cnt) = x

CASE DEFAULT

  ! four vertex
  x = [xi(1, 1, 1), eta(1, 1, 1), zeta(1, 1, 1)]
  temp(1:nrow, 1) = x

  x = [xi(order + 1, 1, 1), eta(order + 1, 1, 1), zeta(order + 1, 1, 1)]
  temp(1:nrow, 2) = x

  x = [xi(1, order + 1, 1), eta(1, order + 1, 1), zeta(1, order + 1, 1)]
  temp(1:nrow, 3) = x

  x = [xi(1, 1, order + 1), eta(1, 1, order + 1), zeta(1, 1, order + 1)]
  temp(1:nrow, 4) = x

  cnt = 4
  ! edge1 x
  DO ii = 1, order - 1
    cnt = cnt + 1
    x = [xi(1 + ii, 1, 1), eta(1 + ii, 1, 1), zeta(1 + ii, 1, 1)]
    temp(1:nrow, cnt) = x

  END DO
  ! edge2 y
  DO ii = 1, order - 1
    cnt = cnt + 1
    x = [xi(1, 1 + ii, 1), eta(1, 1 + ii, 1), zeta(1, 1 + ii, 1)]
    temp(1:nrow, cnt) = x

  END DO
  ! edge3 z
  DO ii = 1, order - 1
    cnt = cnt + 1
    x = [xi(1, 1, 1 + ii), eta(1, 1, 1 + ii), zeta(1, 1, 1 + ii)]
    temp(1:nrow, cnt) = x

  END DO
  ! edge4 xy
  jj = order + 1
  DO ii = 1, order - 1
    cnt = cnt + 1
    x = [xi(jj - ii, 1 + ii, 1), eta(jj - ii, 1 + ii, 1), &
         zeta(jj - ii, 1 + ii, 1)]
    temp(1:nrow, cnt) = x
  END DO

  ! edge5, xz
  DO ii = 1, order - 1
    cnt = cnt + 1
    x = [xi(jj - ii, 1, ii + 1), eta(jj - ii, 1, ii + 1), &
         zeta(jj - ii, 1, ii + 1)]
    temp(1:nrow, cnt) = x
  END DO

  ! edge6, yz
  DO ii = 1, order - 1
    cnt = cnt + 1
    x = [xi(1, jj - ii, ii + 1), eta(1, jj - ii, ii + 1), &
         zeta(1, jj - ii, ii + 1)]
    temp(1:nrow, cnt) = x
  END DO

  ! facet xy
  jj = LagrangeDOF_Triangle(order)
  CALL IJ2VEFC_Triangle(xi=xi(:, :, 1), eta=eta(:, :, 1), &
                        temp=temp_face_in, order=order, N=jj)

  kk = LagrangeInDOF_Triangle(order)
  DO ii = jj - kk + 1, jj
    cnt = cnt + 1
    x = [temp_face_in(1, ii), temp_face_in(2, ii), zeta(1, 1, 1)]
    temp(1:nrow, cnt) = x
  END DO

  ! facet xz
  ! jj = LagrangeDOF_Triangle(order)
  CALL IJ2VEFC_Triangle(xi=xi(:, 1, :), eta=zeta(:, 1, :), &
                        temp=temp_face_in, order=order, N=jj)

  ! kk = LagrangeInDOF_Triangle(order)
  DO ii = jj - kk + 1, jj
    cnt = cnt + 1
    x = [temp_face_in(1, ii), eta(1, 1, 1), temp_face_in(2, ii)]
    temp(1:nrow, cnt) = x
  END DO

  ! facet yz
  ! jj = LagrangeDOF_Triangle(order)
  CALL IJ2VEFC_Triangle(xi=eta(1, :, :), eta=zeta(1, :, :), &
                        temp=temp_face_in, order=order, N=jj)
  ! kk = LagrangeInDOF_Triangle(order)
  DO ii = jj - kk + 1, jj
    cnt = cnt + 1
    x = [xi(1, 1, 1), temp_face_in(1, ii), temp_face_in(2, ii)]
    temp(1:nrow, cnt) = x
  END DO

  ! ! facet 4
  ! cnt = cnt + 1
  ! temp(:, cnt) = [xi(2, 2, 2), eta(2, 2, 2), zeta(2, 2, 2)]
  xi2 = 0.0_DFP
  eta2 = 0.0_DFP
  zeta2 = 0.0_DFP
  DO ii = 0, order
    ll = 0
    DO jj = 0, order
      DO kk = 0, order
        IF (ii + jj + kk .EQ. order) THEN
          ll = ll + 1
          xi2(ii + 1, ll) = xi(ii + 1, jj + 1, kk + 1)
          eta2(ii + 1, ll) = eta(ii + 1, jj + 1, kk + 1)
          zeta2(ii + 1, ll) = zeta(ii + 1, jj + 1, kk + 1)
        END IF
      END DO
    END DO
  END DO

  temp_face_in = 0.0_DFP
  CALL IJK2VEFC_Triangle(xi=xi2, eta=eta2, zeta=zeta2, temp=temp_face_in, &
                         order=order, N=SIZE(temp_face_in, 2))

  ! facet 4
  jj = LagrangeDOF_Triangle(order)
  CALL IJK2VEFC_Triangle(xi=xi2, eta=eta2, zeta=zeta2, temp=temp_face_in, &
                         order=order, N=jj)
  kk = LagrangeInDOF_Triangle(order)
  DO ii = jj - kk + 1, jj
    cnt = cnt + 1
    temp(:, cnt) = temp_face_in(1:3, ii)
  END DO

  jj = LagrangeDOF_Tetrahedron(order)
  kk = LagrangeInDOF_Tetrahedron(order=order)
  CALL IJK2VEFC_Tetrahedron(xi(2:order - 2, 2:order - 2, 2:order - 2), &
                            eta(2:order - 2, 2:order - 2, 2:order - 2), &
             zeta(2:order - 2, 2:order - 2, 2:order - 2), temp(:, cnt + 1:), &
                            order - 4, kk)
END SELECT

END PROCEDURE IJK2VEFC_Tetrahedron

!----------------------------------------------------------------------------
!                                                         IJ2VEFC_Triangle
!----------------------------------------------------------------------------

SUBROUTINE IJK2VEFC_Triangle(xi, eta, zeta, temp, order, N)
  REAL(DFP), INTENT(IN) :: xi(:, :)
  REAL(DFP), INTENT(IN) :: eta(:, :)
  REAL(DFP), INTENT(IN) :: zeta(:, :)
  REAL(DFP), INTENT(OUT) :: temp(:, :)
  INTEGER(I4B), INTENT(IN) :: order
  INTEGER(I4B), INTENT(IN) :: N

  INTEGER(I4B) :: cnt, m, ii, jj, kk, ll, llt, llr

  cnt = 0
  m = order
  llt = INT((m - 1) / 3)
  llr = MOD(m - 1, 3)
  DO ll = 0, llt
    ! v1
    cnt = cnt + 1
    ii = 1 + ll; jj = 1 + ll
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
    temp(3, cnt) = zeta(ii, jj)
    ! v2
    cnt = cnt + 1
    ii = m + 1 - 2 * ll; jj = 1 + ll
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
    temp(3, cnt) = zeta(ii, jj)
    ! v3
    cnt = cnt + 1
    ii = 1 + ll; jj = m + 1 - 2 * ll
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
    temp(3, cnt) = zeta(ii, jj)
    ! nodes on edge 12
    jj = ll + 1
    DO ii = 2 + ll, m - 2 * ll
      cnt = cnt + 1
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
      temp(3, cnt) = zeta(ii, jj)
    END DO
    ! nodes on edge 23
    DO jj = 2 + ll, m - 2 * ll
      cnt = cnt + 1
      ii = m - ll + 2 - jj
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
      temp(3, cnt) = zeta(ii, jj)
    END DO
    ! nodes on edge 31
    ii = ll + 1
    DO jj = m - 2 * ll, 2 + ll, -1
      cnt = cnt + 1
      temp(1, cnt) = xi(ii, jj)
      temp(2, cnt) = eta(ii, jj)
      temp(3, cnt) = zeta(ii, jj)
    END DO
    ! internal nodes
  END DO

  IF (llr .EQ. 2_I4B) THEN
    ! a internal point
    cnt = cnt + 1
    ll = llt + 1
    ii = 1 + ll; jj = 1 + ll
    temp(1, cnt) = xi(ii, jj)
    temp(2, cnt) = eta(ii, jj)
    temp(3, cnt) = zeta(ii, jj)
  END IF

  IF (cnt .NE. N) THEN
    CALL ErrorMsg( &
      & msg="cnt="//tostring(cnt)//" not equal to total DOF, N=" &
      & //tostring(N), &
      & file=__FILE__, &
      & routine="IJ2VEFC_Triangle()", &
      & line=__LINE__, &
      & unitno=stderr)
    RETURN
  END IF

END SUBROUTINE IJK2VEFC_Triangle

!----------------------------------------------------------------------------
!                                                OrthogonalBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasis_Tetrahedron1
CHARACTER(20) :: layout
REAL(DFP) :: x(1:3, 1:SIZE(xij, 2))
REAL(DFP) :: P1(SIZE(xij, 2), 0:order)
REAL(DFP) :: Q1(SIZE(xij, 2), 0:order)
REAL(DFP) :: R1(SIZE(xij, 2), 0:order)
REAL(DFP) :: x2(SIZE(xij, 2), 0:order)
REAL(DFP) :: x3(SIZE(xij, 2), 0:order)
INTEGER(I4B) :: cnt
INTEGER(I4B) :: p, q, r

layout = TRIM(UpperCase(refTetrahedron))
SELECT CASE (TRIM(layout))
CASE ("BIUNIT")
  x = FromBiUnitTetrahedron2BiUnitHexahedron(xin=xij)
CASE ("UNIT")
  x = FromUnitTetrahedron2BiUnitHexahedron(xin=xij)
END SELECT

DO p = 0, order
  x2(:, p) = 0.5_DFP * (1.0_DFP - x(2, :))
  x3(:, p) = 0.5_DFP * (1.0_DFP - x(3, :))
END DO

P1 = LegendreEvalAll(n=order, x=x(1, :))

cnt = 0

DO p = 0, order

  Q1 = (x2**p) * JacobiEvalAll( &
    & n=order, &
    & x=x(2, :), &
    & alpha=REAL(2 * p + 1, DFP), &
    & beta=0.0_DFP)

  DO q = 0, order - p

    R1 = (x3**(p + q)) * JacobiEvalAll( &
    & n=order, &
    & x=x(3, :), &
    & alpha=REAL(2 * p + 2 * q + 2, DFP), &
    & beta=0.0_DFP)

    DO r = 0, order - p - q
      cnt = cnt + 1
      ans(:, cnt) = P1(:, p) * Q1(:, q) * R1(:, r)
    END DO
  END DO
END DO

END PROCEDURE OrthogonalBasis_Tetrahedron1

!----------------------------------------------------------------------------
!                                             OrthogonalBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasis_Tetrahedron2
CHARACTER(20) :: layout
REAL(DFP) :: x0(SIZE(x)), y0(SIZE(y)), z0(SIZE(z))
REAL(DFP) :: xij(3, SIZE(x) * SIZE(y) * SIZE(z))
INTEGER(I4B) :: ii, jj, cnt, kk
REAL(DFP) :: P1(1:3, 0:order)
REAL(DFP) :: Q1(1:3, 0:order)
REAL(DFP) :: R1(1:3, 0:order)
REAL(DFP) :: x2(SIZE(xij, 2), 0:order)
REAL(DFP) :: x3(SIZE(xij, 2), 0:order)
INTEGER(I4B) :: p, q, r

layout = TRIM(UpperCase(refTetrahedron))

SELECT CASE (TRIM(layout))
CASE ("BIUNIT")
  x0 = x
  y0 = y
  z0 = z
CASE ("UNIT")
  x0 = FromUnitLine2BiUnitLine(xin=x)
  y0 = FromUnitLine2BiUnitLine(xin=y)
  z0 = FromUnitLine2BiUnitLine(xin=z)
END SELECT

xij = 0.0_DFP
cnt = 0
DO ii = 1, SIZE(x0)
  DO jj = 1, SIZE(y0)
    DO kk = 1, SIZE(z0)
      cnt = cnt + 1
      xij(1, cnt) = x0(ii)
      xij(2, cnt) = y0(jj)
      xij(3, cnt) = z0(kk)
    END DO
  END DO
END DO

DO p = 0, order
  x2(:, p) = 0.5_DFP * (1.0_DFP - xij(2, :))
  x3(:, p) = 0.5_DFP * (1.0_DFP - xij(3, :))
END DO

P1 = LegendreEvalAll(n=order, x=xij(1, :))

cnt = 0

DO p = 0, order

  Q1 = (x2**p) * JacobiEvalAll( &
    & n=order, &
    & x=xij(2, :), &
    & alpha=REAL(2 * p + 1, DFP), &
    & beta=0.0_DFP)

  DO q = 0, order - p

    R1 = (x3**(p + q)) * JacobiEvalAll( &
    & n=order, &
    & x=xij(3, :), &
    & alpha=REAL(2 * p + 2 * q + 2, DFP), &
    & beta=0.0_DFP)

    DO r = 0, order - p - q
      cnt = cnt + 1
      ans(:, cnt) = P1(:, p) * Q1(:, q) * R1(:, r)
    END DO
  END DO
END DO

END PROCEDURE OrthogonalBasis_Tetrahedron2

!----------------------------------------------------------------------------
!                                       BarycentricVertexBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricVertexBasis_Tetrahedron
ans = TRANSPOSE(lambda(1:4, :))
END PROCEDURE BarycentricVertexBasis_Tetrahedron

!----------------------------------------------------------------------------
!                                 BarycentricVertexBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricVertexBasisGradient_Tetrahedron
INTEGER(I4B) :: ii
REAL(DFP) :: eye4_(4, 4)
eye4_ = eye(4_I4B, 1.0_DFP)
DO CONCURRENT(ii=1:SIZE(ans, 1))
  ans(ii, :, :) = eye4_
END DO
END PROCEDURE BarycentricVertexBasisGradient_Tetrahedron

!----------------------------------------------------------------------------
!                                          BarycentricEdgeBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricEdgeBasis_Tetrahedron
REAL(DFP) :: d_lambda(6 * SIZE(lambda, 2))
REAL(DFP) :: phi( &
  & 1:6 * SIZE(lambda, 2), &
  & 0:MAX( &
  & pe1 - 2, &
  & pe2 - 2, &
  & pe3 - 2, &
  & pe4 - 2, &
  & pe5 - 2, &
  & pe6 - 2))
INTEGER(I4B) :: maxP, tPoints, i1, i2

tPoints = SIZE(lambda, 2)
maxP = SIZE(phi, 2) - 1

i1 = 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(2, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(3, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(4, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(3, :) - lambda(2, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(4, :) - lambda(2, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(4, :) - lambda(3, :)

phi = LobattoKernelEvalAll(n=maxP, x=d_lambda)

ans = BarycentricEdgeBasis_Tetrahedron2( &
  & pe1=pe1, &
  & pe2=pe2, &
  & pe3=pe3, &
  & pe4=pe4, &
  & pe5=pe5, &
  & pe6=pe6, &
  & lambda=lambda, &
  & phi=phi &
  & )

END PROCEDURE BarycentricEdgeBasis_Tetrahedron

!----------------------------------------------------------------------------
!                                          BarycentricEdgeBasis_Tetrahedron2
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricEdgeBasis_Tetrahedron2
INTEGER(I4B) :: tPoints, a, ii, i1, i2
REAL(DFP) :: temp(SIZE(lambda, 2))

ans = 0.0_DFP
tPoints = SIZE(temp)

!! edge(1) = (v1, v2)
a = 0
temp = lambda(1, :) * lambda(2, :)
i1 = 1
i2 = i1 + tPoints - 1
DO ii = 1, pe1 - 1
  a = a + 1
  ans(:, a) = temp * phi(i1:i2, ii - 1)
END DO

!! edge(2) = (v1, v3)
temp = lambda(1, :) * lambda(3, :)
i1 = i2 + 1
i2 = i1 + tPoints - 1
DO ii = 1, pe2 - 1
  a = a + 1
  ans(:, a) = temp * phi(i1:i2, ii - 1)
END DO

!! edge(3) = (v1, v4)
temp = lambda(1, :) * lambda(4, :)
i1 = i2 + 1
i2 = i1 + tPoints - 1
DO ii = 1, pe3 - 1
  a = a + 1
  ans(:, a) = temp * phi(i1:i2, ii - 1)
END DO

!! edge(4) = (v2, v3)
temp = lambda(2, :) * lambda(3, :)
i1 = i2 + 1
i2 = i1 + tPoints - 1
DO ii = 1, pe4 - 1
  a = a + 1
  ans(:, a) = temp * phi(i1:i2, ii - 1)
END DO

!! edge(5) = (v2, v4)
temp = lambda(2, :) * lambda(4, :)
i1 = i2 + 1
i2 = i1 + tPoints - 1
DO ii = 1, pe5 - 1
  a = a + 1
  ans(:, a) = temp * phi(i1:i2, ii - 1)
END DO

!! edge(5) = (v3, v4)
temp = lambda(3, :) * lambda(4, :)
i1 = i2 + 1
i2 = i1 + tPoints - 1
DO ii = 1, pe6 - 1
  a = a + 1
  ans(:, a) = temp * phi(i1:i2, ii - 1)
END DO

END PROCEDURE BarycentricEdgeBasis_Tetrahedron2

!----------------------------------------------------------------------------
!                                   BarycentricEdgeBasisGradient_Tetrahedron2
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricEdgeBasisGradient_Tetrahedron2
INTEGER(I4B) :: a, ii, i1, i2, edges(2, 6), orders(6), iedge, v1, v2, &
  & tPoints
REAL(DFP) :: temp(SIZE(lambda, 2), 6)

tPoints = SIZE(lambda, 2)
ans = 0.0_DFP
a = 0
i2 = 0
temp(:, 1) = lambda(1, :)
temp(:, 2) = lambda(2, :)
temp(:, 3) = lambda(3, :)
temp(:, 4) = lambda(4, :)

edges = EdgeConnectivity_Tetrahedron( &
  & baseinterpol="Lagrange", &
  & basecontinuity="H1")
orders = [pe1, pe2, pe3, pe4, pe5, pe6]

DO iedge = 1, SIZE(edges, 2)
  v1 = edges(1, iedge); v2 = edges(2, iedge)
  temp(:, 5) = temp(:, v1) * temp(:, v2)
  i1 = i2 + 1; i2 = i1 + tPoints - 1
  DO ii = 1, orders(iedge) - 1
    a = a + 1
    temp(:, 6) = temp(:, 5) * dphi(i1:i2, ii - 1)
    ans(:, a, v1) = temp(:, v2) * phi(i1:i2, ii - 1) - temp(:, 6)
    ans(:, a, v2) = temp(:, v1) * phi(i1:i2, ii - 1) + temp(:, 6)
  END DO
END DO
END PROCEDURE BarycentricEdgeBasisGradient_Tetrahedron2

!----------------------------------------------------------------------------
!                                         BarycentricFacetBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricFacetBasis_Tetrahedron
REAL(DFP) :: d_lambda(6 * SIZE(lambda, 2))
REAL(DFP) :: phi( &
  & 1:6 * SIZE(lambda, 2), &
  & 0:MAX( &
  & ps1 - 1, &
  & ps2 - 1, &
  & ps3 - 1, &
  & ps4 - 1))
INTEGER(I4B) :: maxP, tPoints, i1, i2

tPoints = SIZE(lambda, 2)
maxP = SIZE(phi, 2) - 1

i1 = 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(2, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(3, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(4, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(3, :) - lambda(2, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(4, :) - lambda(2, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(4, :) - lambda(3, :)

phi = LobattoKernelEvalAll(n=maxP, x=d_lambda)
ans = BarycentricFacetBasis_Tetrahedron2( &
  & ps1=ps1, &
  & ps2=ps2, &
  & ps3=ps3, &
  & ps4=ps4, &
  & lambda=lambda, &
  & phi=phi)

END PROCEDURE BarycentricFacetBasis_Tetrahedron

!----------------------------------------------------------------------------
!                                         BarycentricFacetBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricFacetBasis_Tetrahedron2
REAL(DFP) :: temp(SIZE(lambda, 2))
INTEGER(I4B) :: tPoints, i1, i2, ii, a
INTEGER(I4B) :: i21(2), i31(2), i41(2), i32(2), i42(2), i43(2)
INTEGER(I4B) :: facetConn(3, 4), fid, n1, n2, cnt, indx1(2, 4), indx2(2, 4)

tPoints = SIZE(temp)

i21 = [1, tPoints]
i31 = i21 + tPoints
i41 = i31 + tPoints
i32 = i41 + tPoints
i42 = i32 + tPoints
i43 = i42 + tPoints
facetConn = FacetConnectivity_Tetrahedron( &
  & baseInterpol="HIERARCHY", &
  & baseContinuity="H1")
indx1 = ((i21.rowconcat.i21) .rowconcat.i31) .rowconcat.i32
indx2 = ((i31.rowconcat.i41) .rowconcat.i41) .rowconcat.i42

ans = 0.0_DFP
i2 = 0
cnt = 0

!! Face1
DO fid = 1, SIZE(facetConn, 2)
  temp = lambda(facetConn(1, fid), :) &
        & * lambda(facetConn(2, fid), :)  &
        & * lambda(facetConn(3, fid), :)
  DO n1 = 1, ps1 - 1
    DO n2 = 1, ps1 - 1 - n1
      cnt = cnt + 1
      ans(:, cnt) = temp  &
                   & * phi(indx1(1, fid):indx1(2, fid), n1 - 1)  &
                   & * phi(indx2(1, fid):indx2(2, fid), n2 - 1)
    END DO
  END DO
END DO

END PROCEDURE BarycentricFacetBasis_Tetrahedron2

!----------------------------------------------------------------------------
!                                 BarycentricFacetBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricFacetBasisGradient_Tetrahedron2
REAL(DFP) :: temp(SIZE(lambda, 2), 8)
INTEGER(I4B) :: tPoints, i1, i2, ii, a, v1, v2, v3
INTEGER(I4B) :: i21(2), i31(2), i41(2), i32(2), i42(2), i43(2)
INTEGER(I4B) :: facetConn(3, 4), fid, n1, n2, cnt, indx1(2, 4), indx2(2, 4)

tPoints = SIZE(lambda, 2)
i21 = [1, tPoints]
i31 = i21 + tPoints
i41 = i31 + tPoints
i32 = i41 + tPoints
i42 = i32 + tPoints
i43 = i42 + tPoints
facetConn = FacetConnectivity_Tetrahedron( &
  & baseInterpol="HIERARCHY", &
  & baseContinuity="H1")
indx1 = ((i21.rowconcat.i21) .rowconcat.i31) .rowconcat.i32
indx2 = ((i31.rowconcat.i41) .rowconcat.i41) .rowconcat.i42

ans = 0.0_DFP
cnt = 0
temp(:, 1) = lambda(1, :)
temp(:, 2) = lambda(2, :)
temp(:, 3) = lambda(3, :)
temp(:, 4) = lambda(4, :)

DO fid = 1, SIZE(facetConn, 2)
  v1 = facetConn(1, fid)
  v2 = facetConn(2, fid)
  v3 = facetConn(3, fid)
  i1 = indx1(1, fid)
  i2 = indx1(1, fid)
  temp(:, 5) = temp(:, v1) * temp(:, v2) * temp(:, v3)

  DO n1 = 1, ps1 - 1
    DO n2 = 1, ps1 - 1 - n1
      cnt = cnt + 1
      temp(:, 6) = phi(i1:i2, n1 - 1) * phi(i1:i2, n2 - 1)
      temp(:, 7) = temp(:, 5) * dphi(i1:i2, n1 - 1) * phi(i1:i2, n2 - 1)
      temp(:, 8) = temp(:, 5) * phi(i1:i2, n1 - 1) * dphi(i1:i2, n2 - 1)

      ans(:, cnt, v1) = temp(:, v2) * temp(:, v3) * temp(:, 6) &
                    & - temp(:, 7) - temp(:, 8)

      ans(:, cnt, v2) = temp(:, v1) * temp(:, v3) * temp(:, 6) &
                    & + temp(:, 7)

      ans(:, cnt, v3) = temp(:, v1) * temp(:, v2) * temp(:, 6) &
                    & + temp(:, 8)
    END DO
  END DO
END DO
END PROCEDURE BarycentricFacetBasisGradient_Tetrahedron2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCellBasis_Tetrahedron
REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))
REAL(DFP) :: phi(1:3 * SIZE(lambda, 2), 0:pb)
INTEGER(I4B) :: maxP, tPoints, i1, i2

tPoints = SIZE(lambda, 2)
maxP = SIZE(phi, 2) - 1

i1 = 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(2, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(3, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(4, :) - lambda(1, :)

phi = LobattoKernelEvalAll(n=maxP, x=d_lambda)
ans = BarycentricCellBasis_Tetrahedron2( &
  & pb=pb, &
  & lambda=lambda, &
  & phi=phi)

END PROCEDURE BarycentricCellBasis_Tetrahedron

!----------------------------------------------------------------------------
!                                         BarycentricCellBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCellBasis_Tetrahedron2
REAL(DFP) :: temp(SIZE(lambda, 2))
INTEGER(I4B) :: tPoints
INTEGER(I4B) :: i21(2), i31(2), i41(2)
INTEGER(I4B) :: n1, n2, n3, cnt

tPoints = SIZE(temp)

i21 = [1, tPoints]
i31 = i21 + tPoints
i41 = i31 + tPoints

ans = 0.0_DFP
cnt = 0

temp = lambda(1, :) &
      & * lambda(2, :)  &
      & * lambda(3, :)  &
      & * lambda(4, :)

DO n1 = 1, pb - 1
  DO n2 = 1, pb - 1 - n1
    DO n3 = 1, pb - 1 - n1 - n2
      cnt = cnt + 1
      ans(:, cnt) = temp  &
                   & * phi(i21(1):i21(2), n1 - 1)  &
                   & * phi(i31(1):i31(2), n2 - 1)  &
                   & * phi(i41(1):i41(2), n3 - 1)
    END DO
  END DO
END DO

END PROCEDURE BarycentricCellBasis_Tetrahedron2

!----------------------------------------------------------------------------
!                                 BarycentricCellBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCellBasisGradient_Tetrahedron2
REAL(DFP) :: temp(SIZE(lambda, 2), 13)
INTEGER(I4B) :: tPoints
INTEGER(I4B) :: i21(2), i31(2), i41(2)
INTEGER(I4B) :: n1, n2, n3, cnt

tPoints = SIZE(lambda, 2)
i21 = [1, tPoints]
i31 = i21 + tPoints
i41 = i31 + tPoints
ans = 0.0_DFP
cnt = 0

temp(:, 1) = lambda(1, :)
temp(:, 2) = lambda(2, :)
temp(:, 3) = lambda(3, :)
temp(:, 4) = lambda(4, :)
temp(:, 5) = PRODUCT(temp(:, 1:4), dim=2)
temp(:, 6) = PRODUCT(temp(:, [2, 3, 4]), dim=2)
temp(:, 7) = PRODUCT(temp(:, [1, 3, 4]), dim=2)
temp(:, 8) = PRODUCT(temp(:, [1, 2, 4]), dim=2)
temp(:, 9) = PRODUCT(temp(:, [1, 2, 3]), dim=2)

DO n1 = 1, pb - 1
  DO n2 = 1, pb - 1 - n1
    DO n3 = 1, pb - 1 - n1 - n2
      cnt = cnt + 1
      temp(:, 10) = phi(i21(1):i21(2), n1 - 1)  &
                   & * phi(i31(1):i31(2), n2 - 1)  &
                   & * phi(i41(1):i41(2), n3 - 1)

      temp(:, 11) = temp(:, 5) * dphi(i21(1):i21(2), n1 - 1)  &
                   & * phi(i31(1):i31(2), n2 - 1)  &
                   & * phi(i41(1):i41(2), n3 - 1)

      temp(:, 12) = temp(:, 5) * phi(i21(1):i21(2), n1 - 1)  &
                   & * dphi(i31(1):i31(2), n2 - 1)  &
                   & * phi(i41(1):i41(2), n3 - 1)

      temp(:, 13) = temp(:, 5) * phi(i21(1):i21(2), n1 - 1)  &
                   & * phi(i31(1):i31(2), n2 - 1)  &
                   & * dphi(i41(1):i41(2), n3 - 1)

      ans(:, cnt, 1) = temp(:, 6) * temp(:, 10) &
                    &- temp(:, 11) - temp(:, 12) - temp(:, 13)

      ans(:, cnt, 2) = temp(:, 7) * temp(:, 10) + temp(:, 11)
      ans(:, cnt, 3) = temp(:, 8) * temp(:, 10) + temp(:, 12)
      ans(:, cnt, 4) = temp(:, 9) * temp(:, 10) + temp(:, 13)
    END DO
  END DO
END DO

END PROCEDURE BarycentricCellBasisGradient_Tetrahedron2

!----------------------------------------------------------------------------
!                                  BarycentricHeirarchicalBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricHeirarchicalBasis_Tetrahedron1
REAL(DFP) :: phi( &
  & 1:6 * SIZE(lambda, 2), &
  & 0:MAX(  &
  & pe1 - 2, &
  & pe2 - 2, &
  & pe3 - 2, &
  & pe4 - 2, &
  & pe5 - 2, &
  & pe6 - 2, &
  & ps1 - 1, &
  & ps2 - 1, &
  & ps3 - 1, &
  & ps4 - 1, &
  & order &
  & ))
REAL(DFP) :: d_lambda(6 * SIZE(lambda, 2))
INTEGER(I4B) :: a, b, maxP, tPoints, i1, i2

tPoints = SIZE(lambda, 2)
maxP = SIZE(phi, 2) - 1_I4B

i1 = 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(2, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(3, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(4, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(3, :) - lambda(2, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(4, :) - lambda(2, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(4, :) - lambda(3, :)

phi = LobattoKernelEvalAll(n=maxP, x=d_lambda)

!! Vertex basis function
ans = 0.0_DFP
ans(:, 1:4) = BarycentricVertexBasis_Tetrahedron(lambda=lambda)
b = 4

!! Edge basis function
IF (ANY([pe1, pe2, pe3, pe4, pe5, pe6] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6
  ans(:, a:b) = BarycentricEdgeBasis_Tetrahedron2( &
    & pe1=pe1,  &
    & pe2=pe2, &
    & pe3=pe3, &
    & pe4=pe4, &
    & pe5=pe5, &
    & pe6=pe6, &
    & lambda=lambda, &
    & phi=phi  &
    & )
END IF

!! Facet basis function
IF (ANY([ps1, ps2, ps3, ps4] .GE. 3_I4B)) THEN
  a = b + 1
  b = a - 1  &
    & + (ps1 - 1_I4B) * (ps1 - 2_I4B) / 2_I4B  &
    & + (ps2 - 1_I4B) * (ps2 - 2_I4B) / 2_I4B  &
    & + (ps3 - 1_I4B) * (ps3 - 2_I4B) / 2_I4B  &
    & + (ps4 - 1_I4B) * (ps4 - 2_I4B) / 2_I4B

  ans(:, a:b) = BarycentricFacetBasis_Tetrahedron2( &
    & ps1=ps1, &
    & ps2=ps2, &
    & ps3=ps3, &
    & ps4=ps4, &
    & lambda=lambda, &
    & phi=phi  &
    & )
END IF

!! Cell basis function
IF (order .GE. 4_I4B) THEN
  a = b + 1
  b = a - 1 &
    & + (order - 1_I4B) * (order - 2_I4B) * (order - 3_I4B) / 6_I4B

  ans(:, a:b) = BarycentricCellBasis_Tetrahedron2( &
    & pb=order, &
    & lambda=lambda, &
    & phi=phi)
END IF
END PROCEDURE BarycentricHeirarchicalBasis_Tetrahedron1

!----------------------------------------------------------------------------
!                                  BarycentricHeirarchicalBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricHeirarchicalBasis_Tetrahedron2
ans = BarycentricHeirarchicalBasis_Tetrahedron( &
    & order=order,  &
    & pe1=order,  &
    & pe2=order,  &
    & pe3=order,  &
    & pe4=order,  &
    & pe5=order,  &
    & pe6=order,  &
    & ps1=order, &
    & ps2=order,  &
    & ps3=order,  &
    & ps4=order,  &
    & lambda=lambda  &
    & )
END PROCEDURE BarycentricHeirarchicalBasis_Tetrahedron2

!----------------------------------------------------------------------------
!                         BarycentricHeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricHeirarchicalBasisGradient_Tetrahedron1
REAL(DFP) :: phi( &
  & 1:6 * SIZE(lambda, 2), &
  & 0:MAX(  &
  & pe1 - 2, &
  & pe2 - 2, &
  & pe3 - 2, &
  & pe4 - 2, &
  & pe5 - 2, &
  & pe6 - 2, &
  & ps1 - 1, &
  & ps2 - 1, &
  & ps3 - 1, &
  & ps4 - 1, &
  & order &
  & ))
REAL(DFP) :: dphi( &
  & 1:6 * SIZE(lambda, 2), &
  & 0:MAX(  &
  & pe1 - 2, &
  & pe2 - 2, &
  & pe3 - 2, &
  & pe4 - 2, &
  & pe5 - 2, &
  & pe6 - 2, &
  & ps1 - 1, &
  & ps2 - 1, &
  & ps3 - 1, &
  & ps4 - 1, &
  & order &
  & ))
REAL(DFP) :: d_lambda(6 * SIZE(lambda, 2))
INTEGER(I4B) :: a, b, maxP, tPoints, i1, i2

tPoints = SIZE(lambda, 2)
maxP = SIZE(phi, 2) - 1_I4B

i1 = 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(2, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(3, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(4, :) - lambda(1, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(3, :) - lambda(2, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(4, :) - lambda(2, :)

i1 = i2 + 1
i2 = i1 + tPoints - 1
d_lambda(i1:i2) = lambda(4, :) - lambda(3, :)

phi = LobattoKernelEvalAll(n=maxP, x=d_lambda)
dphi = LobattoKernelGradientEvalAll(n=maxP, x=d_lambda)

!! Vertex basis function
ans = 0.0_DFP
ans(:, 1:4, :) = BarycentricVertexBasisGradient_Tetrahedron(lambda=lambda)
b = 4

!! Edge basis function
IF (ANY([pe1, pe2, pe3, pe4, pe5, pe6] .GE. 2_I4B)) THEN
  a = b + 1
  b = a - 1 + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6
  ans(:, a:b, :) = BarycentricEdgeBasisGradient_Tetrahedron2( &
    & pe1=pe1,  &
    & pe2=pe2, &
    & pe3=pe3, &
    & pe4=pe4, &
    & pe5=pe5, &
    & pe6=pe6, &
    & lambda=lambda, &
    & phi=phi,  &
    & dphi=dphi  &
    & )
END IF

!! Facet basis function
IF (ANY([ps1, ps2, ps3, ps4] .GE. 3_I4B)) THEN
  a = b + 1
  b = a - 1  &
    & + (ps1 - 1_I4B) * (ps1 - 2_I4B) / 2_I4B  &
    & + (ps2 - 1_I4B) * (ps2 - 2_I4B) / 2_I4B  &
    & + (ps3 - 1_I4B) * (ps3 - 2_I4B) / 2_I4B  &
    & + (ps4 - 1_I4B) * (ps4 - 2_I4B) / 2_I4B

  ans(:, a:b, :) = BarycentricFacetBasisGradient_Tetrahedron2( &
    & ps1=ps1, &
    & ps2=ps2, &
    & ps3=ps3, &
    & ps4=ps4, &
    & lambda=lambda, &
    & phi=phi,  &
    & dphi=dphi  &
    & )
END IF

!! Cell basis function
IF (order .GE. 4_I4B) THEN
  a = b + 1
  b = a - 1 &
    & + (order - 1_I4B) * (order - 2_I4B) * (order - 3_I4B) / 6_I4B

  ans(:, a:b, :) = BarycentricCellBasisGradient_Tetrahedron2( &
    & pb=order, &
    & lambda=lambda, &
    & phi=phi, dphi=dphi)
END IF
END PROCEDURE BarycentricHeirarchicalBasisGradient_Tetrahedron1

!----------------------------------------------------------------------------
!                         BarycentricHeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricHeirarchicalBasisGradient_Tetrahedron2
ans = BarycentricHeirarchicalBasisGradient_Tetrahedron( &
    & order=order,  &
    & pe1=order,  &
    & pe2=order,  &
    & pe3=order,  &
    & pe4=order,  &
    & pe5=order,  &
    & pe6=order,  &
    & ps1=order, &
    & ps2=order,  &
    & ps3=order,  &
    & ps4=order,  &
    & lambda=lambda  &
    & )
END PROCEDURE BarycentricHeirarchicalBasisGradient_Tetrahedron2

!----------------------------------------------------------------------------
!                                                  VertexBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Tetrahedron
ans = BarycentricVertexBasis_Tetrahedron(&
  & lambda=BarycentricCoordTetrahedron( &
  & xin=xij, &
  & refTetrahedron=refTetrahedron))
END PROCEDURE VertexBasis_Tetrahedron

!----------------------------------------------------------------------------
!                                                  EdgeBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeBasis_Tetrahedron
ans = BarycentricEdgeBasis_Tetrahedron(&
  & lambda=BarycentricCoordTetrahedron( &
  & xin=xij, &
  & refTetrahedron=refTetrahedron), &
  & pe1=pe1,  &
  & pe2=pe2,  &
  & pe3=pe3,  &
  & pe4=pe4,  &
  & pe5=pe5,  &
  & pe6=pe6)
END PROCEDURE EdgeBasis_Tetrahedron

!----------------------------------------------------------------------------
!                                                  FacetBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FacetBasis_Tetrahedron
ans = BarycentricFacetBasis_Tetrahedron(&
  & lambda=BarycentricCoordTetrahedron( &
  & xin=xij, &
  & refTetrahedron=refTetrahedron), &
  & ps1=ps1,  &
  & ps2=ps2,  &
  & ps3=ps3,  &
  & ps4=ps4)
END PROCEDURE FacetBasis_Tetrahedron

!----------------------------------------------------------------------------
!                                                  CellBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Tetrahedron
ans = BarycentricCellBasis_Tetrahedron(&
  & lambda=BarycentricCoordTetrahedron( &
    & xin=xij, &
    & refTetrahedron=refTetrahedron), &
  & pb=pb)
END PROCEDURE CellBasis_Tetrahedron

!----------------------------------------------------------------------------
!                                             HeirarchicalBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Tetrahedron1
ans = BarycentricHeirarchicalBasis_Tetrahedron(&
  & lambda=BarycentricCoordTetrahedron( &
    & xin=xij, &
    & refTetrahedron=refTetrahedron), &
  & order=order, &
  & pe1=pe1,  &
  & pe2=pe2,  &
  & pe3=pe3,  &
  & pe4=pe4,  &
  & pe5=pe5,  &
  & pe6=pe6,  &
  & ps1=ps1,  &
  & ps2=ps2,  &
  & ps3=ps3,  &
  & ps4=ps4)
END PROCEDURE HeirarchicalBasis_Tetrahedron1

!----------------------------------------------------------------------------
!                                             HeirarchicalBasis_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Tetrahedron2
ans = BarycentricHeirarchicalBasis_Tetrahedron(&
  & lambda=BarycentricCoordTetrahedron( &
    & xin=xij, &
    & refTetrahedron=refTetrahedron), &
  & order=order)
END PROCEDURE HeirarchicalBasis_Tetrahedron2

!----------------------------------------------------------------------------
!                                               LagrangeEvallAll_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Tetrahedron1
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, tdof
INTEGER(I4B) :: degree(SIZE(xij, 2), 3)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), xx(1, SIZE(xij, 2))
TYPE(String) :: ref0

basisType0 = INPUT(default=Monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)
ref0 = INPUT(default="UNIT", option=refTetrahedron)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    coeff = LagrangeCoeff_Tetrahedron(&
      & order=order, &
      & xij=xij, &
      & basisType=basisType0, &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda, &
      & refTetrahedron=ref0%chars() &
      & )
    coeff0 = TRANSPOSE(coeff)
  ELSE
    coeff0 = TRANSPOSE(coeff)
  END IF
ELSE
  coeff0 = TRANSPOSE( &
    & LagrangeCoeff_Tetrahedron(&
    & order=order, &
    & xij=xij, &
    & basisType=basisType0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda, &
    & refTetrahedron=ref0%chars() &
    & ))
END IF

SELECT CASE (basisType0)

CASE (Monomial)

  degree = LagrangeDegree_Tetrahedron(order=order)
  tdof = SIZE(xij, 2)

  IF (tdof .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(&
      & msg="tdof is not same as size(degree,1)", &
      & file=__FILE__, &
      & routine="LagrangeEvalAll_Tetrahedron1", &
      & line=__LINE__, &
      & unitno=stderr)
    RETURN
  END IF

  DO ii = 1, tdof
    xx(1, ii) = x(1)**degree(ii, 1) &
               & * x(2)**degree(ii, 2) &
               & * x(3)**degree(ii, 3)
  END DO

CASE (Heirarchical)

  xx = HeirarchicalBasis_Tetrahedron( &
    & order=order, &
    & xij=RESHAPE(x, [3, 1]), &
    & refTetrahedron=ref0%chars())

CASE DEFAULT
  xx = OrthogonalBasis_Tetrahedron( &
    & order=order, &
    & xij=RESHAPE(x, [3, 1]),  &
    & refTetrahedron=ref0%chars() &
    & )

END SELECT

ans = MATMUL(coeff0, xx(1, :))

END PROCEDURE LagrangeEvalAll_Tetrahedron1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeEvalAll_Tetrahedron2
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, tdof
INTEGER(I4B) :: degree(SIZE(xij, 2), 3)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2))
REAL(DFP) :: xx(SIZE(x, 2), SIZE(xij, 2))
TYPE(String) :: ref0

basisType0 = INPUT(default=Monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)
ref0 = INPUT(default="UNIT", option=refTetrahedron)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    coeff = LagrangeCoeff_Tetrahedron(&
      & order=order, &
      & xij=xij, &
      & basisType=basisType0, &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda, &
      & refTetrahedron=ref0%chars() &
      & )
  END IF
  coeff0 = coeff
ELSE
  coeff0 = LagrangeCoeff_Tetrahedron(&
    & order=order, &
    & xij=xij, &
    & basisType=basisType0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda, &
    & refTetrahedron=ref0%chars() &
    & )
END IF

SELECT CASE (basisType0)

CASE (Monomial)

  degree = LagrangeDegree_Tetrahedron(order=order)
  tdof = SIZE(xij, 2)

  IF (tdof .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(&
      & msg="tdof is not same as size(degree,1)", &
      & file=__FILE__, &
      & routine="LagrangeEvalAll_Tetrahedron1", &
      & line=__LINE__, &
      & unitno=stderr)
    RETURN
  END IF

  DO ii = 1, tdof
    xx(:, ii) = x(1, :)**degree(ii, 1)  &
               & * x(2, :)**degree(ii, 2)  &
               & * x(3, :)**degree(ii, 3)
  END DO

CASE (Heirarchical)

  xx = HeirarchicalBasis_Tetrahedron( &
    & order=order, &
    & xij=x, &
    & refTetrahedron=ref0%chars())

CASE DEFAULT

  xx = OrthogonalBasis_Tetrahedron( &
    & order=order, &
    & xij=x,  &
    & refTetrahedron=ref0%chars() &
    & )

END SELECT

ans = MATMUL(xx, coeff0)

END PROCEDURE LagrangeEvalAll_Tetrahedron2

!----------------------------------------------------------------------------
!                                               QuadraturePoint_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Tetrahedron1
REAL(DFP), ALLOCATABLE :: temp_t(:, :)
TYPE(string) :: astr

IF (order .LE. MAX_ORDER_TETRAHEDRON_SOLIN) THEN
  astr = TRIM(UpperCase(refTetrahedron))
  temp_t = QuadraturePointTetrahedronSolin(order=order)
  CALL Reallocate(ans, 4_I4B, SIZE(temp_t, 2, kind=I4B))

  IF (PRESENT(xij)) THEN
    ans(1:3, :) = FromUnitTetrahedron2Tetrahedron(  &
      & xin=temp_t(1:3, :), &
      & x1=xij(:, 1), &
      & x2=xij(:, 2), &
      & x3=xij(:, 3), &
      & x4=xij(:, 4) &
      & )

    ans(4, :) = temp_t(4, :) * JacobianTetrahedron( &
      & from="UNIT", &
      & to="TETRAHEDRON", &
      & xij=xij)

  ELSE

    IF (astr%chars() .EQ. "BIUNIT") THEN
      ans(1:3, :) = FromUnitTetrahedron2BiUnitTetrahedron(xin=temp_t(1:3, :))
      ans(4, :) = temp_t(4, :) * JacobianTetrahedron( &
        & from="UNIT", &
        & to="BIUNIT")

    ELSE
      ans = temp_t
    END IF
  END IF

  IF (ALLOCATED(temp_t)) DEALLOCATE (temp_t)
ELSE
  ans = TensorQuadraturepoint_Tetrahedron( &
    & order=order, &
    & quadtype=quadtype, &
    & refTetrahedron=refTetrahedron, &
    & xij=xij)
END IF
END PROCEDURE QuadraturePoint_Tetrahedron1

!----------------------------------------------------------------------------
!                                              QuadraturePoint_Tetrahedron2
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Tetrahedron2
INTEGER(I4B) :: order
order = QuadratureOrderTetrahedronSolin(nips(1))
IF (order .LT. 0) THEN
  ans = Quadraturepoint_Tetrahedron1( &
    & order=order,  &
    & quadtype=quadType, &
    & refTetrahedron=refTetrahedron, &
    & xij=xij)
ELSE
  CALL Errormsg(&
    & msg="This routine is available for nips = [  &
    &  1, 4, 5, 11, 14, 24, 31, 43, 53, 126, 210, 330, 495, 715, 1001]  &
    & TRY CALLING TensorQuadraturePoint_Tetrahedron() instead.", &
    & file=__FILE__, &
    & routine="QuadraturePoint_Tetrahedron2()", &
    & line=__LINE__, &
    & unitno=stderr)
END IF
END PROCEDURE QuadraturePoint_Tetrahedron2

!----------------------------------------------------------------------------
!                                         TensorQuadraturePoint_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorQuadraturePoint_Tetrahedron1
INTEGER(I4B) :: n(4)
n = 1_I4B + INT(order / 2, kind=I4B)
n(2) = n(2) + 1
ans = TensorQuadraturePoint_Tetrahedron2( &
  & nipsx=n(1), &
  & nipsy=n(2), &
  & nipsz=n(3), &
  & quadType=quadType, &
  & refTetrahedron=refTetrahedron, &
  & xij=xij)
END PROCEDURE TensorQuadraturePoint_Tetrahedron1

!----------------------------------------------------------------------------
!                                         TensorQuadraturePoint_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorQuadraturePoint_Tetrahedron2
INTEGER(I4B) :: n(3), nsd
REAL(DFP), ALLOCATABLE :: temp_q(:, :), temp_t(:, :)
TYPE(String) :: astr

astr = TRIM(UpperCase(refTetrahedron))
n(1) = nipsx(1)
n(2) = nipsy(1)
n(3) = nipsz(1)

temp_q = QuadraturePoint_Hexahedron(&
  & nipsx=n(1:1),  &
  & nipsy=n(2:2),  &
  & nipsz=n(3:3),  &
  & quadType1=GaussLegendreLobatto, &
  & quadType2=GaussJacobiRadauLeft, &
  & quadType3=GaussJacobiRadauLeft, &
  & refHexahedron="BIUNIT", &
  & alpha2=1.0_DFP, &
  & beta2=0.0_DFP, &
  & alpha3=2.0_DFP, &
  & beta3=0.0_DFP)

CALL Reallocate(temp_t, SIZE(temp_q, 1, KIND=I4B), SIZE(temp_q, 2, KIND=I4B))
temp_t(1:3, :) = FromBiUnitHexahedron2UnitTetrahedron(xin=temp_q(1:3, :))
temp_t(4, :) = temp_q(4, :) / 8.0_DFP
nsd = 3_I4B
CALL Reallocate(ans, 4_I4B, SIZE(temp_q, 2, KIND=I4B))

IF (PRESENT(xij)) THEN
  ans(1:3, :) = FromUnitTetrahedron2Tetrahedron(  &
    & xin=temp_t(1:3, :), &
    & x1=xij(:, 1), &
    & x2=xij(:, 2), &
    & x3=xij(:, 3), &
    & x4=xij(:, 4) &
    & )
  ans(4, :) = temp_t(4, :) * JacobianTetrahedron( &
    & from="UNIT", &
    & to="TETRAHEDRON", &
    & xij=xij)
ELSE
  IF (astr%chars() .EQ. "BIUNIT") THEN
    ans(1:3, :) = FromUnitTetrahedron2BiUnitTetrahedron(xin=temp_t(1:3, :))
    ans(4, :) = temp_t(4, :) * JacobianTetrahedron( &
      & from="UNIT", &
      & to="BIUNIT")
  ELSE
    ans = temp_t
  END IF
END IF

IF (ALLOCATED(temp_q)) DEALLOCATE (temp_q)
IF (ALLOCATED(temp_t)) DEALLOCATE (temp_t)
END PROCEDURE TensorQuadraturePoint_Tetrahedron2

!----------------------------------------------------------------------------
!                                       LagrangeGradientEvalAll_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeGradientEvalAll_Tetrahedron1
LOGICAL(LGT) :: firstCall0
INTEGER(I4B) :: ii, basisType0, tdof, ai, bi, ci
INTEGER(I4B) :: degree(SIZE(xij, 2), 3)
REAL(DFP) :: coeff0(SIZE(xij, 2), SIZE(xij, 2)), &
  & xx(SIZE(x, 2), SIZE(xij, 2), 3), ar, br, cr
TYPE(String) :: ref0

basisType0 = INPUT(default=Monomial, option=basisType)
firstCall0 = INPUT(default=.TRUE., option=firstCall)
ref0 = INPUT(default="UNIT", option=refTetrahedron)

IF (PRESENT(coeff)) THEN
  IF (firstCall0) THEN
    coeff = LagrangeCoeff_Tetrahedron(&
      & order=order, &
      & xij=xij, &
      & basisType=basisType0, &
      & alpha=alpha, &
      & beta=beta, &
      & lambda=lambda, &
      & refTetrahedron=ref0%chars() &
      & )
  END IF
  coeff0 = coeff
ELSE
  coeff0 = LagrangeCoeff_Tetrahedron(&
    & order=order, &
    & xij=xij, &
    & basisType=basisType0, &
    & alpha=alpha, &
    & beta=beta, &
    & lambda=lambda, &
    & refTetrahedron=ref0%chars() &
    & )
END IF

SELECT CASE (basisType0)

CASE (Monomial)

  degree = LagrangeDegree_Tetrahedron(order=order)
  tdof = SIZE(xij, 2)

  IF (tdof .NE. SIZE(degree, 1)) THEN
    CALL Errormsg(&
      & msg="tdof is not same as size(degree,1)", &
      & file=__FILE__, &
      & routine="LagrangeEvalAll_Tetrahedron1", &
      & line=__LINE__, &
      & unitno=stderr)
    RETURN
  END IF

  DO ii = 1, tdof
    ai = MAX(degree(ii, 1_I4B) - 1_I4B, 0_I4B)
    bi = MAX(degree(ii, 2_I4B) - 1_I4B, 0_I4B)
    ci = MAX(degree(ii, 3_I4B) - 1_I4B, 0_I4B)

    ar = REAL(degree(ii, 1_I4B), DFP)
    br = REAL(degree(ii, 2_I4B), DFP)
    cr = REAL(degree(ii, 3_I4B), DFP)

    xx(:, ii, 1) = (ar * x(1, :)**ai) *  &
                & x(2, :)**degree(ii, 2) *  &
                & x(3, :)**degree(ii, 3)

    xx(:, ii, 2) = x(1, :)**degree(ii, 1) *  &
                & (br * x(2, :)**bi) *  &
                & x(3, :)**degree(ii, 3)

    xx(:, ii, 3) = x(1, :)**degree(ii, 1) *  &
                & x(2, :)**degree(ii, 2) * &
                & (cr * x(2, :)**ci)
  END DO

CASE (Heirarchical)

  xx = HeirarchicalBasisGradient_Tetrahedron( &
    & order=order, &
    & xij=x, &
    & refTetrahedron=ref0%chars())

CASE DEFAULT

  xx = OrthogonalBasisGradient_Tetrahedron( &
    & order=order, &
    & xij=x,  &
    & refTetrahedron=ref0%chars() &
    & )

END SELECT

DO ii = 1, 3
  ! ans(:, ii, :) = TRANSPOSE(MATMUL(xx(:, :, ii), coeff0))
  ans(:, :, ii) = MATMUL(xx(:, :, ii), coeff0)
END DO

END PROCEDURE LagrangeGradientEvalAll_Tetrahedron1

!----------------------------------------------------------------------------
!                                       OrthogonalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasisGradient_Tetrahedron1
CHARACTER(20) :: layout
REAL(DFP) :: x(1:3, 1:SIZE(xij, 2))
REAL(DFP) :: P1(SIZE(xij, 2), 0:order)
REAL(DFP) :: Q1(SIZE(xij, 2), 0:order)
REAL(DFP) :: R1(SIZE(xij, 2), 0:order)
REAL(DFP) :: dP1(SIZE(xij, 2), 0:order)
REAL(DFP) :: dQ1(SIZE(xij, 2), 0:order)
REAL(DFP) :: dR1(SIZE(xij, 2), 0:order)
REAL(DFP) :: temp(SIZE(xij, 2), 10), areal, breal
INTEGER(I4B) :: cnt
INTEGER(I4B) :: p, q, r
LOGICAL(LGT) :: isBiunit
REAL(DFP) :: ans0(SIZE(ans, 1), SIZE(ans, 2), SIZE(ans, 3))

ans0 = 0.0_DFP
layout = TRIM(UpperCase(refTetrahedron))
SELECT CASE (TRIM(layout))
CASE ("BIUNIT")
  x = FromBiUnitTetrahedron2BiUnitHexahedron(xin=xij)
  isBiunit = .TRUE.
CASE ("UNIT")
  x = FromUnitTetrahedron2BiUnitHexahedron(xin=xij)
  isBiunit = .FALSE.
END SELECT

temp(:, 1) = 0.5_DFP * (1.0_DFP - x(2, :))
temp(:, 2) = 0.5_DFP * (1.0_DFP - x(3, :))

P1 = LegendreEvalAll(n=order, x=x(1, :))
dP1 = LegendreGradientEvalAll(n=order, x=x(1, :))
cnt = 0

DO p = 0, order
  areal = -0.5_DFP * REAL(p, DFP)

  Q1 = JacobiEvalAll( &
    & n=order, &
    & x=x(2, :), &
    & alpha=REAL(2 * p + 1, DFP), &
    & beta=0.0_DFP  &
    & )

  dQ1 = JacobiGradientEvalAll( &
    & n=order, &
    & x=x(2, :), &
    & alpha=REAL(2 * p + 1, DFP), &
    & beta=0.0_DFP  &
    & )

  temp(:, 3) = temp(:, 1)**MAX(p - 1_I4B, 0_I4B)
  temp(:, 4) = temp(:, 3) * temp(:, 1)

  DO q = 0, order - p

    breal = -0.5_DFP * REAL(p + q, DFP)

    R1 = JacobiEvalAll( &
      & n=order, &
      & x=x(3, :), &
      & alpha=REAL(2 * p + 2 * q + 2, DFP), &
      & beta=0.0_DFP  &
      & )

    dR1 = JacobiGradientEvalAll( &
      & n=order, &
      & x=x(3, :), &
      & alpha=REAL(2 * p + 2 * q + 2, DFP), &
      & beta=0.0_DFP  &
      & )

    temp(:, 5) = P1(:, p) * Q1(:, q)
    temp(:, 6) = P1(:, p) * dQ1(:, q)
    temp(:, 7) = dP1(:, p) * Q1(:, q)
    temp(:, 9) = temp(:, 2)**MAX(p + q - 1_I4B, 0_I4B)
    temp(:, 10) = temp(:, 9) * temp(:, 2)

    DO r = 0, order - p - q
      temp(:, 8) = temp(:, 5) * R1(:, r)
      cnt = cnt + 1
      ans0(:, cnt, 1) = temp(:, 7) * R1(:, r) * temp(:, 4) * temp(:, 10)
      ans0(:, cnt, 2) = temp(:, 8) * areal * temp(:, 3) * temp(:, 10)  &
        & + temp(:, 6) * R1(:, r) * temp(:, 4) * temp(:, 10)
      ans0(:, cnt, 2) = temp(:, 8) * breal * temp(:, 4) * temp(:, 9)  &
        & + temp(:, 5) * dR1(:, r) * temp(:, 4) * temp(:, 10)
    END DO
  END DO
END DO

IF (isBiunit) THEN
  temp(:, 1) = x(1, :)
  temp(:, 2) = x(2, :)
  temp(:, 3) = x(3, :)

  temp(:, 4) = 2.0_DFP / (temp(:, 2) + temp(:, 3))
  temp(:, 5) = (1.0_DFP + temp(:, 1)) * temp(:, 4) / (temp(:, 2) + temp(:, 3))
  temp(:, 6) = 2.0_DFP / (1.0_DFP - temp(:, 3))
  temp(:, 7) = 1.0_DFP / (1.0_DFP - temp(:, 3))**2

  DO CONCURRENT(p=1:SIZE(ans, 2))
    ans(:, p, 1) = -temp(:, 4) * ans0(:, p, 1)
    ans(:, p, 2) = temp(:, 5) * ans0(:, p, 1) + temp(:, 6) * ans0(:, p, 2)
    ans(:, p, 3) = temp(:, 5) * ans0(:, p, 1)  &
                & + temp(:, 7) * ans0(:, p, 2)  &
                & + ans0(:, p, 3)
  END DO

ELSE

  temp(:, 1:3) = FromUnitTetrahedron2BiUnitTetrahedron(x)

  temp(:, 4) = 2.0_DFP / (temp(:, 2) + temp(:, 3))
  temp(:, 5) = (1.0_DFP + temp(:, 1)) * temp(:, 4) / (temp(:, 2) + temp(:, 3))
  temp(:, 6) = 2.0_DFP / (1.0_DFP - temp(:, 3))
  temp(:, 7) = 1.0_DFP / (1.0_DFP - temp(:, 3))**2

  DO CONCURRENT(p=1:SIZE(ans, 2))
    ans(:, p, 1) = -temp(:, 4) * ans0(:, p, 1)
    ans(:, p, 2) = temp(:, 5) * ans0(:, p, 1) + temp(:, 6) * ans0(:, p, 2)
    ans(:, p, 3) = temp(:, 5) * ans0(:, p, 1)  &
                & + temp(:, 7) * ans0(:, p, 2)  &
                & + ans0(:, p, 3)
  END DO

  ans = 2.0_DFP * ans

END IF

END PROCEDURE OrthogonalBasisGradient_Tetrahedron1

!----------------------------------------------------------------------------
!                                     HeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Tetrahedron1
TYPE(String) :: name
REAL(DFP) :: ans0(SIZE(ans, 1), SIZE(ans, 2), 4)
ans0 = BarycentricHeirarchicalBasisGradient_Tetrahedron(&
  & lambda=BarycentricCoordTetrahedron( &
    & xin=xij, &
    & refTetrahedron=refTetrahedron), &
  & order=order, &
  & pe1=pe1,  &
  & pe2=pe2,  &
  & pe3=pe3,  &
  & pe4=pe4,  &
  & pe5=pe5,  &
  & pe6=pe6,  &
  & ps1=ps1,  &
  & ps2=ps2,  &
  & ps3=ps3,  &
  & ps4=ps4)

ans(:, :, 1) = ans0(:, :, 2) - ans0(:, :, 1)
ans(:, :, 2) = ans0(:, :, 3) - ans0(:, :, 1)
ans(:, :, 3) = ans0(:, :, 4) - ans0(:, :, 1)

name = UpperCase(refTetrahedron)
IF (name == "BIUNIT") THEN
  ans = 0.5_DFP * ans
END IF
END PROCEDURE HeirarchicalBasisGradient_Tetrahedron1

!----------------------------------------------------------------------------
!                                     HeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Tetrahedron2
ans = HeirarchicalBasisGradient_Tetrahedron1( &
  & order=order, &
  & pe1=order, &
  & pe2=order, &
  & pe3=order, &
  & pe4=order, &
  & pe5=order, &
  & pe6=order, &
  & ps1=order, &
  & ps2=order, &
  & ps3=order, &
  & ps4=order, &
  & xij=xij, &
  & refTetrahedron=refTetrahedron)
END PROCEDURE HeirarchicalBasisGradient_Tetrahedron2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!
END SUBMODULE Methods
