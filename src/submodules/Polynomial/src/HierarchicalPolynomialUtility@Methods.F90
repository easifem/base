! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(HierarchicalPolynomialUtility) Methods
USE GlobalData, ONLY: stderr

USE ReferenceElement_Method, ONLY: XiDimension, &
                                   GetTotalNodes, &
                                   ElementTopology, &
                                   GetTotalEdges

USE ErrorHandling, ONLY: ErrorMsg

USE BaseType, ONLY: elemopt => TypeElemNameOpt

USE LineInterpolationUtility, ONLY: HeirarchicalBasis_Line_, &
                                    HeirarchicalBasisGradient_Line_, &
                                    GetTotalInDOF_Line

USE TriangleInterpolationUtility, ONLY: HeirarchicalBasis_Triangle_, &
                                        HeirarchicalBasisGradient_Triangle_, &
                                        GetTotalInDOF_Triangle

USE QuadrangleInterpolationUtility, ONLY: HeirarchicalBasis_Quadrangle_, &
                                      HeirarchicalBasisGradient_Quadrangle_, &
                                          GetTotalInDOF_Quadrangle

USE TetrahedronInterpolationUtility, ONLY: HeirarchicalBasis_Tetrahedron_, &
                                     HeirarchicalBasisGradient_Tetrahedron_, &
                                           GetTotalInDOF_Tetrahedron

USE HexahedronInterpolationUtility, ONLY: HeirarchicalBasis_Hexahedron_, &
                                      HeirarchicalBasisGradient_Hexahedron_, &
                                          GetTotalInDOF_Hexahedron

USE PrismInterpolationUtility, ONLY: GetTotalInDOF_Prism

USE PyramidInterpolationUtility, ONLY: GetTotalInDOF_Pyramid

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalDOF
INTEGER(I4B) :: ii

ans = 0

ii = HierarchicalVertexDOF(elemType=elemType)
ans = ans + ii

IF (PRESENT(cellOrder)) THEN
  ii = HierarchicalCellDOF(elemType=elemType, order=cellOrder)
  ans = ans + ii
END IF

IF (PRESENT(faceOrder)) THEN
  ii = HierarchicalFaceDOF(elemType=elemType, order=faceOrder)
  ans = ans + ii
END IF

IF (PRESENT(edgeOrder)) THEN
  ii = HierarchicalEdgeDOF(elemType=elemType, order=edgeOrder)
  ans = ans + ii
END IF

END PROCEDURE HierarchicalDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalVertexDOF
ans = GetTotalNodes(elemType)
END PROCEDURE HierarchicalVertexDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalEdgeDOF
INTEGER(I4B) :: topo, ii, tedges

topo = ElementTopology(elemType)
ans = 0

SELECT CASE (topo)
CASE (elemopt%Tetrahedron, elemopt%Hexahedron, elemopt%Prism, elemopt%Pyramid)

  tedges = GetTotalEdges(topo)

  DO ii = 1, tedges
    ans = ans + GetTotalInDOF_Line(order=order(ii), baseContinuity="H1", &
                                   baseInterpolation="HEIRARCHICAL")
  END DO

END SELECT

END PROCEDURE HierarchicalEdgeDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalFaceDOF
INTEGER(I4B) :: topo, jj, ii

topo = ElementTopology(elemType)

ans = 0

SELECT CASE (topo)
CASE (elemopt%Point)
  ans = 0

CASE (elemopt%Line)
  ans = 0

CASE (elemopt%Triangle)
  DO ii = 1, 3
    jj = GetTotalInDOF_Line(order=order(1, ii), baseContinuity="H1", &
                            baseInterpolation="HEIRARCHICAL")
    ans = ans + jj
  END DO

CASE (elemopt%Quadrangle)
  DO ii = 1, 4
    jj = GetTotalInDOF_Line(order=order(1, ii), baseContinuity="H1", &
                            baseInterpolation="HEIRARCHICAL")
    ans = ans + jj
  END DO

CASE (elemopt%Tetrahedron)
  DO ii = 1, 4
    jj = GetTotalInDOF_Triangle(order=order(1, ii), baseContinuity="H1", &
                                baseInterpolation="HEIRARCHICAL")
    ans = ans + jj
  END DO

CASE (elemopt%Hexahedron)
  DO ii = 1, 6
    jj = GetTotalInDOF_Quadrangle(p=order(1, ii), q=order(2, ii), &
                                  baseContinuity="H1", &
                                  baseInterpolation="HEIRARCHICAL")
    ans = ans + jj
  END DO

! CASE (elemopt%Prism)
! CASE (elemopt%Pyramid)
END SELECT
END PROCEDURE HierarchicalFaceDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalCellDOF
INTEGER(I4B) :: topo

ans = 0
topo = ElementTopology(elemType)
SELECT CASE (topo)
CASE (elemopt%Point)
  ans = 0
CASE (elemopt%Line)
  ans = GetTotalInDOF_Line(order=order(1), baseContinuity="H1", &
                           baseInterpolation="HEIRARCHICAL")
CASE (elemopt%Triangle)
  ans = GetTotalInDOF_Triangle(order=order(1), baseContinuity="H1", &
                               baseInterpolation="HEIRARCHICAL")
CASE (elemopt%Quadrangle)
 ans = GetTotalInDOF_Quadrangle(p=order(1), q=order(2), baseContinuity="H1", &
                                 baseInterpolation="HEIRARCHICAL")
CASE (elemopt%Tetrahedron)
  ans = GetTotalInDOF_Tetrahedron(order=order(1), baseContinuity="H1", &
                                  baseInterpolation="HEIRARCHICAL")

CASE (elemopt%Hexahedron)
  ans = GetTotalInDOF_Hexahedron(p=order(1), q=order(2), r=order(3), &
                                 baseContinuity="H1", &
                                 baseInterpolation="HEIRARCHICAL")

CASE (elemopt%Prism)
  ans = GetTotalInDOF_Prism(order=order(1), baseContinuity="H1", &
                            baseInterpolation="HEIRARCHICAL")
CASE (elemopt%Pyramid)

  ans = GetTotalInDOF_Pyramid(order=order(1), baseContinuity="H1", &
                              baseInterpolation="HEIRARCHICAL")
END SELECT
END PROCEDURE HierarchicalCellDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalEvalAll
INTEGER(I4B) :: nrow, ncol

nrow = SIZE(xij, 2)
ncol = HierarchicalDOF(elemType=elemType, cellOrder=cellOrder, &
                       faceOrder=faceOrder, edgeOrder=edgeOrder)

ALLOCATE (ans(nrow, ncol))

CALL HierarchicalEvalAll_(elemType=elemType, xij=xij, ans=ans, &
           nrow=nrow, ncol=ncol, domainName=domainName, cellOrder=cellOrder, &
            faceOrder=faceOrder, edgeOrder=edgeOrder, cellOrient=cellOrient, &
                          faceOrient=faceOrient, edgeOrient=edgeOrient)

END PROCEDURE HierarchicalEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalEvalAll_
#ifdef DEBUG_VER
INTEGER(I4B) :: ierr
CHARACTER(*), PARAMETER :: routine = "HierarchicalEvalAll_()"
#endif

INTEGER(I4B) :: topo

nrow = 0; ncol = 0

topo = ElementTopology(elemType)

SELECT CASE (topo)

CASE (elemopt%Line)

#ifdef DEBUG_VER
  CALL check_error_1d(ierr=ierr, routine=routine, &
              cellOrder=cellOrder, faceOrder=faceOrder, edgeOrder=edgeOrder, &
          cellOrient=cellOrient, faceOrient=faceOrient, edgeOrient=edgeOrient)
  IF (ierr .LT. 0) RETURN
#endif

  CALL HeirarchicalBasis_Line_(order=cellOrder(1), xij=xij, ans=ans, &
               nrow=nrow, ncol=ncol, refLine=domainName, orient=cellOrient(1))

CASE (elemopt%Triangle)

#ifdef DEBUG_VER
  CALL check_error_2d(ierr=ierr, tface=3, routine=routine, &
              cellOrder=cellOrder, faceOrder=faceOrder, edgeOrder=edgeOrder, &
          cellOrient=cellOrient, faceOrient=faceOrient, edgeOrient=edgeOrient)
  IF (ierr .LT. 0) RETURN
#endif

  CALL HeirarchicalBasis_Triangle_(order=cellOrder(1), &
                                   pe1=faceOrder(1, 1), &
                                   pe2=faceOrder(1, 2), &
                                   pe3=faceOrder(1, 3), &
                                   xij=xij, &
                                   refTriangle=domainName, &
                                   ans=ans, nrow=nrow, ncol=ncol, &
                                   edgeOrient1=faceOrient(1, 1), &
                                   edgeOrient2=faceOrient(1, 2), &
                                   edgeOrient3=faceOrient(1, 3), &
                                   faceOrient=cellOrient)

CASE (elemopt%Quadrangle)

#ifdef DEBUG_VER
  CALL check_error_2d(ierr=ierr, tface=4, routine=routine, &
              cellOrder=cellOrder, faceOrder=faceOrder, edgeOrder=edgeOrder, &
          cellOrient=cellOrient, faceOrient=faceOrient, edgeOrient=edgeOrient)
  IF (ierr .LT. 0) RETURN
#endif

  CALL HeirarchicalBasis_Quadrangle_(pb=cellOrder(1), &
                                     qb=cellOrder(2), &
                                     pe3=faceOrder(1, 1), &
                                     pe4=faceOrder(1, 3), &
                                     qe1=faceOrder(1, 4), &
                                     qe2=faceOrder(1, 2), &
                                     xij=xij, &
                                     ans=ans, nrow=nrow, ncol=ncol, &
                                     pe3Orient=faceOrient(1, 1), &
                                     pe4Orient=faceOrient(1, 3), &
                                     qe1Orient=faceOrient(1, 4), &
                                     qe2Orient=faceOrient(1, 2), &
                                     faceOrient=cellOrient)

! CASE (elemopt%Tetrahedron)

!   CALL HeirarchicalBasis_Tetrahedron_(order=cellOrder(1), pe1=edgeOrder(1), &
!      pe2=edgeOrder(2), pe3=edgeOrder(3), pe4=edgeOrder(4), pe5=edgeOrder(5), &
!                  pe6=edgeOrder(6), ps1=faceOrder(1, 1), ps2=faceOrder(1, 2), &
!                           ps3=faceOrder(1, 3), ps4=faceOrder(1, 4), xij=xij, &
!                                       refTetrahedron=domainName, ans=ans, &
!                                       nrow=nrow, ncol=ncol)

! CASE (elemopt%Hexahedron)

!   CALL HeirarchicalBasis_Hexahedron_( &
!     pb1=cellOrder(1), pb2=cellOrder(2), pb3=cellOrder(3), &
!     pxy1=faceOrder(1, 1), pxy2=faceOrder(2, 1), &
!     pxz1=faceOrder(1, 2), pxz2=faceOrder(2, 2), &
!     pyz1=faceOrder(1, 3), pyz2=faceOrder(2, 3), &
!     px1=edgeOrder(1), px2=edgeOrder(2), px3=edgeOrder(3), px4=edgeOrder(4), &
!     py1=edgeOrder(5), py2=edgeOrder(6), py3=edgeOrder(7), py4=edgeOrder(8), &
!     pz1=edgeOrder(9), pz2=edgeOrder(10), pz3=edgeOrder(11), &
!     pz4=edgeOrder(12), xij=xij, ans=ans, nrow=nrow, ncol=ncol)

! CASE (elemopt%Prism)

! CASE (elemopt%Pyramid)

CASE DEFAULT
  CALL ErrorMsg(msg="No case found for topology", &
                routine='HierarchicalEvalAll_()', &
                file=__FILE__, line=__LINE__, unitno=stderr)

  RETURN
END SELECT

END PROCEDURE HierarchicalEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalGradientEvalAll
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = SIZE(xij, 2)
dim2 = HierarchicalDOF(elemType=elemType, cellOrder=cellOrder, &
                       faceOrder=faceOrder, edgeOrder=edgeOrder)
dim3 = XiDimension(elemType)

ALLOCATE (ans(dim1, dim2, dim3))

CALL HierarchicalGradientEvalAll_(elemType=elemType, xij=xij, &
            ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, domainName=domainName, &
              cellOrder=cellOrder, faceOrder=faceOrder, edgeOrder=edgeOrder, &
          cellOrient=cellOrient, faceOrient=faceOrient, edgeOrient=edgeOrient)
END PROCEDURE HierarchicalGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalGradientEvalAll_
#ifdef DEBUG_VER
INTEGER(I4B) :: ierr
CHARACTER(*), PARAMETER :: routine = "HierarchicalGradientEvalAll_()"
#endif

INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)

CASE (elemopt%Line)

#ifdef DEBUG_VER
  CALL check_error_2d(ierr=ierr, tface=3, routine=routine, &
              cellOrder=cellOrder, faceOrder=faceOrder, edgeOrder=edgeOrder, &
          cellOrient=cellOrient, faceOrient=faceOrient, edgeOrient=edgeOrient)
  IF (ierr .LT. 0) RETURN
#endif

  CALL HeirarchicalBasisGradient_Line_(order=cellOrder(1), xij=xij, ans=ans, &
    dim1=dim1, dim2=dim2, dim3=dim3, refLine=domainName, orient=cellOrient(1))

CASE (elemopt%Triangle)

#ifdef DEBUG_VER
  CALL check_error_2d(ierr=ierr, tface=3, routine=routine, &
              cellOrder=cellOrder, faceOrder=faceOrder, edgeOrder=edgeOrder, &
          cellOrient=cellOrient, faceOrient=faceOrient, edgeOrient=edgeOrient)
  IF (ierr .LT. 0) RETURN
#endif

  CALL HeirarchicalBasisGradient_Triangle_(order=cellOrder(1), &
                                           pe1=faceOrder(1, 1), &
                                           pe2=faceOrder(1, 2), &
                                           pe3=faceOrder(1, 3), &
                                           xij=xij, &
                                           refTriangle=domainName, &
                                           ans=ans, tsize1=dim1, &
                                           tsize2=dim2, tsize3=dim3, &
                                           edgeOrient1=faceOrient(1, 1), &
                                           edgeOrient2=faceOrient(1, 2), &
                                           edgeOrient3=faceOrient(1, 3), &
                                           faceOrient=cellOrient)

CASE (elemopt%Quadrangle)

#ifdef DEBUG_VER
  CALL check_error_2d(ierr=ierr, tface=4, routine=routine, &
              cellOrder=cellOrder, faceOrder=faceOrder, edgeOrder=edgeOrder, &
          cellOrient=cellOrient, faceOrient=faceOrient, edgeOrient=edgeOrient)
  IF (ierr .LT. 0) RETURN
#endif

  CALL HeirarchicalBasisGradient_Quadrangle_(pb=cellOrder(1), &
                                             qb=cellOrder(2), &
                                             pe3=faceOrder(1, 1), &
                                             qe2=faceOrder(1, 2), &
                                             pe4=faceOrder(1, 3), &
                                             qe1=faceOrder(1, 4), &
                                             xij=xij, &
                                             ans=ans, dim1=dim1, &
                                             dim2=dim2, dim3=dim3, &
                                             pe3Orient=faceOrient(1, 1), &
                                             qe2Orient=faceOrient(1, 2), &
                                             pe4Orient=faceOrient(1, 3), &
                                             qe1Orient=faceOrient(1, 4), &
                                             faceOrient=cellOrient)

! CASE (elemopt%Tetrahedron)

  ! CALL HeirarchicalBasisGradient_Tetrahedron_(order=cellOrder(1), &
  !                      pe1=edgeOrder(1), pe2=edgeOrder(2), pe3=edgeOrder(3), &
  !                      pe4=edgeOrder(4), pe5=edgeOrder(5), pe6=edgeOrder(6), &
  !             ps1=faceOrder(1, 1), ps2=faceOrder(1, 2), ps3=faceOrder(1, 3), &
  !                   ps4=faceOrder(1, 4), xij=xij, refTetrahedron=domainName, &
  !                                    ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

! CASE (elemopt%Hexahedron)

  ! CALL HeirarchicalBasisGradient_Hexahedron_( &
  !   pb1=cellOrder(1), pb2=cellOrder(2), pb3=cellOrder(3), &
  !   pxy1=faceOrder(1, 1), pxy2=faceOrder(2, 1), &
  !   pxz1=faceOrder(1, 2), pxz2=faceOrder(2, 2), &
  !   pyz1=faceOrder(1, 3), pyz2=faceOrder(2, 3), &
  !   px1=edgeOrder(1), px2=edgeOrder(2), px3=edgeOrder(3), px4=edgeOrder(4), &
  !   py1=edgeOrder(5), py2=edgeOrder(6), py3=edgeOrder(7), py4=edgeOrder(8), &
  !   pz1=edgeOrder(9), pz2=edgeOrder(10), pz3=edgeOrder(11), &
  !   pz4=edgeOrder(12), xij=xij, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

! CASE (elemopt%Prism)

! CASE (elemopt%Pyramid)

CASE DEFAULT
  CALL ErrorMsg(msg="No case found for topology", &
                routine='HierarchicalEvalAll_()', &
                file=__FILE__, line=__LINE__, unitno=stderr)

  RETURN
END SELECT

END PROCEDURE HierarchicalGradientEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE check_error_1d(ierr, routine, cellOrder, faceOrder, edgeOrder, &
                          cellOrient, faceOrient, edgeOrient)
  INTEGER(I4B), INTENT(OUT) :: ierr
  CHARACTER(*), INTENT(IN) :: routine
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)

  ! internal variables
  LOGICAL(LGT) :: isok
  CHARACTER(:), ALLOCATABLE :: errmsg

  ierr = 0
  isok = PRESENT(cellOrder)
  IF (.NOT. isok) THEN
    ierr = -1
    errmsg = "cellOrder is not present"
  END IF

  isok = PRESENT(cellOrient)
  IF (.NOT. isok) THEN
    ierr = -2
    errmsg = "cellOrient is not present"
  END IF

  IF (.NOT. isok) THEN
    CALL ErrorMsg(msg=errmsg, routine=routine, file=__FILE__, &
                  line=__LINE__, unitno=stderr)
    RETURN
  END IF

END SUBROUTINE check_error_1d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE check_error_2d(ierr, tface, routine, cellOrder, &
                     faceOrder, edgeOrder, cellOrient, faceOrient, edgeOrient)
  INTEGER(I4B), INTENT(OUT) :: ierr
  INTEGER(I4B), INTENT(IN) :: tface
  CHARACTER(*), INTENT(IN) :: routine
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)

  LOGICAL(LGT) :: isok
  CHARACTER(:), ALLOCATABLE :: errmsg

  ierr = 0

  isok = PRESENT(cellOrder)
  IF (.NOT. isok) THEN
    ierr = ierr - 1
    errmsg = "cellOrder is not present"
    CALL print_error
    RETURN
  END IF

  isok = PRESENT(cellOrient)
  IF (.NOT. isok) THEN
    ierr = ierr - 1
    errmsg = "cellOrient is not present"
    CALL print_error
    RETURN
  END IF

  isok = PRESENT(faceOrder)
  IF (.NOT. isok) THEN
    ierr = ierr - 1
    errmsg = "faceOrder is not present"
    CALL print_error
    RETURN
  END IF

  isok = SIZE(faceOrder, 2) .GE. tface
  IF (.NOT. isok) THEN
    ierr = ierr - 1
    errmsg = "colsize of faceOrder should be at least total face in elements"
    CALL print_error
    RETURN
  END IF

  isok = SIZE(faceOrder, 1) .GE. 3_I4B
  IF (.NOT. isok) THEN
    ierr = ierr - 1
    errmsg = "rowsize of faceOrder should be at least 3"
    CALL print_error
    RETURN
  END IF

  isok = PRESENT(faceOrient)
  IF (.NOT. isok) THEN
    ierr = ierr - 1
    errmsg = "faceOrient is not present"
    CALL print_error
    RETURN
  END IF

  isok = SIZE(faceOrient, 1) .GE. 3
  IF (.NOT. isok) THEN
    ierr = ierr - 1
    errmsg = "rowsize of faceOrient should be at least 3"
    CALL print_error
    RETURN
  END IF

  isok = SIZE(faceOrient, 2) .GE. tface
  IF (.NOT. isok) THEN
    ierr = ierr - 1
    errmsg = "colsize of faceOrient should be at least total face in elements"
    CALL print_error
    RETURN
  END IF

CONTAINS
  SUBROUTINE print_error
    CALL ErrorMsg(msg=errmsg, routine=routine, file=__FILE__, &
                  line=__LINE__, unitno=stderr)
  END SUBROUTINE print_error

END SUBROUTINE check_error_2d

END SUBMODULE Methods
