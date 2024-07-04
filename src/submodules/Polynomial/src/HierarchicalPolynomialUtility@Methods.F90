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
                                   ElementTopology

USE ErrorHandling, ONLY: ErrorMsg

USE BaseType, ONLY: elemopt => TypeElemNameOpt

USE LineInterpolationUtility, ONLY: HeirarchicalBasis_Line_, &
                                    HeirarchicalBasisGradient_Line_

USE TriangleInterpolationUtility, ONLY: HeirarchicalBasis_Triangle_, &
                                        HeirarchicalBasisGradient_Triangle_

USE QuadrangleInterpolationUtility, ONLY: HeirarchicalBasis_Quadrangle_, &
                                         HeirarchicalBasisGradient_Quadrangle_

USE TetrahedronInterpolationUtility, ONLY: HeirarchicalBasis_Tetrahedron_, &
                                        HeirarchicalBasisGradient_Tetrahedron_

USE HexahedronInterpolationUtility, ONLY: HeirarchicalBasis_Hexahedron_, &
                                         HeirarchicalBasisGradient_Hexahedron_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalDOF
ans = 0
END PROCEDURE HierarchicalDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexDOF
ans = GetTotalNodes(elemType)
END PROCEDURE VertexDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeDOF
ans = 0
END PROCEDURE EdgeDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FaceDOF
ans = 0
END PROCEDURE FaceDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE CellDOF
ans = 0
END PROCEDURE CellDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalEvalAll
INTEGER(I4B) :: nrow, ncol

nrow = SIZE(xij, 2)
ncol = HierarchicalDOF(elemType=elemType, cellOrder=cellOrder, &
                       faceOrder=faceOrder, edgeOrder=edgeOrder)

ALLOCATE (ans(nrow, ncol))

CALL HierarchicalEvalAll_(order=order, elemType=elemType, xij=xij, ans=ans, &
           nrow=nrow, ncol=ncol, domainName=domainName, cellOrder=cellOrder, &
                          faceOrder=faceOrder, edgeOrder=edgeOrder)

END PROCEDURE HierarchicalEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalEvalAll_
#ifdef DEBUG_VER
INTEGER(I4B) :: ierr, tedge, tface, nsd
LOGICAL(LGT) :: isok
CHARACTER(:), ALLOCATABLE :: errmsg
#endif

INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)

CASE (elemopt%Line)

#ifdef DEBUG_VER
  nsd = 1
  CALL check_error
  IF (ierr .LT. 0) THEN
    CALL printError
    RETURN
  END IF
#endif

  CALL HeirarchicalBasis_Line_(order=cellOrder(1), xij=xij, ans=ans, &
                               nrow=nrow, ncol=ncol, refLine=domainName)

CASE (elemopt%Triangle)

#ifdef DEBUG_VER
  nsd = 2; tFace = 3
  CALL check_error
  IF (ierr .LT. 0) THEN
    CALL printError
    RETURN
  END IF
#endif

  CALL HeirarchicalBasis_Triangle_(order=cellOrder(1), pe1=faceOrder(1, 1), &
  pe2=faceOrder(1, 2), pe3=faceOrder(1, 3), xij=xij, refTriangle=domainName, &
                                   ans=ans, nrow=nrow, ncol=ncol)

CASE (elemopt%Quadrangle)

#ifdef DEBUG_VER
  nsd = 2; tFace = 4
  CALL check_error
  IF (ierr .LT. 0) THEN
    CALL printError
    RETURN
  END IF
#endif

  CALL HeirarchicalBasis_Quadrangle_(pb=cellOrder(1), qb=cellOrder(2), &
              pe3=faceOrder(1, 1), pe4=faceOrder(1, 3), qe1=faceOrder(1, 4), &
                  qe2=faceOrder(1, 2), xij=xij, ans=ans, nrow=nrow, ncol=ncol)

CASE (elemopt%Tetrahedron)

#ifdef DEBUG_VER
  nsd = 3; tFace = 4; tEdge = 6
  CALL check_error
  IF (ierr .LT. 0) THEN
    CALL printError
    RETURN
  END IF
#endif

  CALL HeirarchicalBasis_Tetrahedron_(order=cellOrder(1), pe1=edgeOrder(1), &
     pe2=edgeOrder(2), pe3=edgeOrder(3), pe4=edgeOrder(4), pe5=edgeOrder(5), &
                 pe6=edgeOrder(6), ps1=faceOrder(1, 1), ps2=faceOrder(1, 2), &
                          ps3=faceOrder(1, 3), ps4=faceOrder(1, 4), xij=xij, &
                                      refTetrahedron=domainName, ans=ans, &
                                      nrow=nrow, ncol=ncol)

CASE (elemopt%Hexahedron)

#ifdef DEBUG_VER
  !! FIXME: Currently we consiering  only three faces
  nsd = 3; tFace = 3; tEdge = 12
  CALL check_error
  IF (ierr .LT. 0) THEN
    CALL printError
    RETURN
  END IF
#endif

  CALL HeirarchicalBasis_Hexahedron_( &
    pb1=cellOrder(1), pb2=cellOrder(2), pb3=cellOrder(3), &
    pxy1=faceOrder(1, 1), pxy2=faceOrder(2, 1), &
    pxz1=faceOrder(1, 2), pxz2=faceOrder(2, 2), &
    pyz1=faceOrder(1, 3), pyz2=faceOrder(2, 3), &
    px1=edgeOrder(1), px2=edgeOrder(2), px3=edgeOrder(3), px4=edgeOrder(4), &
    py1=edgeOrder(5), py2=edgeOrder(6), py3=edgeOrder(7), py4=edgeOrder(8), &
    pz1=edgeOrder(9), pz2=edgeOrder(10), pz3=edgeOrder(11), &
    pz4=edgeOrder(12), xij=xij, ans=ans, nrow=nrow, ncol=ncol)

! CASE (elemopt%Prism)

! CASE (elemopt%Pyramid)

CASE DEFAULT
  CALL ErrorMsg(msg="No case found for topology", &
                routine='HierarchicalEvalAll_()', &
                file=__FILE__, line=__LINE__, unitno=stderr)

  RETURN
END SELECT

#ifdef DEBUG_VER

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

SUBROUTINE check_error

  isok = PRESENT(cellOrder)
  IF (.NOT. isok) THEN
    ierr = -1
    errmsg = "cellOrder is not present"
    RETURN
  END IF

  IF (nsd .GT. 1) THEN

    isok = PRESENT(faceOrder)
    IF (.NOT. isok) THEN
      ierr = -2
      errmsg = "faceOrder is not present"
      RETURN
    END IF

    isok = SIZE(faceOrder, 2) .EQ. tface
    IF (.NOT. isok) THEN
      ierr = -3
      errmsg = "the size of faceOrder should be total face in elements"
      RETURN
    END IF

  END IF

  IF (nsd .EQ. 2) THEN

    isok = PRESENT(edgeOrder)
    IF (.NOT. isok) THEN
      ierr = -4
      errmsg = "edgeOrder is not present"
      RETURN
    END IF

    isok = SIZE(edgeOrder) .EQ. tEdge
    IF (.NOT. isok) THEN
      ierr = -5
      errmsg = "the size of faceOrder should be total face in elements"
      RETURN
    END IF

  END IF

END SUBROUTINE check_error

SUBROUTINE printError
  CALL ErrorMsg(msg=errmsg, routine='HierarchicalEvalAll_()', &
                file=__FILE__, line=__LINE__, unitno=stderr)
END SUBROUTINE printError

#endif

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

CALL HierarchicalGradientEvalAll_(order=order, elemType=elemType, xij=xij, &
            ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, domainName=domainName, &
                cellOrder=cellOrder, faceOrder=faceOrder, edgeOrder=edgeOrder)

END PROCEDURE HierarchicalGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE HierarchicalGradientEvalAll_

#ifdef DEBUG_VER
INTEGER(I4B) :: ierr, tedge, tface, nsd
LOGICAL(LGT) :: isok
CHARACTER(:), ALLOCATABLE :: errmsg
#endif

INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)

CASE (elemopt%Line)

#ifdef DEBUG_VER
  nsd = 1
  CALL check_error
  IF (ierr .LT. 0) THEN
    CALL printError
    RETURN
  END IF
#endif

  CALL HeirarchicalBasisGradient_Line_(order=cellOrder(1), xij=xij, ans=ans, &
                          dim1=dim1, dim2=dim2, dim3=dim3, refLine=domainName)

CASE (elemopt%Triangle)

#ifdef DEBUG_VER
  nsd = 2; tFace = 3
  CALL check_error
  IF (ierr .LT. 0) THEN
    CALL printError
    RETURN
  END IF
#endif

  CALL HeirarchicalBasisGradient_Triangle_(order=cellOrder(1), &
     pe1=faceOrder(1, 1), pe2=faceOrder(1, 2), pe3=faceOrder(1, 3), xij=xij, &
       refTriangle=domainName, ans=ans, tsize1=dim1, tsize2=dim2, tsize3=dim3)

CASE (elemopt%Quadrangle)

#ifdef DEBUG_VER
  nsd = 2; tFace = 4
  CALL check_error
  IF (ierr .LT. 0) THEN
    CALL printError
    RETURN
  END IF
#endif

CALL HeirarchicalBasisGradient_Quadrangle_(pb=cellOrder(1), qb=cellOrder(2), &
              pe3=faceOrder(1, 1), pe4=faceOrder(1, 3), qe1=faceOrder(1, 4), &
       qe2=faceOrder(1, 2), xij=xij, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

CASE (elemopt%Tetrahedron)

#ifdef DEBUG_VER
  nsd = 3; tFace = 4; tEdge = 6
  CALL check_error
  IF (ierr .LT. 0) THEN
    CALL printError
    RETURN
  END IF
#endif

  CALL HeirarchicalBasisGradient_Tetrahedron_(order=cellOrder(1), &
                       pe1=edgeOrder(1), pe2=edgeOrder(2), pe3=edgeOrder(3), &
                       pe4=edgeOrder(4), pe5=edgeOrder(5), pe6=edgeOrder(6), &
              ps1=faceOrder(1, 1), ps2=faceOrder(1, 2), ps3=faceOrder(1, 3), &
                    ps4=faceOrder(1, 4), xij=xij, refTetrahedron=domainName, &
                                     ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

CASE (elemopt%Hexahedron)

#ifdef DEBUG_VER
  !! FIXME: Currently we consiering  only three faces
  nsd = 3; tFace = 3; tEdge = 12
  CALL check_error
  IF (ierr .LT. 0) THEN
    CALL printError
    RETURN
  END IF
#endif

  CALL HeirarchicalBasisGradient_Hexahedron_( &
    pb1=cellOrder(1), pb2=cellOrder(2), pb3=cellOrder(3), &
    pxy1=faceOrder(1, 1), pxy2=faceOrder(2, 1), &
    pxz1=faceOrder(1, 2), pxz2=faceOrder(2, 2), &
    pyz1=faceOrder(1, 3), pyz2=faceOrder(2, 3), &
    px1=edgeOrder(1), px2=edgeOrder(2), px3=edgeOrder(3), px4=edgeOrder(4), &
    py1=edgeOrder(5), py2=edgeOrder(6), py3=edgeOrder(7), py4=edgeOrder(8), &
    pz1=edgeOrder(9), pz2=edgeOrder(10), pz3=edgeOrder(11), &
    pz4=edgeOrder(12), xij=xij, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

! CASE (elemopt%Prism)

! CASE (elemopt%Pyramid)

CASE DEFAULT
  CALL ErrorMsg(msg="No case found for topology", &
                routine='HierarchicalEvalAll_()', &
                file=__FILE__, line=__LINE__, unitno=stderr)

  RETURN
END SELECT

#ifdef DEBUG_VER

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

SUBROUTINE check_error

  isok = PRESENT(cellOrder)
  IF (.NOT. isok) THEN
    ierr = -1
    errmsg = "cellOrder is not present"
    RETURN
  END IF

  IF (nsd .GT. 1) THEN

    isok = PRESENT(faceOrder)
    IF (.NOT. isok) THEN
      ierr = -2
      errmsg = "faceOrder is not present"
      RETURN
    END IF

    isok = SIZE(faceOrder, 2) .EQ. tface
    IF (.NOT. isok) THEN
      ierr = -3
      errmsg = "the size of faceOrder should be total face in elements"
      RETURN
    END IF

  END IF

  IF (nsd .EQ. 2) THEN

    isok = PRESENT(edgeOrder)
    IF (.NOT. isok) THEN
      ierr = -4
      errmsg = "edgeOrder is not present"
      RETURN
    END IF

    isok = SIZE(edgeOrder) .EQ. tEdge
    IF (.NOT. isok) THEN
      ierr = -5
      errmsg = "the size of faceOrder should be total face in elements"
      RETURN
    END IF

  END IF

END SUBROUTINE check_error

SUBROUTINE printError
  CALL ErrorMsg(msg=errmsg, routine='HierarchicalEvalAll_()', &
                file=__FILE__, line=__LINE__, unitno=stderr)
END SUBROUTINE printError

#endif

END PROCEDURE HierarchicalGradientEvalAll_

END SUBMODULE Methods
