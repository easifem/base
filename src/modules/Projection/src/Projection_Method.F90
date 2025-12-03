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

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-01
! summary: This module contains projection methods for getting DOF values
!          This module uses ElemshapeData, various matrix and forceVector
!          modules

MODULE Projection_Method
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: ElemShapeData_

IMPLICIT NONE

PRIVATE

PUBLIC :: GetL2ProjectionDOFValueFromQuadrature

!----------------------------------------------------------------------------
!                                                     GetL2ProjectionDOFValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-01
! summary: L2 Projection method to get DOF values

INTERFACE
  MODULE SUBROUTINE obj_GetL2ProjectionDOFValueFromQuadrature1( &
    elemsd, func, ans, tsize, massMat, ipiv, skipVertices, tVertices)
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! shape function defined on the face of element
    REAL(DFP), INTENT(INOUT) :: func(:)
    !! user defined functions
    !! quadrature values of function
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! nodal coordinates of interpolation points
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! data written in xij
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! mass matrix
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! pivot indices for LU decomposition of mass matrix
    LOGICAL(LGT), INTENT(IN) :: skipVertices
    !! if true then we include only face bubble, that is,
    !! only include internal face bubble.
    INTEGER(I4B), INTENT(IN) :: tVertices
    !! tVertices are needed when onlyFaceBubble is true
    !! tVertices are total number of vertex degree of
    !! freedom
  END SUBROUTINE obj_GetL2ProjectionDOFValueFromQuadrature1
END INTERFACE

INTERFACE GetL2ProjectionDOFValueFromQuadrature
  MODULE PROCEDURE obj_GetL2ProjectionDOFValueFromQuadrature1
END INTERFACE GetL2ProjectionDOFValueFromQuadrature

!----------------------------------------------------------------------------
!                                                     GetL2ProjectionDOFValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-01
! summary: L2 Projection method to get DOF values

INTERFACE
  MODULE SUBROUTINE obj_GetL2ProjectionDOFValueFromQuadrature2( &
    elemsd, timeElemsd, func, ans, tsize, massMat, ipiv, &
    skipVertices, tSpaceVertices, tTimeVertices)
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd, timeElemsd
    !! shape function defined on the face of space element
    !! timeElemsd is shape function data for time element
    REAL(DFP), INTENT(INOUT) :: func(:, :)
    !! user defined functions quadrature values of function
    !! Each column contains value at a given time quadrature points
    !! Each row contains value at a given space quadrature points
    !! Size should be atleast elemsd%nips by timeElemsd%nips
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! nodal coordinates of interpolation points
    !! These are in DOF Format
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! data written in ans
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! mass matrix, the size should be atleast nns * nnt by nns * nnt
    !! We will make space time mass matrix here
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! pivot indices for LU decomposition of mass matrix
    !! the size should be atleast nns * nnt
    LOGICAL(LGT), INTENT(IN) :: skipVertices
    !! if true then we include only face bubble, that is,
    !! only include internal face bubble.
    INTEGER(I4B), INTENT(IN) :: tSpaceVertices
    !! tSpaceVertices are needed when onlyFaceBubble is true
    !! tSpaceVertices are total number of vertex degree of
    !! freedom in space
    INTEGER(I4B), INTENT(IN) :: tTimeVertices
    !! tTimeVertices are needed when onlyFaceBubble is true
    !! tTimeVertices are total number of vertex degree of
    !! freedom in Time
  END SUBROUTINE obj_GetL2ProjectionDOFValueFromQuadrature2
END INTERFACE

INTERFACE GetL2ProjectionDOFValueFromQuadrature
  MODULE PROCEDURE obj_GetL2ProjectionDOFValueFromQuadrature2
END INTERFACE GetL2ProjectionDOFValueFromQuadrature

!----------------------------------------------------------------------------
!                                                     GetL2ProjectionDOFValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-01
! summary: L2 Projection of constant function

INTERFACE
  MODULE SUBROUTINE obj_GetL2ProjectionDOFValueFromQuadrature3( &
    elemsd, ans, tsize, massMat, ipiv, skipVertices, tVertices)
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! shape function defined on the face of element
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! nodal coordinates of interpolation points
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! data written in xij
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! mass matrix
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! pivot indices for LU decomposition of mass matrix
    LOGICAL(LGT), INTENT(IN) :: skipVertices
    !! if true then we include only face bubble, that is,
    !! only include internal face bubble.
    INTEGER(I4B), INTENT(IN) :: tVertices
    !! tVertices are needed when onlyFaceBubble is true
    !! tVertices are total number of vertex degree of
    !! freedom
  END SUBROUTINE obj_GetL2ProjectionDOFValueFromQuadrature3
END INTERFACE

INTERFACE GetL2ProjectionDOFValueFromQuadrature
  MODULE PROCEDURE obj_GetL2ProjectionDOFValueFromQuadrature3
END INTERFACE GetL2ProjectionDOFValueFromQuadrature

!----------------------------------------------------------------------------
!                                                     GetL2ProjectionDOFValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-01
! summary: L2 Projection method to get DOF values

INTERFACE
  MODULE SUBROUTINE obj_GetL2ProjectionDOFValueFromQuadrature4( &
    elemsd, timeElemsd, ans, tsize, massMat, ipiv, &
    skipVertices, tSpaceVertices, tTimeVertices)
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd, timeElemsd
    !! shape function defined on the face of space element
    !! timeElemsd is shape function data for time element
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! nodal coordinates of interpolation points
    !! These are in DOF Format
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! data written in ans
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! mass matrix, the size should be atleast nns * nnt by nns * nnt
    !! We will make space time mass matrix here
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! pivot indices for LU decomposition of mass matrix
    !! the size should be atleast nns * nnt
    LOGICAL(LGT), INTENT(IN) :: skipVertices
    !! if true then we include only face bubble, that is,
    !! only include internal face bubble.
    INTEGER(I4B), INTENT(IN) :: tSpaceVertices
    !! tSpaceVertices are needed when onlyFaceBubble is true
    !! tSpaceVertices are total number of vertex degree of
    !! freedom in space
    INTEGER(I4B), INTENT(IN) :: tTimeVertices
    !! tTimeVertices are needed when onlyFaceBubble is true
    !! tTimeVertices are total number of vertex degree of
    !! freedom in Time
  END SUBROUTINE obj_GetL2ProjectionDOFValueFromQuadrature4
END INTERFACE

INTERFACE GetL2ProjectionDOFValueFromQuadrature
  MODULE PROCEDURE obj_GetL2ProjectionDOFValueFromQuadrature4
END INTERFACE GetL2ProjectionDOFValueFromQuadrature

END MODULE Projection_Method
