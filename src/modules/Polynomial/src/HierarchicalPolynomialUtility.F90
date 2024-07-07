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

MODULE HierarchicalPolynomialUtility
USE GlobalData, ONLY: DFP, I4B, LGT

IMPLICIT NONE
PRIVATE

PUBLIC :: HierarchicalDOF
PUBLIC :: HierarchicalVertexDOF
PUBLIC :: HierarchicalEdgeDOF
PUBLIC :: HierarchicalFaceDOF
PUBLIC :: HierarchicalCellDOF

PUBLIC :: HierarchicalEvalAll_
PUBLIC :: HierarchicalEvalAll

PUBLIC :: HierarchicalGradientEvalAll_
PUBLIC :: HierarchicalGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-03
! summary:  Returns the total number of degree of freedom

INTERFACE
  MODULE PURE FUNCTION HierarchicalDOF(elemType, cellOrder, faceOrder, &
                                       edgeOrder) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order, alkways needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order, needed for 2D and 3D elements
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order needed for 1D elements
    INTEGER(I4B) :: ans
    !! number of degree of freedom
  END FUNCTION HierarchicalDOF
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-03
! summary:  Returns the total number of degree of freedom

INTERFACE
  MODULE PURE FUNCTION HierarchicalVertexDOF(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
    !! number of degree of freedom
  END FUNCTION HierarchicalVertexDOF
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION HierarchicalEdgeDOF(order, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! order,
    !! the size of order should be same as
    !! the total number of edges in element
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
    !! number of degree of freedom
  END FUNCTION HierarchicalEdgeDOF
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION HierarchicalFaceDOF(order, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order(:, :)
    !! order
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
    !! number of degree of freedom
  END FUNCTION HierarchicalFaceDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 j
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION HierarchicalCellDOF(order, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order(:)
    !! order
    !! for quadrangle element, size of order should be 2
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B) :: ans
    !! number of degree of freedom
  END FUNCTION HierarchicalCellDOF
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION HierarchicalEvalAll(order, elemType, xij, domainName, &
                    cellOrder, faceOrder, edgeOrder, cellOrient, faceOrient, &
                                      edgeOrient) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Hierarchical polynomials
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT ! BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order, always needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order, needed for 2D and 3D elements
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order, needed for 3D elements only
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Value of n+1 Hierarchical polynomials at point x
    INTEGER(I4B), INTENT(IN) :: edgeOrient(:)
    !! edge orientation
    INTEGER(I4B), INTENT(IN) :: faceOrient(:, :)
    !! face orientation
    INTEGER(I4B), INTENT(IN) :: cellOrient(:)
    !! cell orientation
  END FUNCTION HierarchicalEvalAll
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE HierarchicalEvalAll_(order, elemType, xij, ans, nrow, &
                          ncol, domainName, cellOrder, faceOrder, edgeOrder, &
                                         cellOrient, faceOrient, edgeOrient)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Hierarchical polynomials
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Value of n+1 Hierarchical polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(x, 2)
    !! ncol = SIZE(xij, 2)
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT ! BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order, always needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order, needed for 2D and 3D elements
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order, needed for 3D elements only
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! orientation of cell
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! orientation of face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! edge orientation
  END SUBROUTINE HierarchicalEvalAll_
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION HierarchicalGradientEvalAll(order, elemType, xij, &
        domainName, cellOrder, faceOrder, edgeOrder, cellOrient, faceOrient, &
                                              edgeOrient) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Hierarchical polynomials
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT ! BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order, always needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order, needed for 2D and 3D elements
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order, needed for 3D elements only
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
    !! Value of n+1 Hierarchical polynomials at point x
    INTEGER(I4B), INTENT(IN) :: edgeOrient(:)
    !! edge orientation
    INTEGER(I4B), INTENT(IN) :: faceOrient(:, :)
    !! face orientation
    INTEGER(I4B), INTENT(IN) :: cellOrient(:)
    !! cell orientation
  END FUNCTION HierarchicalGradientEvalAll
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE HierarchicalGradientEvalAll_(order, elemType, xij, ans, &
              dim1, dim2, dim3, domainName, cellOrder, faceOrder, edgeOrder, &
                                           cellOrient, faceOrient, edgeOrient)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Hierarchical polynomials
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! Value of n+1 Hierarchical polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! nrow = SIZE(x, 2)
    !! ncol = SIZE(xij, 2)
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT ! BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrder(:)
    !! cell order, always needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
    !! face order, needed for 2D and 3D elements
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
    !! edge order, needed for 3D elements only
    !! cell order, always needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! orientation of cell
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! orientation of face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! edge orientation
  END SUBROUTINE HierarchicalGradientEvalAll_
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE HierarchicalPolynomialUtility
