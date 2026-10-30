! This program is a part of EASIFEM library
! Copyright (C) 2020-2021 Vikas Sharma, Ph.D
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
! along with this program.  If not, see <https://www.gnu.org/licenses/>

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Hierarchical entity DOF counts and oriented basis evaluation.
!
! Order and orientation arrays are entity dependent. Basis evaluation
! currently supports Line, Triangle and Quadrangle. DOF helpers cover
! additional topologies, which does not imply corresponding evaluation
! support.
!
! Arguments:
!   None at module scope; see the public interfaces.
!
! Returns:
!   Public polynomial and interpolation procedures.
!
! Formula:
! $$N=N_v+N_e+N_f+N_c$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE HierarchicalPolynomialUtility, ONLY: HierarchicalEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1, 3)
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! x(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! y = HierarchicalEvalAll(elemType=Line, xij=x, &
!   domainName="BIUNIT", cellOrder=[1_I4B], cellOrient=[1_I4B], &
!   faceOrder=RESHAPE([1_I4B], [1, 1]), edgeOrder=[1_I4B], &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), edgeOrient=[1_I4B])
! CALL OK(MAXVAL(ABS(SUM(y, DIM=2)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Linear endpoint modes")
! END PROGRAM main
!```

MODULE HierarchicalPolynomialUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Count hierarchical total degrees of freedom.
!
! CellOrder supplies directional interior orders. For 2D elements faceOrder
! describes boundary edges; edgeOrder is used for 3D elements. Only supplied
! entity order arrays contribute to HierarchicalDOF. Vertex counts use the
! nodes of the supplied element identifier.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   cellOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Directional cell
! order(s).
!   faceOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:, :). Orders of boundary
! faces; in 2D these are boundary edges.
!   edgeOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Orders of edges in 3D.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N=N_v+N_e+N_f+N_c$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE HierarchicalPolynomialUtility, ONLY: HierarchicalDOF
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: ndof
!
! ndof = HierarchicalDOF(elemType=Line, cellOrder=[3_I4B])
! CALL OK(ndof == 4, "Hierarchical entity degree count")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Count hierarchical vertex degrees of freedom.
!
! CellOrder supplies directional interior orders. For 2D elements faceOrder
! describes boundary edges; edgeOrder is used for 3D elements. Only supplied
! entity order arrays contribute to HierarchicalDOF. Vertex counts use the
! nodes of the supplied element identifier.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N=N_v+N_e+N_f+N_c$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE HierarchicalPolynomialUtility, ONLY: HierarchicalVertexDOF
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: ndof
!
! ndof = HierarchicalVertexDOF(elemType=Line)
! CALL OK(ndof == 2, "Hierarchical entity degree count")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Count hierarchical edge interior degrees of freedom.
!
! CellOrder supplies directional interior orders. For 2D elements faceOrder
! describes boundary edges; edgeOrder is used for 3D elements. Only supplied
! entity order arrays contribute to HierarchicalDOF. Vertex counts use the
! nodes of the supplied element identifier.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN)(:). Interpolation degree; use a
! nonnegative value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N=N_v+N_e+N_f+N_c$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Tetrahedron
! USE HierarchicalPolynomialUtility, ONLY: HierarchicalEdgeDOF
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: ndof
!
! ndof = HierarchicalEdgeDOF(elemType=Tetrahedron, &
!   order=[2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B])
! CALL OK(ndof == 6, "Hierarchical entity degree count")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Count hierarchical face interior degrees of freedom.
!
! CellOrder supplies directional interior orders. For 2D elements faceOrder
! describes boundary edges; edgeOrder is used for 3D elements. Only supplied
! entity order arrays contribute to HierarchicalDOF. Vertex counts use the
! nodes of the supplied element identifier.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN)(:, :). Interpolation degree; use a
! nonnegative value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N=N_v+N_e+N_f+N_c$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Triangle
! USE HierarchicalPolynomialUtility, ONLY: HierarchicalFaceDOF
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: ndof
!
! ndof = HierarchicalFaceDOF(elemType=Triangle, &
!   order=RESHAPE([2_I4B, 2_I4B, 2_I4B], [1, 3]))
! CALL OK(ndof == 3, "Hierarchical entity degree count")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Count hierarchical cell interior degrees of freedom.
!
! CellOrder supplies directional interior orders. For 2D elements faceOrder
! describes boundary edges; edgeOrder is used for 3D elements. Only supplied
! entity order arrays contribute to HierarchicalDOF. Vertex counts use the
! nodes of the supplied element identifier.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN)(:). Interpolation degree; use a
! nonnegative value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N=N_v+N_e+N_f+N_c$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE HierarchicalPolynomialUtility, ONLY: HierarchicalCellDOF
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: ndof
!
! ndof = HierarchicalCellDOF(elemType=Line, order=[3_I4B])
! CALL OK(ndof == 2, "Hierarchical entity degree count")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate hierarchical element basis functions.
!
! Currently dispatches to Line, Triangle and Quadrangle only. Points are
! columns of xij. Results use point, basis, then coordinate axes. Function
! forms require all three orientation arguments even for a line; unused arrays
! may be dummy arrays. Relevant order and orientation arrays must be present
! in subroutine forms too.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   cellOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Directional cell
! order(s).
!   faceOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:, :). Orders of boundary
! faces; in 2D these are boundary edges.
!   edgeOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Orders of edges in 3D.
!   edgeOrient : INTEGER(I4B), INTENT(IN)(:). Edge orientation code(s).
!   faceOrient : INTEGER(I4B), INTENT(IN)(:, :). Boundary face orientation
! code(s).
!   cellOrient : INTEGER(I4B), INTENT(IN)(:). Cell orientation code(s).
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$u(x)=\sum_a u_a\phi_a(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE HierarchicalPolynomialUtility, ONLY: HierarchicalEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1, 3)
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! x(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! y = HierarchicalEvalAll(elemType=Line, xij=x, &
!   domainName="BIUNIT", cellOrder=[1_I4B], cellOrient=[1_I4B], &
!   faceOrder=RESHAPE([1_I4B], [1, 1]), edgeOrder=[1_I4B], &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), edgeOrient=[1_I4B])
! CALL OK(MAXVAL(ABS(SUM(y, DIM=2)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Linear endpoint modes")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION HierarchicalEvalAll(elemType, xij, domainName, &
                    cellOrder, faceOrder, edgeOrder, cellOrient, faceOrient, &
                                      edgeOrient) RESULT(ans)
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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate hierarchical element basis functions.
!
! Currently dispatches to Line, Triangle and Quadrangle only. Points are
! columns of xij. Results use point, basis, then coordinate axes. Function
! forms require all three orientation arguments even for a line; unused arrays
! may be dummy arrays. Relevant order and orientation arrays must be present
! in subroutine forms too.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   cellOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Directional cell
! order(s).
!   faceOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:, :). Orders of boundary
! faces; in 2D these are boundary edges.
!   edgeOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Orders of edges in 3D.
!   cellOrient : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Cell orientation
! code(s).
!   faceOrient : INTEGER(I4B), OPTIONAL, INTENT(IN)(:, :). Boundary face
! orientation code(s).
!   edgeOrient : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Edge orientation
! code(s).
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$u(x)=\sum_a u_a\phi_a(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE HierarchicalPolynomialUtility, ONLY: HierarchicalEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1, 3)
! REAL(DFP) :: y(3, 2)
! INTEGER(I4B) :: nr, nc
!
! x(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! CALL HierarchicalEvalAll_(elemType=Line, xij=x, &
!   domainName="BIUNIT", cellOrder=[1_I4B], cellOrient=[1_I4B], &
!   faceOrder=RESHAPE([1_I4B], [1, 1]), edgeOrder=[1_I4B], &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), edgeOrient=[1_I4B], &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(SUM(y, DIM=2)-1.0_DFP)) &
!   < 1.0E-11_DFP, "Linear endpoint modes")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE HierarchicalEvalAll_(elemType, xij, ans, nrow, &
                          ncol, domainName, cellOrder, faceOrder, edgeOrder, &
                                         cellOrient, faceOrient, edgeOrient)
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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate gradients of hierarchical element basis functions.
!
! Currently dispatches to Line, Triangle and Quadrangle only. Points are
! columns of xij. Results use point, basis, then coordinate axes. Function
! forms require all three orientation arguments even for a line; unused arrays
! may be dummy arrays. Relevant order and orientation arrays must be present
! in subroutine forms too.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   cellOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Directional cell
! order(s).
!   faceOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:, :). Orders of boundary
! faces; in 2D these are boundary edges.
!   edgeOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Orders of edges in 3D.
!   edgeOrient : INTEGER(I4B), INTENT(IN)(:). Edge orientation code(s).
!   faceOrient : INTEGER(I4B), INTENT(IN)(:, :). Boundary face orientation
! code(s).
!   cellOrient : INTEGER(I4B), INTENT(IN)(:). Cell orientation code(s).
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :, :).
!
! Formula:
! $$\nabla u(x)=\sum_a u_a\nabla\phi_a(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE HierarchicalPolynomialUtility, ONLY: HierarchicalGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1, 3)
! REAL(DFP), ALLOCATABLE :: y(:, :, :)
!
! x(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! y = HierarchicalGradientEvalAll(elemType=Line, xij=x, &
!   domainName="BIUNIT", cellOrder=[1_I4B], cellOrient=[1_I4B], &
!   faceOrder=RESHAPE([1_I4B], [1, 1]), edgeOrder=[1_I4B], &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), edgeOrient=[1_I4B])
! CALL OK(MAXVAL(ABS(y(:, 1, 1)+0.5_DFP)) &
!   < 1.0E-11_DFP, "Linear endpoint modes")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION HierarchicalGradientEvalAll(elemType, xij, &
        domainName, cellOrder, faceOrder, edgeOrder, cellOrient, faceOrient, &
                                              edgeOrient) RESULT(ans)
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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate gradients of hierarchical element basis functions.
!
! Currently dispatches to Line, Triangle and Quadrangle only. Points are
! columns of xij. Results use point, basis, then coordinate axes. Function
! forms require all three orientation arguments even for a line; unused arrays
! may be dummy arrays. Relevant order and orientation arrays must be present
! in subroutine forms too.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   ans : REAL(DFP), INTENT(INOUT)(:, :, :). Result storage; allocate enough
! space for all active dimensions.
!   dim1 : INTEGER(I4B), INTENT(OUT). Active first dimension (evaluation
! points).
!   dim2 : INTEGER(I4B), INTENT(OUT). Active second dimension (basis
! functions).
!   dim3 : INTEGER(I4B), INTENT(OUT). Active third dimension (coordinate
! directions).
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   cellOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Directional cell
! order(s).
!   faceOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:, :). Orders of boundary
! faces; in 2D these are boundary edges.
!   edgeOrder : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Orders of edges in 3D.
!   cellOrient : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Cell orientation
! code(s).
!   faceOrient : INTEGER(I4B), OPTIONAL, INTENT(IN)(:, :). Boundary face
! orientation code(s).
!   edgeOrient : INTEGER(I4B), OPTIONAL, INTENT(IN)(:). Edge orientation
! code(s).
!
! Returns:
!   Writes ans(:, :, :), dim1, dim2, dim3.
!
! Formula:
! $$\nabla u(x)=\sum_a u_a\nabla\phi_a(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE HierarchicalPolynomialUtility, ONLY: HierarchicalGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(1, 3)
! REAL(DFP) :: y(3, 2, 1)
! INTEGER(I4B) :: nr, nc, nd
!
! x(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! CALL HierarchicalGradientEvalAll_(elemType=Line, xij=x, &
!   domainName="BIUNIT", cellOrder=[1_I4B], cellOrient=[1_I4B], &
!   faceOrder=RESHAPE([1_I4B], [1, 1]), edgeOrder=[1_I4B], &
!   faceOrient=RESHAPE([1_I4B], [1, 1]), edgeOrient=[1_I4B], &
!   ans=y, dim1=nr, dim2=nc, dim3=nd)
! CALL OK(MAXVAL(ABS(y(:, 1, 1)+0.5_DFP)) &
!   < 1.0E-11_DFP, "Linear endpoint modes")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE HierarchicalGradientEvalAll_(elemType, xij, ans, &
              dim1, dim2, dim3, domainName, cellOrder, faceOrder, edgeOrder, &
                                           cellOrient, faceOrient, edgeOrient)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradient of polynomials at quadrature points
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! dim1 = SIZE(xij, 2)
    !! dim2 = SIZE(xij, 2)
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
