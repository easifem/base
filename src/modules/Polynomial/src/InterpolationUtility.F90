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
! summary: Vandermonde matrices and element interpolation metadata.
!
! This module provides a one dimensional Vandermonde constructor and
! dispatches DOF counts and reference domain names to element modules.
!
! Arguments:
!   None at module scope; see the public interfaces.
!
! Returns:
!   Public polynomial and interpolation procedures.
!
! Formula:
! $$V_{i,j+1}=x_i^j$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE InterpolationUtility, ONLY: VandermondeMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), v(3, 3)
!
! x = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! v = VandermondeMatrix(order=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(v(:, 1)-1.0_DFP)) < 1.0E-12_DFP, &
!   "Constant column")
! CALL OK(MAXVAL(ABS(v(:, 3)-x*x)) < 1.0E-12_DFP, &
!   "Quadratic column")
! END PROGRAM main
!```

MODULE InterpolationUtility
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: REAL32
USE GlobalData, ONLY: REAL64
USE String_Class, ONLY: String

IMPLICIT NONE

PRIVATE

PUBLIC :: VandermondeMatrix
PUBLIC :: GetTotalInDOF
PUBLIC :: GetTotalDOF
PUBLIC :: RefElemDomain

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Build a one dimensional monomial Vandermonde matrix.
!
! Use nonnegative order. Rows correspond to points and columns to increasing
! powers.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   x : REAL(REAL32), INTENT(IN)(:). Evaluation coordinate(s).
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   x : REAL(REAL64), INTENT(IN)(:). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(REAL32)(SIZE(x), order + 1).
!
! Formula:
! $$V_{i,j+1}=x_i^j,\quad 0\le j\le\mathrm{order}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE InterpolationUtility, ONLY: VandermondeMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), v(3, 3)
!
! x = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! v = VandermondeMatrix(order=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(v(:, 1)-1.0_DFP)) < 1.0E-12_DFP, &
!   "Constant column")
! CALL OK(MAXVAL(ABS(v(:, 3)-x*x)) < 1.0E-12_DFP, &
!   "Quadratic column")
! END PROGRAM main
!```

INTERFACE VandermondeMatrix
  MODULE PURE FUNCTION VandermondeMatrix_Real32(order, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    REAL(REAL32), INTENT(IN) :: x(:)
    REAL(REAL32) :: ans(SIZE(x), order + 1)
  END FUNCTION VandermondeMatrix_Real32

  MODULE PURE FUNCTION VandermondeMatrix_Real64(order, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    REAL(REAL64), INTENT(IN) :: x(:)
    REAL(REAL64) :: ans(SIZE(x), order + 1)
  END FUNCTION VandermondeMatrix_Real64
END INTERFACE VandermondeMatrix

!----------------------------------------------------------------------------
!                                                             GetTotalDOF
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return total interpolation DOFs for an element.
!
! Dispatches by element topology. Supported continuity and interpolation names
! depend on the element specific implementation.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   baseContinuity : CHARACTER(LEN=*), INTENT(IN). Continuity family string,
! for example H1.
!   baseInterpolation : CHARACTER(LEN=*), INTENT(IN). Interpolation family
! string, for example Lagrange.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{line,int}}=\max(p-1,0)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE InterpolationUtility, ONLY: GetTotalDOF
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: ndof
!
! ndof = GetTotalDOF(elemType=Line, order=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange")
! CALL OK(ndof == 3, "Quadratic line degree count")
! END PROGRAM main
!```

INTERFACE GetTotalDOF
  MODULE PURE FUNCTION GetTotalDOF1(elemType, order, baseContinuity, &
                                    baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type, Point, Line, Triangle
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    CHARACTER(LEN=*), INTENT(IN) :: baseContinuity
    !! continuity of basis, H1, HDiv, HCurl
    CHARACTER(LEN=*), INTENT(IN) :: baseInterpolation
    !! interpolation of basis, Lagrange, Heirarchical
    INTEGER(I4B) :: ans
    !! total number of degrees of freedom
  END FUNCTION GetTotalDOF1
END INTERFACE GetTotalDOF

!----------------------------------------------------------------------------
!                                                             GetTotalInDOF
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return interior interpolation DOFs for an element.
!
! Dispatches by element topology. Supported continuity and interpolation names
! depend on the element specific implementation.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   baseContinuity : CHARACTER(LEN=*), INTENT(IN). Continuity family string,
! for example H1.
!   baseInterpolation : CHARACTER(LEN=*), INTENT(IN). Interpolation family
! string, for example Lagrange.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{line,int}}=\max(p-1,0)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE InterpolationUtility, ONLY: GetTotalInDOF
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: ndof
!
! ndof = GetTotalInDOF(elemType=Line, order=2_I4B, &
!   baseContinuity="H1", baseInterpolation="Lagrange")
! CALL OK(ndof == 1, "Quadratic line degree count")
! END PROGRAM main
!```

INTERFACE GetTotalInDOF
  MODULE PURE FUNCTION GetTotalInDOF1(elemType, order, baseContinuity, &
                                      baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type, Point, Line, Triangle
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    CHARACTER(LEN=*), INTENT(IN) :: baseContinuity
    !! continuity of basis, H1, HDiv, HCurl
    CHARACTER(LEN=*), INTENT(IN) :: baseInterpolation
    !! interpolation of basis, Lagrange, Heirarchical
    INTEGER(I4B) :: ans
    !! total number of degrees of freedom
  END FUNCTION GetTotalInDOF1
END INTERFACE GetTotalInDOF

!----------------------------------------------------------------------------
!                                                           RefElemDomain
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the reference domain name for an interpolation family.
!
! The result is a String object. The Line implementation currently returns
! BIUNIT.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   baseContinuity : CHARACTER(*), INTENT(IN). Continuity family string, for
! example H1.
!   baseInterpol : CHARACTER(*), INTENT(IN). Interpolation family string, for
! example Lagrange.
!
! Returns:
!   ans : TYPE(String).
!
! Formula:
! $$\widehat K_{\mathrm{line}}=[-1,1]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line
! USE InterpolationUtility, ONLY: RefElemDomain
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(String) :: domain
!
! domain = RefElemDomain(elemType=Line, baseContinuity="H1", &
!   baseInterpol="Lagrange")
! CALL OK(domain%chars() == "BIUNIT", "Line reference domain")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION RefElemDomain(elemType, baseContinuity, baseInterpol) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Cointinuity (conformity) of basis functions
    !! "H1", "HDiv", "HCurl", "DG"
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! Basis function family for Interpolation
    !! Lagrange, Hierarchy, Serendipity, Hermit, Orthogonal
    TYPE(String) :: ans
  END FUNCTION RefElemDomain
END INTERFACE

END MODULE InterpolationUtility
