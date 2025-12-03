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

MODULE ForceVector_Method
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: ElemShapeData_, FEVariable_, FEVariableScalar_, &
                    FEVariableVector_, FEVariableMatrix_
IMPLICIT NONE
PRIVATE

PUBLIC :: ForceVector
PUBLIC :: ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 May 2022
! summary: Force vector
!
!# Introduction
!
! This subroutine computes the following expression:
!
! $$
! F_{I}=\int_{\Omega}N^{I}d\Omega
! $$

INTERFACE
  MODULE FUNCTION ForceVector1(test) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION ForceVector1
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector1
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 May 2022
! summary: Force vector
!
!# Introduction
!
! This subroutine computes the following expression:
!
! $$
! F_{I}=\int_{\Omega}N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_1(test, ans, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE ForceVector_1
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_1
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! $$
! F_{I}=\int_{\Omega}\rho N^{I}d\Omega
! $$

INTERFACE
  MODULE FUNCTION ForceVector2(test, c, crank) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableScalar_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION ForceVector2
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector2
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                                ForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! $$
! F_{I}=\int_{\Omega} c N^{I} d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_2(test, c, crank, ans, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c
    !! Scalar variables
    TYPE(FEVariableScalar_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE ForceVector_2
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_2
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! This routine computes the following integral
!
! $$
! F(i,I)=\int_{\Omega}c_{i}N^{I}d\Omega
! $$

INTERFACE
  MODULE FUNCTION ForceVector3(test, c, crank) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ForceVector3
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector3
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! This routine computes the following integral
!
! $$
! F(i,I)=\int_{\Omega}v_{i}N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_3(test, c, crank, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE ForceVector_3
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_3
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! This routine computes the following integral
!
! $$
! F(i,j,I)=\int_{\Omega}c_{ij}N^{I}d\Omega
! $$

INTERFACE
  MODULE FUNCTION ForceVector4(test, c, crank) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableMatrix_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION ForceVector4
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector4
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! This routine computes the following integral
!
! $$
! F(i,j,I)=\int_{\Omega}k_{ij}N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_4(test, c, crank, ans, dim1, dim2, dim3)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableMatrix_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE ForceVector_4
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_4
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! This routine computes the following integral
!
! $$
! F_{I}=\int_{\Omega}\rho_{1}\rho_{2}N^{I}d\Omega
! $$

INTERFACE
  MODULE FUNCTION ForceVector5(test, c1, c1rank, c2, c2rank) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION ForceVector5
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector5
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! This routine computes the following integral
!
! $$
! F_{I}=\int_{\Omega}c_{1}c_{2}N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_5(test, c1, c1rank, c2, c2rank, ans, &
                                  tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE ForceVector_5
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_5
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE FUNCTION ForceVector6(test, c1, c1rank, c2, c2rank) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ForceVector6
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector6
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector

INTERFACE
  MODULE SUBROUTINE ForceVector_6(test, c1, c1rank, c2, c2rank, ans, &
                                  nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE ForceVector_6
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_6
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! This routine computes the following.
!
! $$
! F(i,j,I)=\int_{\Omega}\rho k_{ij}N^{I}d\Omega
! $$

INTERFACE
  MODULE FUNCTION ForceVector7(test, c1, c1rank, c2, c2rank) &
    RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION ForceVector7
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector7
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! This routine computes the following.
!
! $$
! F(i,j,I)=\int_{\Omega}\rho k_{ij}N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_7(test, c1, c1rank, c2, c2rank, ans, &
                                  dim1, dim2, dim3)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE ForceVector_7
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_7
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! $$
! F_{I}=\int_{\Omega}\rho N^{I}d\Omega
! $$

INTERFACE
  MODULE FUNCTION ForceVector8(test, c) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    REAL(DFP), INTENT(IN) :: c(:)
    !! defined on quadrature point
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION ForceVector8
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector8
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! $$
! F_{I}=\int_{\Omega}\rho N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_8(test, c, ans, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    REAL(DFP), INTENT(IN) :: c(:)
    !! defined on quadrature point
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE ForceVector_8
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_8
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! $$
! F_{I}=\int_{\Omega}\rho N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_9( &
    N, js, ws, thickness, nns, nips, c, ans, tsize)
    REAL(DFP), INTENT(IN) :: N(:, :), js(:), ws(:), thickness(:)
    INTEGER(I4B), INTENT(IN) :: nns, nips
    REAL(DFP), INTENT(IN) :: c(:)
    !! defined on quadrature point
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE ForceVector_9
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_9
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! $$
! F_{I}=\int_{\Omega}\rho N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_10( &
    N, js, ws, thickness, nns, nips, c, skipVertices, tVertices, ans, tsize)
    REAL(DFP), INTENT(IN) :: N(:, :), js(:), ws(:), thickness(:)
    INTEGER(I4B), INTENT(IN) :: nns, nips
    REAL(DFP), INTENT(IN) :: c(:)
    !! defined on quadrature point
    LOGICAL(LGT), INTENT(IN) :: skipVertices
    INTEGER(I4B), INTENT(IN) :: tVertices
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE ForceVector_10
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_10
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! $$
! F_{I}=\int_{\Omega}\rho N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_11( &
    spaceN, timeN, js, ws, jt, wt, spaceThickness, timeThickness, nns, nnt, &
    nips, nipt, c, ans, tsize)
    REAL(DFP), INTENT(IN) :: spaceN(:, :), js(:), ws(:), spaceThickness(:)
    REAL(DFP), INTENT(IN) :: timeN(:, :), jt(:), wt(:), timeThickness(:)
    INTEGER(I4B), INTENT(IN) :: nns, nips, nnt, nipt
    REAL(DFP), INTENT(IN) :: c(:, :)
    !! defined on quadrature point
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Force vector is returned in DOF format
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE ForceVector_11
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_11
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! $$
! F_{I}=\int_{\Omega}\rho N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_12( &
    spaceN, timeN, js, ws, jt, wt, spaceThickness, timeThickness, nns, nnt, &
    nips, nipt, c, skipVertices, tSpaceVertices, tTimeVertices, ans, tsize)
    REAL(DFP), INTENT(IN) :: spaceN(:, :), js(:), ws(:), spaceThickness(:)
    REAL(DFP), INTENT(IN) :: timeN(:, :), jt(:), wt(:), timeThickness(:)
    INTEGER(I4B), INTENT(IN) :: nns, nips, nnt, nipt
    REAL(DFP), INTENT(IN) :: c(:, :)
    !! defined on quadrature point
    LOGICAL(LGT), INTENT(IN) :: skipVertices
    INTEGER(I4B), INTENT(IN) :: tSpaceVertices, tTimeVertices
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE ForceVector_12
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_12
END INTERFACE ForceVector_


!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! $$
! F_{I}=\int_{\Omega}\rho N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_13( &
    N, js, ws, thickness, nns, nips, ans, tsize)
    REAL(DFP), INTENT(IN) :: N(:, :), js(:), ws(:), thickness(:)
    INTEGER(I4B), INTENT(IN) :: nns, nips
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE ForceVector_13
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_13
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! $$
! F_{I}=\int_{\Omega}\rho N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_14( &
    N, js, ws, thickness, nns, nips, skipVertices, tVertices, ans, tsize)
    REAL(DFP), INTENT(IN) :: N(:, :), js(:), ws(:), thickness(:)
    INTEGER(I4B), INTENT(IN) :: nns, nips
    LOGICAL(LGT), INTENT(IN) :: skipVertices
    INTEGER(I4B), INTENT(IN) :: tVertices
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE ForceVector_14
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_14
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! $$
! F_{I}=\int_{\Omega}\rho N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_15( &
    spaceN, timeN, js, ws, jt, wt, spaceThickness, timeThickness, nns, nnt, &
    nips, nipt, ans, tsize)
    REAL(DFP), INTENT(IN) :: spaceN(:, :), js(:), ws(:), spaceThickness(:)
    REAL(DFP), INTENT(IN) :: timeN(:, :), jt(:), wt(:), timeThickness(:)
    INTEGER(I4B), INTENT(IN) :: nns, nips, nnt, nipt
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Force vector is returned in DOF format
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE ForceVector_15
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_15
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! $$
! F_{I}=\int_{\Omega}\rho N^{I}d\Omega
! $$

INTERFACE
  MODULE SUBROUTINE ForceVector_16( &
    spaceN, timeN, js, ws, jt, wt, spaceThickness, timeThickness, nns, nnt, &
    nips, nipt, skipVertices, tSpaceVertices, tTimeVertices, ans, tsize)
    REAL(DFP), INTENT(IN) :: spaceN(:, :), js(:), ws(:), spaceThickness(:)
    REAL(DFP), INTENT(IN) :: timeN(:, :), jt(:), wt(:), timeThickness(:)
    INTEGER(I4B), INTENT(IN) :: nns, nips, nnt, nipt
    LOGICAL(LGT), INTENT(IN) :: skipVertices
    INTEGER(I4B), INTENT(IN) :: tSpaceVertices, tTimeVertices
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE ForceVector_16
END INTERFACE

INTERFACE ForceVector_
  MODULE PROCEDURE ForceVector_16
END INTERFACE ForceVector_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ForceVector_Method
