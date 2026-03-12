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

MODULE STForceVector_Method
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: FEVariableScalar_
USE BaseType, ONLY: FEVariableVector_
USE BaseType, ONLY: FEVariableMatrix_
USE GlobalData, ONLY: I4B, DFP, LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: STForceVector
PUBLIC :: STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Force vector
!
!# STForceVector
!
!This method computes space time force vector.
!
!$$
!F(I,a)=\int \int N^{I} T_{a} ds dt
!$$
!
INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector1(test) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector1
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space time force vector without allocation
!
!# STForceVector_
!
!This method computes space time force vector.
!
!$$
!F(I,a)=\int \int N^{I} T_{a} ds dt
!$$
!

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_1(test, ans, nrow, ncol, &
                                             scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_1
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Space time force vector
!
!# STForceVector
!
! Space time force vector. In this case `c` is scalar FEVariable.
!
!$$
!F(I,a)=\int \int N^{I} T_{a} \rho ds dt
!$$
!
! Here $\rho$ is denoted by $c$.

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector2(test, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    !! space time element shape data vector
    TYPE(FEVariable_), INTENT(IN) :: c
    !! scalar finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: crank
    !! scalar
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! space time force vector
  END FUNCTION obj_STForceVector2
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Space time force vector
!
!# STForceVector_
!
! In this method following space time force vector is computed.
! In this case `c` is scalar FEVariable.
!
!$$
!F(I,a)=\int \int N^{I} T_{a} \rho ds dt
!$$
!
! Here $\rho$ is denoted by $c$.

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_2( &
    test, c, crank, ans, nrow, ncol, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    !! vector of space time element shape data
    TYPE(FEVariable_), INTENT(IN) :: c
    !! scalar finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: crank
    !! scalar finite element variable
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space time force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols with ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_2
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Space time force vector
!
!# STForceVector
!
! In this method following space time force vector is computed.
! In this case `c` is vector FEVariable.
! The result denotes vector force vector.
!
!$$
!F(i,I,a)=\int \int N^{I} T_{a} c_{i} ds dt
!$$
!

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector3(test, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    !! space time element shape functions
    TYPE(FEVariable_), INTENT(IN) :: c
    !! vector finite variable
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    !! vector finite variable
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
    !! space time force vector
    !! first index: space component
    !! second index: space nodal value
    !! third index: time nodal value
  END FUNCTION obj_STForceVector3
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Space time force vector
!
!# STForceVector_
!
! In this method following space time force vector is computed.
! In this case `c` is vector FEVariable.
! The result denotes vector force vector.
!
!$$
!F(i,I,a)=\int \int N^{I} T_{a} c_{i} ds dt
!$$

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_3( &
    test, c, crank, ans, dim1, dim2, dim3, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_3
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Space time force vector for matrix
!
!# STForceVector
!
! In this method following space-time force vector is computed.
! In this case `c` is matrix FEVariable.
! The result denotes matrix force vector.
!
!$$
!F(i,j,I,a)=\int \int N^{I} T_{a} c_{ij} ds dt
!$$

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector4(test, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    !! vector of space-time element shape data
    TYPE(FEVariable_), INTENT(IN) :: c
    !! matrix finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: crank
    !! matrix finite element variable
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! space-time force vector for matrix
    !! index1: row component of matrix
    !! index2: col component of matrix
    !! index3: space node
    !! index4: time node
  END FUNCTION obj_STForceVector4
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for matrix.
!
!# STForceVector_
!
! In this method following space-time force vector is computed.
! In this case `c` is matrix FEVariable.
! The result denotes matrix force vector.
!
!$$
!F(i,j,I,a)=\int \int N^{I} T_{a} c_{ij} ds dt
!$$

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_4( &
    test, c, crank, ans, dim1, dim2, dim3, dim4, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableMatrix_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_4
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Space-time force vector for scalar
!
!# STForceVector
!
! In this method following space time force vector is computed.
! In this case `c1` is scalar FEVariable.
! In this case `c2` is scalar FEVariable.
!
!$$
!F(I,a)=\int \int N^{I} T_{a} \rho_{1} \rho_{2} ds dt
!$$
!
! Here $\rho_{1}$ is denoted by $c1$.
! Here $\rho_{2}$ is denoted by $c2$.

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector5(test, c1, c1rank, c2, c2rank) &
    RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    !! a vector of space time element shape data
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! scalar finite element variable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! scalar finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! scalar finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    !! scalar finite element variable
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! space time force vector for scalar
    !! index1: space index
    !! index2: time index
  END FUNCTION obj_STForceVector5
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Space time force vector for scalar
!
!# STForceVector_
!
! In this method following space time force vector is computed.
! In this case `c1` is scalar FEVariable.
! In this case `c2` is scalar FEVariable.
!
!$$
!F(I,a)=\int \int N^{I} T_{a} \rho_{1} \rho_{2} ds dt
!$$
!
! Here $\rho_{1}$ is denoted by $c1$.
! Here $\rho_{2}$ is denoted by $c2$.

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_5( &
    test, c1, c1rank, c2, c2rank, ans, nrow, ncol, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_5
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Space time force vector for vector
!
!# STForceVector_
!
! In this method following space time force vector is computed.
!
! In this case `c1` is scalar FEVariable.
! In this case `c2` is vector FEVariable.
!
!$$
!F(i,I,a)=\int \int N^{I} T_{a} \rho c_{i} ds dt
!$$
!
! Here $\rho$ is denoted by $c1$.
! Here $\mathbf{c}$ is denoted by $c2$.

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector6(test, c1, c1rank, c2, c2rank) &
    RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    !! a vector of space-time element shape data
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! scalar finite element variable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! vector finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! scalar finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    !! vector finite element variable
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
    !! space-vector force vector for vector
    !! index1: space component
    !! index2: space nodal value
    !! index3: time nodal value
  END FUNCTION obj_STForceVector6
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Space time force vector for vector
!
!# STForceVector_
!
! In this method following space time force vector is computed.
!
! In this case `c1` is scalar FEVariable.
! In this case `c2` is vector FEVariable.
!
!$$
!F(i,I,a)=\int \int N^{I} T_{a} \rho c_{i} ds dt
!$$
!
! Here $\rho$ is denoted by $c1$.
! Here $\mathbf{c}$ is denoted by $c2$.
INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_6( &
    test, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3, scale, &
    addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    !! space-time element shape data
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! scalar finite element variable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! vector finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! scalar finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    !! vector finite element variable
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! space-time force vector for vector
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! these are dimensions of data written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_6
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Space-time force vector for matrix
!
!# STForceVector
!
! In this method following space-time force vector for matrix is computed.
! In this case `c1` is scalar FEVariable.
! In this case `c2` is matrix FEVariable.
!
!$$
!F(i,j,I,a)=\int \int N^{I} T_{a} \rho c_{ij}  ds dt
!$$
!
! Here $\rho$ is denoted by $c1$.
! Here $\mathbf{c}$ is denoted by $c2$.

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector7(test, c1, c1rank, c2, c2rank) &
    RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    !! Vector of space-time element shape data
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! Scalar finite element variable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! Matrix finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! Scalar finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    !! Matrix finite element variable
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
    !! space-time force vector for matrix
    !! index1: row componenet of matrix
    !! index2: col componenet of matrix
    !! index3: space nodal values
    !! index4: time nodal values
  END FUNCTION obj_STForceVector7
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Space-time force vector for matrix
!
!# STForceVector
!
! In this method following space-time force vector for matrix is computed.
! In this case `c1` is scalar FEVariable.
! In this case `c2` is matrix FEVariable.
!
!$$
!F(i,j,I,a)=\int \int N^{I} T_{a} \rho c_{ij}  ds dt
!$$
!
! Here $\rho$ is denoted by $c1$.
! Here $\mathbf{c}$ is denoted by $c2$.

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_7( &
    test, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3, dim4, &
    scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_7
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for scalar

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector8(test, term1) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector8
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for scalar

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_8(test, term1, ans, nrow, ncol, &
                                             scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_8
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for scalar

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector9(test, term1, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableScalar_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector9
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for scalar

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_9( &
    test, term1, c, crank, ans, nrow, ncol, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableScalar_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_9
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for vector

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector10(test, term1, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION obj_STForceVector10
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for vector

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_10( &
    test, term1, c, crank, ans, dim1, dim2, dim3, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_10
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for matrix field

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector11(test, term1, c, crank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableMatrix_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION obj_STForceVector11
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for matrix field

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_11( &
    test, term1, c, crank, ans, dim1, dim2, dim3, dim4, scale, &
    addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableMatrix_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_11
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for scalar

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector12( &
    test, term1, c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector12
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for scalar

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_12( &
    test, term1, c1, c1rank, c2, c2rank, ans, nrow, ncol, scale, &
    addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_12
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for vector

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector13( &
    test, term1, c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION obj_STForceVector13
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for vector

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_13( &
    test, term1, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3, scale, &
    addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_13
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for matrix

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector14( &
    test, term1, c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION obj_STForceVector14
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for matrix

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_14( &
    test, term1, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3, dim4, &
    scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    INTEGER(I4B), INTENT(IN) :: term1
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_14
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for scalar

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector15(test, projection, c, crank) &
    RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector15
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for scalar

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_15( &
    test, projection, c, crank, ans, nrow, ncol, temp, scale, &
    addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
    !! temp array to keep projection data at ips and ipt
    !! size should be at least (nns x nnt)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_15
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for scalar

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector16( &
    test, projection, c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector16
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for scalar

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_16( &
    test, projection, c1, c1rank, c2, c2rank, ans, nrow, ncol, temp, &
    scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
    !! temp array to keep projection data at ips and ipt
    !! size should be at least (nns x nnt)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_16
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for vector

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector17( &
    test, projection, c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! projection is made on c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    !!
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION obj_STForceVector17
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for vector

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_17( &
    test, projection, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3, temp, &
    scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! projection is made on c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! c2 force vector
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_17
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for matrix

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector18( &
    test, projection, c1, c1rank, c2, c2rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION obj_STForceVector18
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for matrix

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_18( &
    test, projection, c1, c1rank, c2, c2rank, ans, dim1, dim2, dim3, dim4, &
    temp, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! projection vector
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_18
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for space

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector19( &
    test, projection, c1, c1rank, c2, c2rank, c3, c3rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariable_), INTENT(IN) :: c3
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c3rank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_STForceVector19
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for space

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_19( &
    test, projection, c1, c1rank, c2, c2rank, c3, c3rank, ans, nrow, ncol, &
    temp, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariable_), INTENT(IN) :: c3
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c3rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_19
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for vector

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector20( &
    test, projection, c1, c1rank, c2, c2rank, c3, c3rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariable_), INTENT(IN) :: c3
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    TYPE(FEVariableVector_), INTENT(IN) :: c3rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION obj_STForceVector20
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for vector

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_20( &
    test, projection, c1, c1rank, c2, c2rank, c3, c3rank, ans, &
    dim1, dim2, dim3, temp, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariable_), INTENT(IN) :: c3
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    !! projection on c1
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    TYPE(FEVariableVector_), INTENT(IN) :: c3rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_20
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for matrix

INTERFACE STForceVector
  MODULE PURE FUNCTION obj_STForceVector21( &
    test, projection, c1, c1rank, c2, c2rank, c3, c3rank) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariable_), INTENT(IN) :: c3
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c3rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)
  END FUNCTION obj_STForceVector21
END INTERFACE STForceVector

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space-time force vector for matrix

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_21( &
    test, projection, c1, c1rank, c2, c2rank, c3, c3rank, ans, dim1, dim2, &
    dim3, dim4, temp, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    CHARACTER(LEN=*), INTENT(IN) :: projection
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariable_), INTENT(IN) :: c3
    TYPE(FEVariableVector_), INTENT(IN) :: c1rank
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    TYPE(FEVariableMatrix_), INTENT(IN) :: c3rank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_21
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space time force vector without allocation
!
!# STForceVector_
!
! This method computes space time force vector.
!
!$$
!F(I,a)=\int \int N^{I} T_{a} ds dt
!$$
!
! $N$ is taken from testSpace, and $T$ is taken from testTime.

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_22( &
    testSpace, testTime, ans, nrow, ncol, scale, addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: testSpace
    !! test function for space
    CLASS(ElemshapeData_), INTENT(IN) :: testTime
    !! test function for time
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space time force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_22
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Space time force vector
!
!# STForceVector_
!
! In this method following space time force vector is computed.
! In this case `c` is scalar FEVariable.
!
!$$
!F(I,a)=\int \int N^{I} T_{a} \rho ds dt
!$$
!
! Here $\rho$ is denoted by $c$.
!
! In this method space shape functions are taken from
! testSpace, and time shape functions are taken from testTime.

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_23( &
    testSpace, testTime, c, crank, ans, nrow, ncol, scale, addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: testSpace
    !! test function for space
    CLASS(ElemshapeData_), INTENT(IN) :: testTime
    !! test function for time
    TYPE(FEVariable_), INTENT(IN) :: c
    !! scalar finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space time force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_23
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: Space time force vector
!
!# STForceVector_
!
! In this method following space time force vector is computed.
! In this case `c` is vector FEVariable.
! The result denotes vector force vector.
!
!$$
!F(i,I,a)=\int \int N^{I} T_{a} c_{i} ds dt
!$$
!
! In this case space shape function data is used from testSpace,
! and time shape function data is used from testTime

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_24( &
    testSpace, testTime, c, crank, ans, dim1, dim2, dim3, scale, &
    addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: testSpace
    CLASS(ElemshapeData_), INTENT(IN) :: testTime
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_24
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!                                                             STForceVector_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-06
! summary: space time force vector without allocation
!
!# STForceVector_
!
! This method computes space time force vector.
!
!$$
!F(I,a)=\int \int N^{I} T_{a} c_{1} c_{2} ds dt
!$$
!
! $N$ is taken from testSpace, and $T$ is taken from testTime.
!
! $c_{1}$ and $c_{2}$ are scalar finite variable.

INTERFACE STForceVector_
  MODULE PURE SUBROUTINE obj_STForceVector_25( &
    testSpace, testTime, c1, c1rank, c2, c2rank, ans, &
    nrow, ncol, scale, addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: testSpace
    !! test function in space
    CLASS(ElemshapeData_), INTENT(IN) :: testTime
    !! test function in time
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! scalar finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! scalar finite element variable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! scalar finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: c2rank
    !! scalar finite element variable
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space-time force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! upper bound of data written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_STForceVector_25
END INTERFACE STForceVector_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STForceVector_Method

