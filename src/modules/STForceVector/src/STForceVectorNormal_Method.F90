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

MODULE STForceVectorNormal_Method
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: FEVariableScalar_
USE BaseType, ONLY: FEVariableVector_
USE BaseType, ONLY: FEVariableMatrix_
IMPLICIT NONE
PRIVATE

PUBLIC :: STForceVectorNormal_

!----------------------------------------------------------------------------
!                                                       STForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute space-time normal force vector
!
!# STForceVectorNormal_
!
! This subroutine computes the following expression:
!
! $$
! F_{I,a}=\int \int N^{I} T_{a} \mathbf{c} \cdot \mathbf{n} d\Omega
! $$

INTERFACE STForceVectorNormal_
  MODULE SUBROUTINE STForceVectorNormal_1( &
    testSpace, testTime, c, ans, nrow, ncol, scale, addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: testSpace
    !! shape function for space
    CLASS(ElemshapeData_), INTENT(IN) :: testTime
    !! shape function for time
    REAL(DFP), INTENT(IN) :: c(:)
    !! this vector is used for projecting on normal
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space time normal force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution to ans
  END SUBROUTINE STForceVectorNormal_1
END INTERFACE STForceVectorNormal_

!----------------------------------------------------------------------------
!                                                       STForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute space-time normal force vector
!
!# STForceVectorNormal_
!
! This subroutine computes the following expression:
!
!$$
!F_{I,a}=\int \int N^{I} T_{a} \mathbf{c} \cdot \mathbf{n} d\Omega
!$$

INTERFACE STForceVectorNormal_
  MODULE SUBROUTINE STForceVectorNormal_2( &
    test, c, ans, nrow, ncol, scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    !! shape function for space
    REAL(DFP), INTENT(IN) :: c(:)
    !! this vector is used for projecting on normal
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space time normal force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution to ans
  END SUBROUTINE STForceVectorNormal_2
END INTERFACE STForceVectorNormal_

!----------------------------------------------------------------------------
!                                                      STForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# STForceVectorNormal_
!
! This subroutine computes the following expression:
!
!$$
!F_{I,a}=\int \int N^{I} T_{a} \mathbf{c} \cdot \mathbf{n} d\Omega
!$$

INTERFACE STForceVectorNormal_
  MODULE SUBROUTINE STForceVectorNormal_3( &
    testSpace, testTime, c, crank, ans, nrow, ncol, scale, addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: testSpace
    !! test shape data for space
    CLASS(ElemshapeData_), INTENT(IN) :: testTime
    !! test shape data for time
    TYPE(FEVariable_), INTENT(IN) :: c
    !! vector finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    !! vector finite element variable
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space time force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution
  END SUBROUTINE STForceVectorNormal_3
END INTERFACE STForceVectorNormal_

!----------------------------------------------------------------------------
!                                                      STForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# STForceVectorNormal_
!
! This subroutine computes the following expression:
!
!$$
!F_{I,a}=\int \int N^{I} T_{a} \mathbf{c} \cdot \mathbf{n} d\Omega
!$$

INTERFACE STForceVectorNormal_
  MODULE SUBROUTINE STForceVectorNormal_4( &
    test, c, crank, ans, nrow, ncol, scale, addContribution)
    CLASS(STElemShapeData_), INTENT(IN) :: test(:)
    !! test shape data for space
    TYPE(FEVariable_), INTENT(IN) :: c
    !! vector finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    !! vector finite element variable
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space time force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution
  END SUBROUTINE STForceVectorNormal_4
END INTERFACE STForceVectorNormal_

!----------------------------------------------------------------------------
!                                                        STForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# STForceVectorNormal_
!
! This subroutine computes the following expression:
!
!$$
!F_{I,a}=\int \int N^{I} T_{a} \mathbf{c} \cdot \mathbf{n} d\Omega
!$$

INTERFACE STForceVectorNormal_
  MODULE SUBROUTINE STForceVectorNormal_5( &
    testSpace, testTime, c1, c1rank, c2, ans, nrow, ncol, scale, &
    addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: testSpace
    !! test element shape data for space
    CLASS(ElemshapeData_), INTENT(IN) :: testTime
    !! test element shape data for time
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! scalar finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! scalar finite element variable
    REAL(DFP), INTENT(IN) :: c2(:)
    !! normal vector will be projected on c2
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space-time normal force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution
  END SUBROUTINE STForceVectorNormal_5
END INTERFACE STForceVectorNormal_

!----------------------------------------------------------------------------
!                                                       STForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# STForceVectorNormal_
!
! This subroutine computes the following expression:
!
!$$
!F_{I,a}=\int \int N^{I} T_{a} \mathbf{c} \cdot \mathbf{n} d\Omega
!$$

INTERFACE STForceVectorNormal_
  MODULE SUBROUTINE STForceVectorNormal_6( &
    test, c1, c1rank, c2, ans, nrow, ncol, scale, &
    addContribution)
    CLASS(STElemShapeData_), INTENT(IN) :: test(:)
    !! test element shape data for space
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! scalar finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! scalar finite element variable
    REAL(DFP), INTENT(IN) :: c2(:)
    !! normal vector will be projected on c2
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space-time normal force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution
  END SUBROUTINE STForceVectorNormal_6
END INTERFACE STForceVectorNormal_

!----------------------------------------------------------------------------
!                                                       STForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# STForceVectorNormal_
!
! This subroutine computes the following expression:
!
!$$
!F_{I,a}=\int \int N^{I} T_{a} c_{1} \mathbf{c}_{2} \cdot \mathbf{n} d\Omega
!$$

INTERFACE STForceVectorNormal_
  MODULE SUBROUTINE STForceVectorNormal_7( &
    testSpace, testTime, c1, c1rank, c2, c2rank, ans, nrow, ncol, scale, &
    addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: testSpace
    !! element shape data for space
    CLASS(ElemshapeData_), INTENT(IN) :: testTime
    !! element shape data for time
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! scalar finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! scalar finite element variable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! vector finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    !! vector finite element variable
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space-time force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution
  END SUBROUTINE STForceVectorNormal_7
END INTERFACE STForceVectorNormal_

!----------------------------------------------------------------------------
!                                                       STForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# STForceVectorNormal_
!
! This subroutine computes the following expression:
!
!$$
!F_{I,a}=\int \int N^{I} T_{a} c_{1} \mathbf{c}_{2} \cdot \mathbf{n} d\Omega
!$$

INTERFACE STForceVectorNormal_
  MODULE SUBROUTINE STForceVectorNormal_8( &
    test, c1, c1rank, c2, c2rank, ans, nrow, ncol, scale, &
    addContribution)
    CLASS(STElemShapeData_), INTENT(IN) :: test(:)
    !! element shape data for space
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! scalar finite element variable
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    !! scalar finite element variable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! vector finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    !! vector finite element variable
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space-time force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution
  END SUBROUTINE STForceVectorNormal_8
END INTERFACE STForceVectorNormal_

!----------------------------------------------------------------------------
!                                                        STForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# STForceVectorNormal_
!
! This subroutine computes the following expression:
!
!$$
!F_{I,a}=\int \int N^{I} T_{a} \mathbf{k} \cdot \mathbf{c} \cdot \mathbf{n}
!d\Omega
!$$

INTERFACE STForceVectorNormal_
  MODULE SUBROUTINE STForceVectorNormal_9( &
    testSpace, testTime, c1, c1rank, c2, ans, nrow, ncol, scale, &
    addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: testSpace
    !! test element shape data for space
    CLASS(ElemshapeData_), INTENT(IN) :: testTime
    !! test element shape data for time
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! matrix finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: c1rank
    !! matrix finite element variable
    REAL(DFP), INTENT(IN) :: c2(:)
    !! normal vector will be projected
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space-time nodal vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution
  END SUBROUTINE STForceVectorNormal_9
END INTERFACE STForceVectorNormal_

!----------------------------------------------------------------------------
!                                                       STForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# STForceVectorNormal_
!
! This subroutine computes the following expression:
!
!$$
!F_{I,a}=\int \int N^{I} T_{a} \mathbf{k} \cdot \mathbf{c} \cdot \mathbf{n}
!d\Omega
!$$

INTERFACE STForceVectorNormal_
  MODULE SUBROUTINE STForceVectorNormal_10( &
    test, c1, c1rank, c2, ans, nrow, ncol, scale, &
    addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    !! test element shape data for space
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! matrix finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: c1rank
    !! matrix finite element variable
    REAL(DFP), INTENT(IN) :: c2(:)
    !! normal vector will be projected
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space-time nodal vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution
  END SUBROUTINE STForceVectorNormal_10
END INTERFACE STForceVectorNormal_

!----------------------------------------------------------------------------
!                                                       STForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# STForceVectorNormal_
!
!This subroutine computes the following expression:
!
!$$
!F_{I,a}=\int \int N^{I} T_{a} \mathbf{k} \cdot \mathbf{c} \cdot \mathbf{n}
!d\Omega
!$$

INTERFACE STForceVectorNormal_
  MODULE SUBROUTINE STForceVectorNormal_11( &
    testSpace, testTime, c1, c1rank, c2, c2rank, ans, nrow, ncol, scale, &
    addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: testSpace
    !! test element shape data for space
    CLASS(ElemshapeData_), INTENT(IN) :: testTime
    !! test element shape data for time
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! matrix finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: c1rank
    !! matrix finite element variable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! vector finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    !! vector finite element variable
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space time force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written to ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution to ans
  END SUBROUTINE STForceVectorNormal_11
END INTERFACE STForceVectorNormal_

!----------------------------------------------------------------------------
!                                                       STForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# STForceVectorNormal_
!
!This subroutine computes the following expression:
!
!$$
!F_{I,a}=\int \int N^{I} T_{a} \mathbf{k} \cdot \mathbf{c} \cdot \mathbf{n}
!d\Omega
!$$

INTERFACE STForceVectorNormal_
  MODULE SUBROUTINE STForceVectorNormal_12( &
    test, c1, c1rank, c2, c2rank, ans, nrow, ncol, scale, &
    addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: test(:)
    !! vector of space time element shape data
    TYPE(FEVariable_), INTENT(IN) :: c1
    !! matrix finite element variable
    TYPE(FEVariableMatrix_), INTENT(IN) :: c1rank
    !! matrix finite element variable
    TYPE(FEVariable_), INTENT(IN) :: c2
    !! vector finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    !! vector finite element variable
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! space time force vector
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written to ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add contribution to ans
  END SUBROUTINE STForceVectorNormal_12
END INTERFACE STForceVectorNormal_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STForceVectorNormal_Method
