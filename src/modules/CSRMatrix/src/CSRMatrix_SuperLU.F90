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

MODULE CSRMatrix_Superlu
USE BaseType, ONLY: CSRMatrix_
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE
PRIVATE

! PUBLIC :: GetLU
! PUBLIC :: LUSolve
! PUBLIC :: ! Solve
PUBLIC :: InitiateSuperluRHS
PUBLIC :: InitiateSuperluA
PUBLIC :: LinSolve
PUBLIC :: SuperluDeallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  23-01-26
! summary: Initiate Superlu data structure inside csrmatrix

INTERFACE
  MODULE SUBROUTINE InitiateSuperluA(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
  END SUBROUTINE InitiateSuperluA
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  23-01-26
! summary: Initiate Superlu data structure inside csrmatrix

INTERFACE
  MODULE SUBROUTINE SetSuperluA(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
  END SUBROUTINE SetSuperluA
END INTERFACE

!----------------------------------------------------------------------------
!                                                      InitiateSuperluRHS
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-26
! summary: Set RHS

INTERFACE InitiateSuperluRHS
  MODULE SUBROUTINE InitiateSuperluRHS1(obj, rhs)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: rhs(:)
  END SUBROUTINE InitiateSuperluRHS1
END INTERFACE InitiateSuperluRHS

!----------------------------------------------------------------------------
!                                                      InitiateSuperluRHS
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-26
! summary: Set RHS

INTERFACE InitiateSuperluRHS
  MODULE SUBROUTINE InitiateSuperluRHS2(obj, rhs)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: rhs(:, :)
  END SUBROUTINE InitiateSuperluRHS2
END INTERFACE InitiateSuperluRHS

!----------------------------------------------------------------------------
!                                                             SetSuperluRHS
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-26
! summary: Set RHS

INTERFACE SetSuperluRHS
  MODULE SUBROUTINE SetSuperluRHS1(obj, rhs)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: rhs(:)
  END SUBROUTINE SetSuperluRHS1
END INTERFACE SetSuperluRHS

!----------------------------------------------------------------------------
!                                                              SetSuperluRHS
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-26
! summary: Set RHS

INTERFACE SetSuperluRHS
  MODULE SUBROUTINE SetSuperluRHS2(obj, rhs)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: rhs(:, :)
  END SUBROUTINE SetSuperluRHS2
END INTERFACE SetSuperluRHS

!----------------------------------------------------------------------------
!                                                               GetSuperlux
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-26
! summary: Get solutions

INTERFACE GetSuperlux
  MODULE SUBROUTINE GetSuperluX1(obj, x)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: x(:)
  END SUBROUTINE GetSuperluX1
END INTERFACE GetSuperlux

!----------------------------------------------------------------------------
!                                                               GetSuperlux
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-26
! summary: Get solutions

INTERFACE GetSuperlux
  MODULE SUBROUTINE GetSuperluX2(obj, x)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: x(:, :)
  END SUBROUTINE GetSuperluX2
END INTERFACE GetSuperlux

!----------------------------------------------------------------------------
!                                                    InitiateSuperLuOptions
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-26
! summary: Initiate Superlu Options

INTERFACE
  MODULE SUBROUTINE InitiateSuperLuOptions(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
  END SUBROUTINE InitiateSuperLuOptions
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetSuperluOptions
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-26
! summary: Set options for superlu

INTERFACE
  MODULE SUBROUTINE SetSuperluOptions(obj, Fact, Equil, ColPerm, &
      & Trans, IterRefine, DiagPivotThresh, SymmetricMode, &
      & PivotGrowth, ConditionNumber, RowPerm, ILU_DropRule, &
      & ILU_DropTol, ILU_FillFactor, ILU_MILU, PrintStat)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Fact
    !! Fact_t
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Equil
    !! yes_no_t%YES, yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ColPerm
    !! Colperm_t%COLAMD
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Trans
    !! Trans_t%TRANS, Trans_t%
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: IterRefine
    !! IterRefine_t
    REAL(DFP), OPTIONAL, INTENT(IN) :: DiagPivotThresh
    !! From 0 to 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: SymmetricMode
    !!
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: PivotGrowth
    !! yes_no_t%YES, yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ConditionNumber
    !! yes_no_t%YES, yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: RowPerm
    !!
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ILU_DropRule
    !!
    REAL(DFP), OPTIONAL, INTENT(IN) :: ILU_DropTol
    !!
    REAL(DFP), OPTIONAL, INTENT(IN) :: ILU_FillFactor
    !!
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ILU_MILU
    !!
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: PrintStat
    !!
  END SUBROUTINE SetSuperluOptions
END INTERFACE

!----------------------------------------------------------------------------
!                                                             SuperluDGSSVX
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-27
! summary: Call Superlu DGSSVX to solve Ax=b

INTERFACE
  MODULE SUBROUTINE SuperluDGSSVX(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
  END SUBROUTINE SuperluDGSSVX
END INTERFACE

!----------------------------------------------------------------------------
!                                               InitiateSuperluDGSSVXParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-27
! summary: Initiate Superlu DGSSVX variables

INTERFACE
  MODULE SUBROUTINE InitiateSuperluDGSSVXParam(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
  END SUBROUTINE InitiateSuperluDGSSVXParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SuperluPrintStat
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-27
! summary: Print statistics

INTERFACE
  MODULE SUBROUTINE SuperluDisplayStat(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
  END SUBROUTINE SuperluDisplayStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SuperluDeallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-27
! summary: SuperluDeallocate

INTERFACE
  MODULE SUBROUTINE SuperluDeallocate(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
  END SUBROUTINE SuperluDeallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                LinSolve1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-26
! summary: Linear solver using LUSolve
!
!# Introduction
!
! This routine solves `A*X=B`

INTERFACE LinSolve
  MODULE SUBROUTINE LinSolve1(X, A, B, isTranspose, isFactored, &
    & ColPerm, Equil, IterRefine, PivotGrowth, DiagPivotThresh, &
    & ConditionNumber, SymmetricMode, PrintStat, info)
    REAL(DFP), INTENT(INOUT) :: X(:)
    !! Solution
    TYPE(CSRMatrix_), INTENT(INOUT) :: A
    !! CSRMatrix
    REAL(DFP), INTENT(IN) :: B(:)
    !! RHS, it will not be modified, we will make a copy of it
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! Should we solve `A*X=B` or `transpose(A)*X=B`
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFactored
    !! is A already factored
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ColPerm
    !! Colperm_t%NATURAL
    !! Colperm_t%MMD_ATA
    !! Colperm_t%MMD_AT_PLUS_A
    !! Colperm_t%COLAMD
    !! Colperm_t%MY_PERMC
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Equil
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: IterRefine
    !! IterRefine_t%NO
    !! IterRefine_t%SLU_SINGLE
    !! IterRefine_t%SLU_DOUBLE
    !! IterRefine_t%SLU_EXTRA
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: PivotGrowth
    !! yes_no_t%YES
    !! yes_no_t%NO
    REAL(DFP), OPTIONAL, INTENT(IN) :: DiagPivotThresh
    !! between 0 and 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ConditionNumber
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: SymmetricMode
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: PrintStat
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! if info equal  to zero then success, else failure
  END SUBROUTINE LinSolve1
END INTERFACE LinSolve

!----------------------------------------------------------------------------
!                                                         LinSolve2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-26
! summary: Linear solver using LUSolve
!
!# Introduction
!
! This routine solves `A*X=B`

INTERFACE LinSolve
  MODULE SUBROUTINE LinSolve2(X, A, B, isTranspose, isFactored, &
    & ColPerm, Equil, IterRefine, PivotGrowth, DiagPivotThresh, &
    & ConditionNumber, SymmetricMode, PrintStat, info)
    REAL(DFP), INTENT(INOUT) :: X(:, :)
    !! Solution
    TYPE(CSRMatrix_), INTENT(INOUT) :: A
    !! CSRMatrix
    REAL(DFP), INTENT(IN) :: B(:, :)
    !! RHS, it will not be modified, we will make a copy of it
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! Should we solve `A*X=B` or `transpose(A)*X=B`
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFactored
    !! is A already factored
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ColPerm
    !! Colperm_t%NATURAL
    !! Colperm_t%MMD_ATA
    !! Colperm_t%MMD_AT_PLUS_A
    !! Colperm_t%COLAMD
    !! Colperm_t%MY_PERMC
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Equil
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: IterRefine
    !! IterRefine_t%NO
    !! IterRefine_t%SLU_SINGLE
    !! IterRefine_t%SLU_DOUBLE
    !! IterRefine_t%SLU_EXTRA
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: PivotGrowth
    !! yes_no_t%YES
    !! yes_no_t%NO
    REAL(DFP), OPTIONAL, INTENT(IN) :: DiagPivotThresh
    !! between 0 and 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ConditionNumber
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: SymmetricMode
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: PrintStat
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! if info equal  to zero then success, else failure
  END SUBROUTINE LinSolve2
END INTERFACE LinSolve

!----------------------------------------------------------------------------
!                                                                LinSolve1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-26
! summary: Linear solver using LUSolve
!
!# Introduction
!
! This routine solves `A*X=B`
! Solution is returned in B

INTERFACE LinSolve
  MODULE SUBROUTINE LinSolve3(A, B, isTranspose, isFactored, &
    & ColPerm, Equil, IterRefine, PivotGrowth, DiagPivotThresh, &
    & ConditionNumber, SymmetricMode, PrintStat, info)
    TYPE(CSRMatrix_), INTENT(INOUT) :: A
    !! CSRMatrix
    REAL(DFP), INTENT(INOUT) :: B(:)
    !! RHS, it will not be modified, we will make a copy of it
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! Should we solve `A*X=B` or `transpose(A)*X=B`
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFactored
    !! is A already factored
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ColPerm
    !! Colperm_t%NATURAL
    !! Colperm_t%MMD_ATA
    !! Colperm_t%MMD_AT_PLUS_A
    !! Colperm_t%COLAMD
    !! Colperm_t%MY_PERMC
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Equil
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: IterRefine
    !! IterRefine_t%NO
    !! IterRefine_t%SLU_SINGLE
    !! IterRefine_t%SLU_DOUBLE
    !! IterRefine_t%SLU_EXTRA
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: PivotGrowth
    !! yes_no_t%YES
    !! yes_no_t%NO
    REAL(DFP), OPTIONAL, INTENT(IN) :: DiagPivotThresh
    !! between 0 and 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ConditionNumber
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: SymmetricMode
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: PrintStat
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! if info equal  to zero then success, else failure
  END SUBROUTINE LinSolve3
END INTERFACE LinSolve

!----------------------------------------------------------------------------
!                                                         LinSolve2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-26
! summary: Linear solver using LUSolve
!
!# Introduction
!
! This routine solves `A*X=B`

INTERFACE LinSolve
  MODULE SUBROUTINE LinSolve4(A, B, isTranspose, isFactored, &
    & ColPerm, Equil, IterRefine, PivotGrowth, DiagPivotThresh, &
    & ConditionNumber, SymmetricMode, PrintStat, info)
    TYPE(CSRMatrix_), INTENT(INOUT) :: A
    !! CSRMatrix
    REAL(DFP), INTENT(INOUT) :: B(:, :)
    !! RHS, it will be modified on return, solution is in B
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! Should we solve `A*X=B` or `transpose(A)*X=B`
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFactored
    !! is A already factored
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ColPerm
    !! Colperm_t%NATURAL
    !! Colperm_t%MMD_ATA
    !! Colperm_t%MMD_AT_PLUS_A
    !! Colperm_t%COLAMD
    !! Colperm_t%MY_PERMC
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Equil
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: IterRefine
    !! IterRefine_t%NO
    !! IterRefine_t%SLU_SINGLE
    !! IterRefine_t%SLU_DOUBLE
    !! IterRefine_t%SLU_EXTRA
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: PivotGrowth
    !! yes_no_t%YES
    !! yes_no_t%NO
    REAL(DFP), OPTIONAL, INTENT(IN) :: DiagPivotThresh
    !! between 0 and 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ConditionNumber
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: SymmetricMode
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: PrintStat
    !! yes_no_t%YES
    !! yes_no_t%NO
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! if info equal  to zero then success, else failure
  END SUBROUTINE LinSolve4
END INTERFACE LinSolve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE CSRMatrix_Superlu
