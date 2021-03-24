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

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Unary operator for [[SparseMaatrix_]]

SUBMODULE( SparseMatrix_Method ) Unary
USE BaseMethod
IMPLICIT NONE

CONTAINS

#include "./csort.inc"
#include "./clncsr.inc"
#include "./getelm.inc"

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE aij_convert_dns
  INTEGER( I4B ) :: i, j, nrow
  nrow = SIZE( IA ) - 1
  CALL Reallocate( mat, nrow, nrow )
  DO i = 1, nrow
    DO j = IA( i ), IA( i + 1 ) - 1
      mat( i, JA( j ) ) = A( j )
    END DO
  END DO
END PROCEDURE aij_convert_dns

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_convert_dns
  INTEGER( I4B ) :: i, j
  CALL Reallocate( To, From%nrow, From%ncol )
  DO i = 1, From%nrow
    DO j = From%IA( i ), From%IA( i + 1 ) - 1
      To( i, From%JA( j ) ) = From%A( j )
    END DO
  END DO
END PROCEDURE obj_convert_dns

!----------------------------------------------------------------------------
!                                                                    ColSORT
!----------------------------------------------------------------------------

MODULE PROCEDURE csort_csr
  CALL CSORT_SAAD( A=Obj%A, JA=Obj%JA, IA=Obj%IA, &
    & Values=INPUT( Option = isValues, Default = .TRUE. ) )
END PROCEDURE csort_csr

!----------------------------------------------------------------------------
!                                                           RemoveDuplicates
!----------------------------------------------------------------------------

MODULE PROCEDURE remove_duplicates_csr
  CALL Reallocate( Obj%IndexUT, Obj%nrow )
  CALL CLNCSR_SAAD( A=Obj%A, JA=Obj%JA, IA=Obj%IA, IndexUT = Obj%IndexUT, &
    & JOB=1, isValues = INPUT( Option = isValues, Default = .TRUE. ) )
END PROCEDURE remove_duplicates_csr

!----------------------------------------------------------------------------
!                                                                      Clean
!----------------------------------------------------------------------------

MODULE PROCEDURE clean_csr
  CALL Reallocate( Obj%IndexUT, Obj%nrow )
  CALL CLNCSR_SAAD( A=Obj%A, JA=Obj%JA, IA=Obj%IA, IndexUT=Obj%IndexUT, &
    & JOB=INPUT( Option = ExtraOption, Default = 1), &
    & isValues = INPUT( Option = isValues, Default = .TRUE. ) )
END PROCEDURE clean_csr

!----------------------------------------------------------------------------
!                                                                      Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE copy_csr_csr
  CALL initiate( Obj = To, tdof = From%tdof, tNodes = [From%tNodes], &
    & MatrixProp = From%MatrixProp )
  CALL Reallocate( To%A, From%nnz )
  To%IA = From%IA
  To%JA = From%JA
  IF( ALLOCATED( From%A ) ) To%A = From%A
  IF( ALLOCATED( From%RowSize ) ) To%RowSize = From%RowSize
  IF( ALLOCATED( From%ColSize ) ) To%ColSize = From%ColSize
  IF( ALLOCATED( From%Diag ) ) To%Diag = From%Diag
  IF( ALLOCATED( From%DiagIndx ) ) To%DiagIndx = From%DiagIndx
  IF( ALLOCATED( From%IndexUT ) ) To%IndexUT = From%IndexUT
END PROCEDURE copy_csr_csr

!----------------------------------------------------------------------------
!                                                                ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE get_scalar_value
  Ans = GETELM_SAAD( I=I, J=J, A=Obj%A, IA=Obj%IA, JA=Obj%JA, &
    & SORTED=INPUT( Default=Obj%isSorted, Option=SORTED ))
END PROCEDURE get_scalar_value

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Unary