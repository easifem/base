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

PUBLIC :: SetSparsity

!----------------------------------------------------------------------------
!                                                    SetSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern of a given row
!  - If `obj%tdof` is equal to 1, then `Col` is sorted in increasing order,
! and appended to `obj%Row(Row)`
! - If `obj%tdof` is not equal to 1, then based on the storage format and
! `Col` connectivity information is generated.

INTERFACE
MODULE SUBROUTINE csrMat_setSparsity1( obj, row, col )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: row
  INTEGER( I4B ), INTENT( IN ) :: col( : )
END SUBROUTINE csrMat_setSparsity1
END INTERFACE

INTERFACE SetSparsity
  MODULE PROCEDURE csrMat_setSparsity1
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                      SetSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine sets the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern of a given row
! This subroutine calls `csrMat_setSparsity1`

INTERFACE
MODULE SUBROUTINE csrMat_setSparsity2( obj, row, col )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: row( : )
  TYPE( IntVector_ ), INTENT( IN ) :: col( : )
END SUBROUTINE csrMat_setSparsity2
END INTERFACE

INTERFACE SetSparsity
  MODULE PROCEDURE csrMat_setSparsity2
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                      SetSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern of a given row
!  - If `obj%tdof` is equal to 1, then `Col` is sorted in increasing order,
! and appended to `obj%Row(Row)`
! - If `obj%tdof` is not equal to 1, then based on the storage format and
! `Col` connectivity information is generated.

INTERFACE
MODULE SUBROUTINE csrMat_setSparsity3( obj, row, col, ivar, jvar )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: row
  INTEGER( I4B ), INTENT( IN ) :: col( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
END SUBROUTINE csrMat_setSparsity3
END INTERFACE

INTERFACE SetSparsity
  MODULE PROCEDURE csrMat_setSparsity3
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                      SetSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine sets the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine sets the sparsity pattern of a given row
! This subroutine calls `csrMat_setSparsity1`

INTERFACE
MODULE SUBROUTINE csrMat_setSparsity4( obj, row, col, ivar, jvar )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: row( : )
  TYPE( IntVector_ ), INTENT( IN ) :: col( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
END SUBROUTINE csrMat_setSparsity4
END INTERFACE

INTERFACE SetSparsity
  MODULE PROCEDURE csrMat_setSparsity4
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                      SetSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine set sparsity pattern of `sparsematrix_`
!
!# Introduction
!
! authors: Dr. Vikas Sharma
!
! This subroutine set sparsity pattern of `sparsematrix_`
! This will finally set the data into
!
! - `obj%A(:)`
! - `obj%IA(:)`
! - `obj%JA(:)`
! in CSR format. This routine also set data inside `obj%ColSize(:)` and
! `obj%RowSize(:) `, and `obj%DiagIndx(:)`

INTERFACE
MODULE SUBROUTINE csrMat_setSparsity_final( obj )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
END SUBROUTINE csrMat_setSparsity_final
END INTERFACE

INTERFACE SetSparsity
  MODULE PROCEDURE csrMat_setSparsity_final
END INTERFACE SetSparsity

