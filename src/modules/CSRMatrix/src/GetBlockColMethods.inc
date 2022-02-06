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

PUBLIC :: GetBlockColumn

!----------------------------------------------------------------------------
!                                                  getBlockColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the col of a sparse matrix
!
!# Introduction
!
! - This routine returns the col of given block matrix
! - This routine is designed to handle block matrices, which
! means it only works when StorageFMT is DOF_FMT
! - ivar is the row number for the block matrix, whose col are to be
! extracting
! - the result is returned inside `value`.
! - `value` should be allocated
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE
MODULE SUBROUTINE csrMat_getBlockColumn1( obj, ivar, iColumn, &
  & value, scale, addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: iColumn
  REAL( DFP ), INTENT( INOUT ) :: value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getBlockColumn1
END INTERFACE

INTERFACE getBlockColumn
  MODULE PROCEDURE csrMat_getBlockColumn1
END INTERFACE getBlockColumn

!----------------------------------------------------------------------------
!                                                  getBlockColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the col of a sparse matrix
!
!# Introduction
!
! - This routine returns the col of given block matrix
! - This routine is designed to handle block matrices, which
! means it only works when StorageFMT is DOF_FMT
! - ivar is the row number for the block matrix, whose col are to be
! extracting
! - the result is returned inside `value`.
! - `value` should be allocated
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE
MODULE SUBROUTINE csrMat_getBlockColumn1b( obj, ivar, iColumn, &
  & value, scale, addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: iColumn(:)
  REAL( DFP ), INTENT( INOUT ) :: value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getBlockColumn1b
END INTERFACE

INTERFACE getBlockColumn
  MODULE PROCEDURE csrMat_getBlockColumn1b
END INTERFACE getBlockColumn

!----------------------------------------------------------------------------
!                                                  getBlockColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the col of a sparse matrix
!
!# Introduction
!
! - This routine returns the col of given block matrix
! - This routine is designed to handle block matrices, which
! means it only works when StorageFMT is DOF_FMT
! - ivar is the row number for the block matrix, whose col are to be
! extracting
! - the result is returned inside `value`.
! - `value` should be allocated
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE
MODULE SUBROUTINE csrMat_getBlockColumn2( obj, ivar, nodenum, idof, &
  & value, scale, addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), INTENT( INOUT ) :: value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getBlockColumn2
END INTERFACE

INTERFACE getBlockColumn
  MODULE PROCEDURE csrMat_getBlockColumn2
END INTERFACE getBlockColumn

!----------------------------------------------------------------------------
!                                                  getBlockColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the nodenum and idof.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from nodenum and idof depends upon the storageFMT.
!
! Here, jvar, idof, and nodenum is used to calculate the index of
! physical variable jvar and its degree of freedom
!

INTERFACE
MODULE SUBROUTINE csrMat_getBlockColumn3( obj, ivar, jvar, nodenum, idof, &
  & value, scale,  addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), INTENT( INOUT ) :: value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getBlockColumn3
END INTERFACE

INTERFACE getBlockColumn
  MODULE PROCEDURE csrMat_getBlockColumn3
END INTERFACE getBlockColumn

!----------------------------------------------------------------------------
!                                                  getBlockColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the nodenum and idof.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from nodenum and idof depends upon the storageFMT.
!
! Here, jvar, idof, and nodenum is used to calculate the index of
! physical variable jvar and its degree of freedom
!

INTERFACE
MODULE SUBROUTINE csrMat_getBlockColumn4( obj, ivar, jvar, nodenum, &
  & spacecompo, timecompo, value, scale,  addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo
  REAL( DFP ), INTENT( INOUT ) :: value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getBlockColumn4
END INTERFACE

INTERFACE getBlockColumn
  MODULE PROCEDURE csrMat_getBlockColumn4
END INTERFACE getBlockColumn

!----------------------------------------------------------------------------
!                                                  getBlockColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the nodenum and idof.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from nodenum and idof depends upon the storageFMT.
!
! Here, jvar, idof, and nodenum is used to calculate the index of
! physical variable jvar and its degree of freedom
!

INTERFACE
MODULE SUBROUTINE csrMat_getBlockColumn5( obj, ivar, jvar, nodenum, &
  & spacecompo, timecompo, value, scale,  addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo(:)
  REAL( DFP ), INTENT( INOUT ) :: value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getBlockColumn5
END INTERFACE

INTERFACE getBlockColumn
  MODULE PROCEDURE csrMat_getBlockColumn5
END INTERFACE getBlockColumn

!----------------------------------------------------------------------------
!                                                  getBlockColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the nodenum and idof.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from nodenum and idof depends upon the storageFMT.
!
! Here, jvar, idof, and nodenum is used to calculate the index of
! physical variable jvar and its degree of freedom
!

INTERFACE
MODULE SUBROUTINE csrMat_getBlockColumn6( obj, ivar, jvar, nodenum, &
  & spacecompo, timecompo, value, scale,  addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  INTEGER( I4B ), INTENT( IN ) :: spacecompo(:)
  INTEGER( I4B ), INTENT( IN ) :: timecompo
  REAL( DFP ), INTENT( INOUT ) :: value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getBlockColumn6
END INTERFACE

INTERFACE getBlockColumn
  MODULE PROCEDURE csrMat_getBlockColumn6
END INTERFACE getBlockColumn

!----------------------------------------------------------------------------
!                                                  getBlockColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the nodenum and idof.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from nodenum and idof depends upon the storageFMT.
!
! Here, jvar, idof, and nodenum is used to calculate the index of
! physical variable jvar and its degree of freedom
!

INTERFACE
MODULE SUBROUTINE csrMat_getBlockColumn7( obj, ivar, jvar, nodenum, &
  & spacecompo, timecompo, value, scale,  addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: nodenum(:)
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo(:)
  REAL( DFP ), INTENT( INOUT ) :: value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getBlockColumn7
END INTERFACE

INTERFACE getBlockColumn
  MODULE PROCEDURE csrMat_getBlockColumn7
END INTERFACE getBlockColumn

!----------------------------------------------------------------------------
!                                                  getBlockColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the Column of a sparse matrix
!
!# Introduction
!
! - This routine returns the Column of a sparse matrix. The Column index is
! calculated using the nodenum and idof.
! - `nodenum` is the node number
! - `idof` is the degree of freedom number
! - `iColumn` calculated from nodenum and idof depends upon the storageFMT.
!
! Here, jvar, idof, and nodenum is used to calculate the index of
! physical variable jvar and its degree of freedom
!

INTERFACE
MODULE SUBROUTINE csrMat_getBlockColumn8( obj, ivar, jvar, nodenum, &
  & spacecompo, timecompo, value, scale,  addContribution )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: nodenum(:)
  INTEGER( I4B ), INTENT( IN ) :: spacecompo(:)
  INTEGER( I4B ), INTENT( IN ) :: timecompo
  REAL( DFP ), INTENT( INOUT ) :: value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE csrMat_getBlockColumn8
END INTERFACE

INTERFACE getBlockColumn
  MODULE PROCEDURE csrMat_getBlockColumn8
END INTERFACE getBlockColumn
