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

MODULE CSRMatrix_SetBlockRowMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_


PUBLIC :: SetBlockRow

!----------------------------------------------------------------------------
!                                                     setBlockRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_setBlockRow1( obj, jvar, irow, value )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: irow
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE csrMat_setBlockRow1
END INTERFACE

INTERFACE setBlockRow
  MODULE PROCEDURE csrMat_setBlockRow1
END INTERFACE setBlockRow

!----------------------------------------------------------------------------
!                                                     setBlockRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_setBlockRow2( obj, jvar, irow, value )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: irow
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE csrMat_setBlockRow2
END INTERFACE

INTERFACE setBlockRow
  MODULE PROCEDURE csrMat_setBlockRow2
END INTERFACE setBlockRow

!----------------------------------------------------------------------------
!                                                     setBlockRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_setBlockRow3( obj, ivar, jvar, nodenum, idof, &
  & value )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE csrMat_setBlockRow3
END INTERFACE

INTERFACE setBlockRow
  MODULE PROCEDURE csrMat_setBlockRow3
END INTERFACE setBlockRow

!----------------------------------------------------------------------------
!                                                     setBlockRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_setBlockRow4( obj, ivar, jvar, nodenum, idof, &
  & value )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), INTENT( IN ) :: value(:)
END SUBROUTINE csrMat_setBlockRow4
END INTERFACE

INTERFACE setBlockRow
  MODULE PROCEDURE csrMat_setBlockRow4
END INTERFACE setBlockRow

!----------------------------------------------------------------------------
!                                                     setBlockRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_setBlockRow5( obj, ivar, jvar, nodenum, spacecompo,&
  & timecompo, value )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE csrMat_setBlockRow5
END INTERFACE

INTERFACE setBlockRow
  MODULE PROCEDURE csrMat_setBlockRow5
END INTERFACE setBlockRow

!----------------------------------------------------------------------------
!                                                     setBlockRow@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine sets the Column of a sparse matrix

INTERFACE
MODULE SUBROUTINE csrMat_setBlockRow6( obj, ivar, jvar, nodenum, spacecompo,&
  & timecompo, value )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo
  REAL( DFP ), INTENT( IN ) :: value(:)
END SUBROUTINE csrMat_setBlockRow6
END INTERFACE

INTERFACE setBlockRow
  MODULE PROCEDURE csrMat_setBlockRow6
END INTERFACE setBlockRow

END MODULE CSRMatrix_SetBlockRowMethods
