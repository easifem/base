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

PUBLIC :: Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 Marach 2021
! summary: This subroutine add contribution

INTERFACE
MODULE PURE SUBROUTINE csrMat_add0( obj, nodenum, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
    !! Node numbers
  REAL( DFP ), INTENT( IN ) :: value( :, : )
    !! Element finite element matrix
  REAL( DFP ), INTENT( IN ) :: scale
    !! Scale is used to scale the Val before adding it to the obj
END SUBROUTINE csrMat_add0
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE csrMat_add0
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 Marach 2021
! summary: This subroutine add contribution

INTERFACE
MODULE PURE SUBROUTINE csrMat_add1( obj, nodenum, value, scale, storageFMT )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
    !! Node numbers
  REAL( DFP ), INTENT( IN ) :: value( :, : )
    !! Element finite element matrix
  REAL( DFP ), INTENT( IN ) :: scale
    !! Scale is used to scale the Val before adding it to the obj
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
    !! Storage format of element finite matrix
END SUBROUTINE csrMat_add1
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE csrMat_add1
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Adds all values of sparse matrix to given scalar value
!
!# Introduction
! This routine adds all values of sparse matrix to given value.
! This routine signifies `obj=obj+scale*value`.

INTERFACE
MODULE PURE SUBROUTINE csrMat_add2( obj, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add2
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE csrMat_add2
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This subroutine adds a single entry of sparse matrix
!
!# Introduction
!
! This subroutine adds a single entry of sparse matrix.
! Before using this subroutien the user should be aware of the storage
! pattern of degree of freedom. However, if total number of degrees of
! freedom is one then there is not need to worry. In my opinion, this routine
! should be avoided by general user.

INTERFACE
MODULE PURE SUBROUTINE csrMat_add3( obj, irow, icolumn, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: irow
  INTEGER( I4B ), INTENT( IN ) :: icolumn
  REAL( DFP ), INTENT( IN ) :: value
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add3
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE csrMat_add3
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: adds the specific row and column entry to a given value
!
!# Introduction
!
! This routine adds the specific row and column entry to a given value.
! The row and column index is calculated by using (iNodeNum, idof) and
! (jNodeNum, jdof), respectively.
! After computing the irow and icolumn (internally) this routine calls,
! `csrMat_add3`.

INTERFACE
MODULE PURE SUBROUTINE csrMat_add4( obj, iNodeNum, jNodeNum, idof, &
  & jdof, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNodeNum
  INTEGER( I4B ), INTENT( IN ) :: jNodeNum
  INTEGER( I4B ), INTENT( IN ) :: idof
  INTEGER( I4B ), INTENT( IN ) :: jdof
  REAL( DFP ), INTENT( IN ) :: value
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add4
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE csrMat_add4
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine add the selected value in sparse matrix

INTERFACE
MODULE PURE SUBROUTINE csrMat_add5( obj, nodenum, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add5
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE csrMat_add5
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine add the value in sparse matrix
!
!# Introduction
!
! - This subroutine adds the values in block sparse matrix.
! - The storage pattern of both sparse matrix and value
! (the element matrix) should be in `FMT_DOF`.
!
!$$
! obj(Nptrs,Nptrs)=value(:,:)
!$$
!

INTERFACE
MODULE PURE SUBROUTINE csrMat_add6( obj, iNodeNum, jNodeNum, &
  & ivar, jvar, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNodeNum( : )
  INTEGER( I4B ), INTENT( IN ) :: jNodeNum( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  REAL( DFP ), INTENT( IN ) :: value( :, : )
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add6
END INTERFACE

INTERFACE add
  MODULE PROCEDURE csrMat_add6
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	adds the specific row and column entry to a given value
!
!# Introduction
!
! - This routine adds the specific row and column entry to a given value.
! - The irow and icolumn index in [[CSRMatrix_]] are calculated by using
! (iNodeNum, iDOF) and (jNodeNum, jDOF), respectively.
! - To do the above task, the routine employs [[DOF_Method:getNodeLoc]] method
! - After computing the irow and icolumn (internally) this routine calls,
! `csrMat_add3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:csrMat_add3]]
!@endnote
!
!@note
! idof, jdof are continuously numbered, so if there are two
! or more physical variables, then idof and jdof of the second
! or later physical variables will not start from 1.
!@endnote

INTERFACE
MODULE PURE SUBROUTINE csrMat_add7( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, iDOF, jDOF, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNodeNum
    !! row node number
  INTEGER( I4B ), INTENT( IN ) :: jNodeNum
    !! column node number
  INTEGER( I4B ), INTENT( IN ) :: ivar
    !!
  INTEGER( I4B ), INTENT( IN ) :: jvar
    !!
  INTEGER( I4B ), INTENT( IN ) :: iDOF
    !! row degree of freedom
  INTEGER( I4B ), INTENT( IN ) :: jDOF
    !! col degree of freedom
  REAL( DFP ), INTENT( IN ) :: value
    !! scalar value to be add
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add7
END INTERFACE

INTERFACE add
  MODULE PROCEDURE csrMat_add7
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	adds the specific row and column entry to a given value

INTERFACE
MODULE PURE SUBROUTINE csrMat_add8( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, iDOF, jDOF, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNodeNum(:)
    !! row node number
  INTEGER( I4B ), INTENT( IN ) :: jNodeNum(:)
    !! column node number
  INTEGER( I4B ), INTENT( IN ) :: ivar
    !!
  INTEGER( I4B ), INTENT( IN ) :: jvar
    !!
  INTEGER( I4B ), INTENT( IN ) :: iDOF
    !! row degree of freedom
  INTEGER( I4B ), INTENT( IN ) :: jDOF
    !! col degree of freedom
  REAL( DFP ), INTENT( IN ) :: value(:,:)
    !! scalar value to be add
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add8
END INTERFACE

INTERFACE add
  MODULE PROCEDURE csrMat_add8
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17/01/2022
! summary: 	adds the specific row and column entry to a given value
!
!# Introduction
!
! - This routine adds the specific row and column entry to a given value.
! - The irow and icolumn index in [[CSRMatrix_]] are calculated by using
! (iNodeNum, iDOF) and (jNodeNum, jDOF), respectively.
! - To do the above task, the routine employs [[DOF_Method:getNodeLoc]] method
! - After computing the irow and icolumn (internally) this routine calls,
! `csrMat_add3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:csrMat_add3]]
!@endnote
!
!@note
! idof, jdof are continuously numbered, so if there are two
! or more physical variables, then idof and jdof of the second
! or later physical variables will not start from 1.
!@endnote

INTERFACE
MODULE PURE SUBROUTINE csrMat_add9( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNodeNum
    !! row node number
  INTEGER( I4B ), INTENT( IN ) :: jNodeNum
    !! column node number
  INTEGER( I4B ), INTENT( IN ) :: ivar
    !!
  INTEGER( I4B ), INTENT( IN ) :: jvar
    !!
  INTEGER( I4B ), INTENT( IN ) :: ispacecompo
  INTEGER( I4B ), INTENT( IN ) :: itimecompo
  INTEGER( I4B ), INTENT( IN ) :: jspacecompo
  INTEGER( I4B ), INTENT( IN ) :: jtimecompo
  REAL( DFP ), INTENT( IN ) :: value
    !! scalar value to be add
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add9
END INTERFACE

INTERFACE add
  MODULE PROCEDURE csrMat_add9
END INTERFACE add


!----------------------------------------------------------------------------
!                                                            add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17/01/2022
! summary: This subroutine add the value in sparse matrix
!
!# Introduction
!
! - This subroutine adds the values in block sparse matrix.
! - The storage pattern of both sparse matrix and value
! (the element matrix) should be in `FMT_DOF`.
!
!$$
! obj(Nptrs,Nptrs)=value(:,:)
!$$
!

INTERFACE
MODULE PURE SUBROUTINE csrMat_add10( obj, iNodeNum, jNodeNum, &
  & ivar, jvar, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNodeNum( : )
  INTEGER( I4B ), INTENT( IN ) :: jNodeNum( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  REAL( DFP ), INTENT( IN ) :: value
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add10
END INTERFACE

INTERFACE add
  MODULE PROCEDURE csrMat_add10
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17/01/2022
! summary: 	adds the specific row and column entry to a given value

INTERFACE
MODULE PURE SUBROUTINE csrMat_add11( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, iDOF, jDOF, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNodeNum(:)
    !! row node number
  INTEGER( I4B ), INTENT( IN ) :: jNodeNum(:)
    !! column node number
  INTEGER( I4B ), INTENT( IN ) :: ivar
    !!
  INTEGER( I4B ), INTENT( IN ) :: jvar
    !!
  INTEGER( I4B ), INTENT( IN ) :: iDOF
    !! row degree of freedom
  INTEGER( I4B ), INTENT( IN ) :: jDOF
    !! col degree of freedom
  REAL( DFP ), INTENT( IN ) :: value
    !! scalar value to be add
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add11
END INTERFACE

INTERFACE add
  MODULE PROCEDURE csrMat_add11
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17/01/2022
! summary: 	adds the specific row and column entry to a given value

INTERFACE
MODULE PURE SUBROUTINE csrMat_add12( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNodeNum(:)
    !! row node number
  INTEGER( I4B ), INTENT( IN ) :: jNodeNum(:)
    !! column node number
  INTEGER( I4B ), INTENT( IN ) :: ivar
    !!
  INTEGER( I4B ), INTENT( IN ) :: jvar
    !!
  INTEGER( I4B ), INTENT( IN ) :: ispacecompo
  INTEGER( I4B ), INTENT( IN ) :: itimecompo
  INTEGER( I4B ), INTENT( IN ) :: jspacecompo
  INTEGER( I4B ), INTENT( IN ) :: jtimecompo
  REAL( DFP ), INTENT( IN ) :: value
    !! scalar value to be add
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add12
END INTERFACE

INTERFACE add
  MODULE PROCEDURE csrMat_add12
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17/01/2022
! summary: 	adds the specific row and column entry to a given value

INTERFACE
MODULE PURE SUBROUTINE csrMat_add13( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNodeNum(:)
    !! row node number
  INTEGER( I4B ), INTENT( IN ) :: jNodeNum(:)
    !! column node number
  INTEGER( I4B ), INTENT( IN ) :: ivar
    !!
  INTEGER( I4B ), INTENT( IN ) :: jvar
    !!
  INTEGER( I4B ), INTENT( IN ) :: ispacecompo
  INTEGER( I4B ), INTENT( IN ) :: itimecompo(:)
  INTEGER( I4B ), INTENT( IN ) :: jspacecompo
  INTEGER( I4B ), INTENT( IN ) :: jtimecompo(:)
  REAL( DFP ), INTENT( IN ) :: value
    !! scalar value to be add
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add13
END INTERFACE

INTERFACE add
  MODULE PROCEDURE csrMat_add13
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17/01/2022
! summary: 	adds the specific row and column entry to a given value

INTERFACE
MODULE PURE SUBROUTINE csrMat_add14( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, value, scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNodeNum(:)
    !! row node number
  INTEGER( I4B ), INTENT( IN ) :: jNodeNum(:)
    !! column node number
  INTEGER( I4B ), INTENT( IN ) :: ivar
    !!
  INTEGER( I4B ), INTENT( IN ) :: jvar
    !!
  INTEGER( I4B ), INTENT( IN ) :: ispacecompo(:)
  INTEGER( I4B ), INTENT( IN ) :: itimecompo
  INTEGER( I4B ), INTENT( IN ) :: jspacecompo(:)
  INTEGER( I4B ), INTENT( IN ) :: jtimecompo
  REAL( DFP ), INTENT( IN ) :: value
    !! scalar value to be add
  REAL( DFP ), INTENT( IN ) :: scale
END SUBROUTINE csrMat_add14
END INTERFACE

INTERFACE add
  MODULE PROCEDURE csrMat_add14
END INTERFACE add