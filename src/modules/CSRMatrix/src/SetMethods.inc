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

PUBLIC :: Set

!----------------------------------------------------------------------------
!                                                            set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine set the value in sparse matrix
!
!# Introduction
!
! - This subroutine sets the value in [[CSRMatrix_]]
! - Shape( value ) = [SIZE(nodenum)*tdof, SIZE(nodenum)*tdof]
! - Usually `value` denotes the element matrix
! - Symbolic we are performing following task `obj(nodenum, nodenum)=value`

INTERFACE
MODULE PURE SUBROUTINE csrMat_set0( obj, nodenum, value )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value( :, : )
END SUBROUTINE csrMat_set0
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set0
END INTERFACE set

!----------------------------------------------------------------------------
!                                                            set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine set the value in sparse matrix
!
!# Introduction
!
! This subroutine sets the values in sparse matrix.
!
!$$
! obj(Nptrs,Nptrs)=value(:,:)
!$$
!
! - Usually `value(:,:)` represents the element finite element matrix
! - The shape of `value` should be the tdof*size(nodenum), tdof*size(nodenum)
! - `tdof` is the total degree of freedom in obj%csr%dof
!
! - `StorageFMT` denotes the storage format of `value`
! It can be `Nodes_FMT` or `DOF_FMT`
!
! - Usually, element matrix is stored with `DOF_FMT`

INTERFACE
MODULE PURE SUBROUTINE csrMat_set1( obj, nodenum, value, storageFMT )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value( :, : )
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
END SUBROUTINE csrMat_set1
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set1
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Sets all values of sparse matrix to given scalar value
!
!# Introduction
! This routine sets all values of sparse matrix to given value.
! This routine is used to define an assignment operator. Therefore, we can
! call this routine by `obj=value`.

INTERFACE
MODULE PURE SUBROUTINE csrMat_set2( obj, value )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE csrMat_set2
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set2
END INTERFACE set

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE csrMat_set2
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: Sets a single entry of sparse matrix
!
!# Introduction
!
! - This subroutine sets a single entry of sparse matrix.
! - Before using this routine the user should be aware of the storage
! pattern of degree of freedom.
! - However, if total number of degrees of freedom is one then there is not
! need to worry.
!
!@warning
! This routine should be avoided by general user.
!@endwarning

INTERFACE
MODULE PURE SUBROUTINE csrMat_set3( obj, irow, icolumn, value )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: irow
  INTEGER( I4B ), INTENT( IN ) :: icolumn
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE csrMat_set3
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set3
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Sets the specific row and column entry to a given value
!
!# Introduction
!
! - This routine sets the specific row and column entry to a given value.
! - The irow and icolumn index in [[CSRMatrix_]] are calculated by using
! (iNodeNum, iDOF) and (jNodeNum, jDOF), respectively.
! - To do the above task, the routine employs [[DOF_Method:getNodeLoc]] method
! - After computing the irow and icolumn (internally) this routine calls,
! `csrMat_set3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:csrMat_set3]]
!@endnote
!
!@note
! rowdof, coldof are continuously numbered, so if there are two
! or more physical variables, then rowdof and coldof of the second
! or later physical variables will not start from 1.
!@endnote

INTERFACE
MODULE PURE SUBROUTINE csrMat_set4( obj, iNodeNum, jNodeNum, iDOF, &
  & jDOF, value )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNodeNum
    !! row node number
  INTEGER( I4B ), INTENT( IN ) :: jNodeNum
    !! column node number
  INTEGER( I4B ), INTENT( IN ) :: iDOF
    !! row degree of freedom
  INTEGER( I4B ), INTENT( IN ) :: jDOF
    !! col degree of freedom
  REAL( DFP ), INTENT( IN ) :: value
    !! scalar value to be set
END SUBROUTINE csrMat_set4
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set4
END INTERFACE set

!----------------------------------------------------------------------------
!                                                            set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine sets selected values in sparse matrix
!
!# Introduction
!
! This subroutine sets selected values of the sparse matrix to the scalar
! value `value`
!
! This routine corresponds to `obj(nodenum) = value`

INTERFACE
MODULE PURE SUBROUTINE csrMat_set5( obj, nodenum, value )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE csrMat_set5
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set5
END INTERFACE set

!----------------------------------------------------------------------------
!                                                            set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine set the value in sparse matrix
!
!# Introduction
!
! - This subroutine sets the values in block sparse matrix.
! - The storage pattern of both sparse matrix and value
! (the element matrix) should be in `FMT_DOF`.
!
!$$
! obj(Nptrs,Nptrs)=value(:,:)
!$$
!

INTERFACE
MODULE PURE SUBROUTINE csrMat_set6( obj, iNodeNum, jNodeNum, &
  & ivar, jvar, value )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNodeNum( : )
  INTEGER( I4B ), INTENT( IN ) :: jNodeNum( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  REAL( DFP ), INTENT( IN ) :: value( :, : )
END SUBROUTINE csrMat_set6
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set6
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Sets the specific row and column entry to a given value
!
!# Introduction
!
! - This routine sets the specific row and column entry to a given value.
! - The irow and icolumn index in [[CSRMatrix_]] are calculated by using
! (iNodeNum, iDOF) and (jNodeNum, jDOF), respectively.
! - To do the above task, the routine employs [[DOF_Method:getNodeLoc]] method
! - After computing the irow and icolumn (internally) this routine calls,
! `csrMat_set3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:csrMat_set3]]
!@endnote
!
!@note
! rowdof, coldof are continuously numbered, so if there are two
! or more physical variables, then rowdof and coldof of the second
! or later physical variables will not start from 1.
!@endnote

INTERFACE
MODULE PURE SUBROUTINE csrMat_set7( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, iDOF, jDOF, value )
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
    !! scalar value to be set
END SUBROUTINE csrMat_set7
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set7
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Sets the specific row and column entry to a given value

INTERFACE
MODULE PURE SUBROUTINE csrMat_set8( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, iDOF, jDOF, value )
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
    !! scalar value to be set
END SUBROUTINE csrMat_set8
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set8
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Sets the specific row and column entry to a given value
!
!# Introduction
!
! - This routine sets the specific row and column entry to a given value.
! - The irow and icolumn index in [[CSRMatrix_]] are calculated by using
! (iNodeNum, iDOF) and (jNodeNum, jDOF), respectively.
! - To do the above task, the routine employs [[DOF_Method:getNodeLoc]] method
! - After computing the irow and icolumn (internally) this routine calls,
! `csrMat_set3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:csrMat_set3]]
!@endnote
!
!@note
! rowdof, coldof are continuously numbered, so if there are two
! or more physical variables, then rowdof and coldof of the second
! or later physical variables will not start from 1.
!@endnote

INTERFACE
MODULE PURE SUBROUTINE csrMat_set9( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, value )
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
    !! col degree of freedom
  REAL( DFP ), INTENT( IN ) :: value
    !! scalar value to be set
END SUBROUTINE csrMat_set9
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set9
END INTERFACE set

!----------------------------------------------------------------------------
!                                                            set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine set the value in sparse matrix
!
!# Introduction
!
! - This subroutine sets the values in block sparse matrix.
! - The storage pattern of both sparse matrix and value
! (the element matrix) should be in `FMT_DOF`.
!
!$$
! obj(Nptrs,Nptrs)=value(:,:)
!$$
!

INTERFACE
MODULE PURE SUBROUTINE csrMat_set10( obj, iNodeNum, jNodeNum, &
  & ivar, jvar, value )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNodeNum( : )
  INTEGER( I4B ), INTENT( IN ) :: jNodeNum( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE csrMat_set10
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set10
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Sets the specific row and column entry to a given value

INTERFACE
MODULE PURE SUBROUTINE csrMat_set11( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, iDOF, jDOF, value )
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
    !! scalar value to be set
END SUBROUTINE csrMat_set11
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set11
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Sets the specific row and column entry to a given value

INTERFACE
MODULE PURE SUBROUTINE csrMat_set12( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, value )
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
    !! scalar value to be set
END SUBROUTINE csrMat_set12
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set12
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Sets the specific row and column entry to a given value

INTERFACE
MODULE PURE SUBROUTINE csrMat_set13( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, value )
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
    !! scalar value to be set
END SUBROUTINE csrMat_set13
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set13
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	Sets the specific row and column entry to a given value

INTERFACE
MODULE PURE SUBROUTINE csrMat_set14( obj, iNodeNum, jNodeNum, ivar,  &
  & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, value )
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
    !! scalar value to be set
END SUBROUTINE csrMat_set14
END INTERFACE

INTERFACE set
  MODULE PROCEDURE csrMat_set14
END INTERFACE set
