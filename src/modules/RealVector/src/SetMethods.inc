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

PUBLIC :: Set

!----------------------------------------------------------------------------
!                                                            set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set all values to given scalar

INTERFACE
MODULE SUBROUTINE realVec_set1( obj,  value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE realVec_set1
END INTERFACE

INTERFACE Set
  MODULE PROCEDURE realVec_set1
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set all values by given vector

INTERFACE
MODULE SUBROUTINE realVec_set2( obj,  value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE realVec_set2
END INTERFACE

INTERFACE Set
  MODULE PROCEDURE realVec_set2
END INTERFACE Set


!----------------------------------------------------------------------------
!                                                            set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Jan 2022
! summary: set selected values

INTERFACE
MODULE SUBROUTINE realVec_set3( obj, nodenum, value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE realVec_set3
END INTERFACE

INTERFACE Set
  MODULE PROCEDURE realVec_set3
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set4( obj, nodenum, value )
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE realvec_set4
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set4
END INTERFACE set

!----------------------------------------------------------------------------
!                                                            set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Jan 2022
! summary: set selected values

INTERFACE
MODULE SUBROUTINE realVec_set5( obj, nodenum, value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE realVec_set5
END INTERFACE

INTERFACE Set
  MODULE PROCEDURE realVec_set5
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                              set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set range of values to a scalar

INTERFACE
MODULE SUBROUTINE realVec_set6( obj,  istart, iend, stride, value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE realVec_set6
END INTERFACE

INTERFACE Set
  MODULE PROCEDURE realVec_set6
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set range of values to a vector

INTERFACE
MODULE SUBROUTINE realVec_set7( obj, istart, iend, stride, value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE realVec_set7
END INTERFACE

INTERFACE Set
  MODULE PROCEDURE realVec_set7
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set8(obj, dofobj, nodenum, value, conversion)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: conversion( 1 )
END SUBROUTINE realvec_set8
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set8
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set9(obj, dofobj, nodenum, value)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE realvec_set9
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set9
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set10( obj, dofobj, nodenum, value, idof )
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: idof
END SUBROUTINE realvec_set10
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set10
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set11( obj, dofobj, nodenum, value, idof )
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: idof
END SUBROUTINE realvec_set11
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set11
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set12( obj, dofobj, nodenum, value, ivar, idof)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: idof
END SUBROUTINE realvec_set12
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set12
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set13( obj, dofobj, nodenum, value, ivar, idof)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: idof
END SUBROUTINE realvec_set13
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set13
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set14( obj, dofobj, nodenum, value, ivar, &
  & spacecompo, timecompo)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo
END SUBROUTINE realvec_set14
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set14
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set15( obj, dofobj, nodenum, value, ivar, &
  & spacecompo, timecompo)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo
END SUBROUTINE realvec_set15
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set15
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set16( obj, dofobj, nodenum, value, ivar, &
  & spacecompo, timecompo)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo(:)
END SUBROUTINE realvec_set16
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set16
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set17( obj, dofobj, nodenum, value, ivar, &
  & spacecompo, timecompo)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo(:)
END SUBROUTINE realvec_set17
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set17
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set18( obj, dofobj, nodenum, value, ivar, &
  & spacecompo, timecompo)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spacecompo(:)
  INTEGER( I4B ), INTENT( IN ) :: timecompo
END SUBROUTINE realvec_set18
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set18
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set19( obj, dofobj, nodenum, value, ivar, &
  & spacecompo, timecompo)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spacecompo(:)
  INTEGER( I4B ), INTENT( IN ) :: timecompo
END SUBROUTINE realvec_set19
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set19
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set20(obj, dofobj, nodenum, value)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE realvec_set20
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set20
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set21(obj, dofobj, nodenum, value, idof)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: idof
END SUBROUTINE realvec_set21
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set21
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set22(obj, dofobj, nodenum, value, ivar, idof)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: idof
END SUBROUTINE realvec_set22
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set22
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set23(obj, dofobj, nodenum, value, ivar, &
  & spacecompo, timecompo)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo
END SUBROUTINE realvec_set23
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set23
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set24(obj, dofobj, nodenum, value, ivar, &
  & spacecompo, timecompo)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo(:)
END SUBROUTINE realvec_set24
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set24
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
MODULE PURE SUBROUTINE realvec_set25(obj, dofobj, nodenum, value, ivar, &
  & spacecompo, timecompo)
  TYPE( Realvector_ ), INTENT( INOUT ) :: obj
  CLASS( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spacecompo(:)
  INTEGER( I4B ), INTENT( IN ) :: timecompo
END SUBROUTINE realvec_set25
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realvec_set25
END INTERFACE set

!----------------------------------------------------------------------------
!                                                            set@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 June 2022
! summary: obj1=obj2

INTERFACE
MODULE PURE SUBROUTINE realVec_set26( obj, value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  CLASS( RealVector_ ), INTENT( IN ) :: value
END SUBROUTINE realVec_set26
END INTERFACE

INTERFACE set
  MODULE PROCEDURE realVec_set26
END INTERFACE set