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

MODULE DOF_GetValueMethods
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: DOF_
IMPLICIT NONE

PRIVATE

PUBLIC :: Getvalue
PUBLIC :: Get

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the values of degrees of freedom in a single vector
!
!# Introduction
! This subroutine extracts the values from `val` corresponding to
! degrees of freedom specified by `idof(:)` and return it in `V`
!
! - `StorageFMT` can be 'Nodes_FMT' or `DOF_FMT`. It specify the storage
! format of returned vector.

INTERFACE GetValue
  MODULE PURE SUBROUTINE dof_getvalue1(v, val, obj, idof, storageFMT, &
                                       nodenum)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: v(:)
    !! values to return
    REAL(DFP), INTENT(IN) :: val(:)
    !! values to extract from
    TYPE(DOF_), INTENT(IN) :: obj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! degrees of freedom to extract
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format of returned vector
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node numbers to extract
  END SUBROUTINE dof_getvalue1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the values of degrees of freedom in a 2D array
!
!# Introduction
! This subroutine extracts all the values from `val` corresponding to
! degrees of freedom specified by `idof(:)` and return it in `V(:,:)`
! values in `v(:,:)` are stored in xiJ format.
!
! - Force3D will return a vector in 3D. if there are only two components
! then it will set the third component to 0
!

INTERFACE GetValue
  MODULE PURE SUBROUTINE dof_getvalue2(v, val, obj, idof, force3D)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: v(:, :)
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
  END SUBROUTINE dof_getvalue2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the values of degrees of freedom in a single vector
!
!# Introduction
! This subroutine extracts the values of from `val` corresponding to
! degrees of freedom specified by `idof(:)` and return it in `V`
!
! - `StorageFMT` can be 'Nodes_FMT' or `DOF_FMT`. It specify the storage
! format of returned vector.

INTERFACE GetValue
  MODULE PURE SUBROUTINE dof_getvalue3(v, val, obj, idof, storageFMT)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: v(:)
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    INTEGER(I4B), INTENT(IN) :: storageFMT
  END SUBROUTINE dof_getvalue3
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

INTERFACE Get
  MODULE PURE FUNCTION dof_get1(val, obj, idof, StorageFMT, nodenum, &
                                Force3D) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    INTEGER(I4B), INTENT(IN) :: StorageFMT
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION dof_get1
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

INTERFACE Get
  MODULE PURE FUNCTION dof_get2(val, obj, idof, StorageFMT, &
                                Force3D) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    INTEGER(I4B), INTENT(IN) :: StorageFMT
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION dof_get2
END INTERFACE Get

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DOF_GetValueMethods
