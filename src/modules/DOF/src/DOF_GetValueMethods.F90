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

PUBLIC :: GetValue
PUBLIC :: GetValue_
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
  MODULE PURE SUBROUTINE obj_GetValue1(v, val, obj, idof, storageFMT, &
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
  END SUBROUTINE obj_GetValue1
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
  MODULE PURE SUBROUTINE obj_GetValue2(v, val, obj, idof, force3D)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: v(:, :)
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
  END SUBROUTINE obj_GetValue2
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
  MODULE PURE SUBROUTINE obj_GetValue3(v, val, obj, idof, storageFMT)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: v(:)
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    INTEGER(I4B), INTENT(IN) :: storageFMT
  END SUBROUTINE obj_GetValue3
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

INTERFACE Get
  MODULE PURE FUNCTION obj_get1(val, obj, idof, StorageFMT, nodenum, &
                                Force3D) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    INTEGER(I4B), INTENT(IN) :: StorageFMT
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION obj_get1
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

INTERFACE Get
  MODULE PURE FUNCTION obj_get2(val, obj, idof, StorageFMT, &
                                Force3D) RESULT(ans)
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    INTEGER(I4B), INTENT(IN) :: StorageFMT
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION obj_get2
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                                  GetValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: Returns the values of degrees of freedom in a single vector
!
!# Introduction
!
! This subroutine is same as GetValue1
! but it does not allocate any extra memory

INTERFACE GetValue_
 MODULE PURE SUBROUTINE obj_GetValue_1(v, tsize, val, obj, idof, storageFMT, &
                                        nodenum)
    REAL(DFP), INTENT(INOUT) :: v(:)
    !! values to return
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written in v
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
  END SUBROUTINE obj_GetValue_1
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                  GetValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: Returns the values of degrees of freedom in a 2D array
!
!# Introduction
!
! This subroutine is same as GetValue2 but
! it does not allocate any extra memory

INTERFACE GetValue_
 MODULE PURE SUBROUTINE obj_GetValue_2(v, val, nrow, ncol, obj, idof, force3D)
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! Data to be returned
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in v
    REAL(DFP), INTENT(IN) :: val(:)
    !! values to extract from
    TYPE(DOF_), INTENT(IN) :: obj
    !! degree of freedom object for val
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! degrees of freedom to extract
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
    !! if true then return 3D vector
  END SUBROUTINE obj_GetValue_2
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                  GetValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: Returns the values of degrees of freedom in a single vector
!
!# Introduction
!
! This subroutine is same as GetValue3 but
! it does not allocate any extra memory

INTERFACE GetValue_
  MODULE PURE SUBROUTINE obj_GetValue_3(v, tsize, val, obj, idof, storageFMT)
    REAL(DFP), INTENT(INOUT) :: v(:)
    !! values to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written in v
    REAL(DFP), INTENT(IN) :: val(:)
    !! values to extract from
    TYPE(DOF_), INTENT(IN) :: obj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! degrees of freedom to extract
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! stroage format of returned vector
  END SUBROUTINE obj_GetValue_3
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-26
! summary: Returns the values
!
!# Introduction
!
! This routine performs following operations without extra memory allocation
! index = obj_GetIndex1(obj, nodenum)
! v = val(index)

INTERFACE GetValue_
  MODULE PURE SUBROUTINE obj_GetValue_4(v, tsize, val, obj, nodenum)
    REAL(DFP), INTENT(INOUT) :: v(:)
    !! Values to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Size of data written in v
    REAL(DFP), INTENT(IN) :: val(:)
    !! Values to extract from
    TYPE(DOF_), INTENT(IN) :: obj
    !! Degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! Node number to extract
  END SUBROUTINE obj_GetValue_4
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-26
! summary: Returns the values
!
!# Introduction
!
! This routine performs following operations without extra memory allocation
! index = obj_GetIndex2(obj, nodenum, ivar)
! v = val(index)

INTERFACE GetValue_
  MODULE PURE SUBROUTINE obj_GetValue_5(v, tsize, val, obj, nodenum, ivar)
    REAL(DFP), INTENT(INOUT) :: v(:)
    !! Values to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Size of data written in v
    REAL(DFP), INTENT(IN) :: val(:)
    !! Values to extract from
    TYPE(DOF_), INTENT(IN) :: obj
    !! Degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! Node number to extract
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable numbers
  END SUBROUTINE obj_GetValue_5
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-26
! summary: Returns the values
!
!# Introduction
!
! This routine performs following operations without extra memory allocation
! index = obj_GetIndex4(obj, nodenum)
! v = val(index)

INTERFACE GetValue_
  MODULE PURE SUBROUTINE obj_GetValue_6(v, tsize, val, obj, nodenum)
    REAL(DFP), INTENT(INOUT) :: v(:)
    !! Values to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Size of data written in v
    REAL(DFP), INTENT(IN) :: val(:)
    !! Values to extract from
    TYPE(DOF_), INTENT(IN) :: obj
    !! Degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! Node number to extract
  END SUBROUTINE obj_GetValue_6
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                  GetValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: Returns the values of degrees of freedom in a single vector

INTERFACE GetValue_
  MODULE PURE SUBROUTINE obj_GetValue_7(v, tsize, val, obj, idof, nodenum)
    REAL(DFP), INTENT(INOUT) :: v(:)
    !! values to return
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written in v
    REAL(DFP), INTENT(IN) :: val(:)
    !! values to extract from
    TYPE(DOF_), INTENT(IN) :: obj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: idof
    !! global degrees of freedom to extract
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node numbers to extract
  END SUBROUTINE obj_GetValue_7
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                  GetValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: Returns the values of degrees of freedom in a single vector

INTERFACE GetValue_
  MODULE PURE SUBROUTINE obj_GetValue_8(v, tsize, val, obj, idof, isidof)
    REAL(DFP), INTENT(INOUT) :: v(:)
    !! values to return
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written in v
    REAL(DFP), INTENT(IN) :: val(:)
    !! values to extract from
    TYPE(DOF_), INTENT(IN) :: obj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: idof
    !! global degrees of freedom to extract
    LOGICAL(LGT), INTENT(IN) :: isidof
    !! This variable is not used, it here to create unique interface
    !! otherwise it conflicts with obj_GetValue_4
  END SUBROUTINE obj_GetValue_8
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DOF_GetValueMethods
