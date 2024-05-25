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

MODULE RealVector_GetValueMethods
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: DOF_, RealVector_

IMPLICIT NONE
PRIVATE

PUBLIC :: GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Jan 2022
! summary: Returns a vector of real from [[RealVector_]]
!
!# Introduction
!
! This routine returns a RealVector from a subset of another
! RealVector.
!
! Both obj and value should be allocated.

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_getvalue1(obj, VALUE, istart, iend, stride)
    CLASS(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    CLASS(RealVector_), INTENT(INOUT) :: VALUE
  END SUBROUTINE obj_getvalue1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Jan 2022
! summary: Returns a vector of real from [[RealVector_]]
!
!# Introduction
!
! This routine returns a RealVector from a subset of another
! RealVector.
!
! Both obj and value should be allocated.

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_getvalue2(obj, dofobj, VALUE, idof)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    CLASS(RealVector_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_getvalue2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Jan 2022
! summary:         Returns a vector of real from [[RealVector_]]
!
!# Introduction
!
! This routine returns a RealVector from a subset of another
! RealVector.
!
! Both obj and value should be allocated.

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_getvalue3(obj, dofobj, VALUE, ivar, idof)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    CLASS(RealVector_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_getvalue3
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Jan 2022
! summary:         Returns a vector of real from [[RealVector_]]
!
!# Introduction
!
! This routine returns a RealVector from a subset of another
! RealVector.
!
! Both obj and value should be allocated.

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_getvalue4(obj, dofobj, VALUE, ivar, &
                                       spacecompo, timecompo)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    CLASS(RealVector_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_getvalue4
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Jan 2022
! summary: Returns a vector of real from [[RealVector_]]
!
!# Introduction
!
! This routine returns a RealVector from a subset of another
! RealVector.
!
! Both obj and value should be allocated.

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_getvalue5(obj, dofobj, idofobj, &
                                       VALUE, dofvalue, idofvalue)
    CLASS(RealVector_), INTENT(IN) :: obj
    !! Real vector whose value is to be extracted
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! DOF for obj
    INTEGER(I4B), INTENT(IN) :: idofobj
    !! idof for obj
    CLASS(RealVector_), INTENT(INOUT) :: VALUE
    !! real vector to be returned
    TYPE(DOF_), INTENT(IN) :: dofvalue
    !! dof for value
    INTEGER(I4B), INTENT(IN) :: idofvalue
    !! idof for value
  END SUBROUTINE obj_getvalue5
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Jan 2022
! summary:         Returns a vector of real from [[RealVector_]]
!
!# Introduction
!
! This routine returns a RealVector from a subset of another
! RealVector.
!
! Both obj and value should be allocated.
!
!@note
! The size of idofobj and idofvalue should be equal.
!@endnote

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_getvalue6(obj, dofobj, idofobj, &
                                       VALUE, dofvalue, idofvalue)
    CLASS(RealVector_), INTENT(IN) :: obj
    !! Real vector whose value is to be extracted
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! DOF for obj
    INTEGER(I4B), INTENT(IN) :: idofobj(:)
    !! idof for obj
    CLASS(RealVector_), INTENT(INOUT) :: VALUE
    !! values to be returned
    TYPE(DOF_), INTENT(IN) :: dofvalue
    !! dof for value
    INTEGER(I4B), INTENT(IN) :: idofvalue(:)
    !! idof for value
  END SUBROUTINE obj_getvalue6
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Jan 2022
! summary:         Returns a vector of real from [[RealVector_]]
!
!# Introduction
!
! This routine returns a RealVector from a subset of another
! RealVector.
!
! Both obj and value should be allocated.

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_getvalue7(obj, dofobj, ivarobj, idofobj, &
                                       VALUE, dofvalue, ivarvalue, idofvalue)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! DOF for obj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    !! physical variable for obj
    INTEGER(I4B), INTENT(IN) :: idofobj
    !! idof for obj
    CLASS(RealVector_), INTENT(INOUT) :: VALUE
    TYPE(DOF_), INTENT(IN) :: dofvalue
    INTEGER(I4B), INTENT(IN) :: ivarvalue
    INTEGER(I4B), INTENT(IN) :: idofvalue
  END SUBROUTINE obj_getvalue7
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Jan 2022
! summary:         Returns a vector of real from [[RealVector_]]
!
!# Introduction
!
! This routine returns a RealVector from a subset of another
! RealVector.
!
! Both obj and value should be allocated.

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_getvalue8(obj, dofobj, ivarobj, idofobj, &
                                       VALUE, dofvalue, ivarvalue, idofvalue)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    INTEGER(I4B), INTENT(IN) :: idofobj(:)
    CLASS(RealVector_), INTENT(INOUT) :: VALUE
    TYPE(DOF_), INTENT(IN) :: dofvalue
    INTEGER(I4B), INTENT(IN) :: ivarvalue
    INTEGER(I4B), INTENT(IN) :: idofvalue(:)
  END SUBROUTINE obj_getvalue8
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Jan 2022
! summary:         Returns a vector of real from [[RealVector_]]
!
!# Introduction
!
! This routine returns a RealVector from a subset of another
! RealVector.
!
! Both obj and value should be allocated.

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_getvalue9(obj, dofobj, ivarobj, &
                    spacecompoobj, timecompoobj, VALUE, dofvalue, ivarvalue, &
                                       spacecompovalue, timecompovalue)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    !! physical variable for obj
    INTEGER(I4B), INTENT(IN) :: spacecompoobj
    !! space component for obj
    INTEGER(I4B), INTENT(IN) :: timecompoobj
    !! time component for obj
    CLASS(RealVector_), INTENT(INOUT) :: VALUE
    !! values to be returned
    TYPE(DOF_), INTENT(IN) :: dofvalue
    !! dof for value
    INTEGER(I4B), INTENT(IN) :: ivarvalue
    !! physical variable for value
    INTEGER(I4B), INTENT(IN) :: spacecompovalue
    !! space component for value
    INTEGER(I4B), INTENT(IN) :: timecompovalue
    !! time component for value
  END SUBROUTINE obj_getvalue9
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Jan 2022
! summary:         Returns a vector of real from [[RealVector_]]
!
!# Introduction
!
! This routine returns a RealVector from a subset of another
! RealVector.
!
! Both obj and value should be allocated.

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_getvalue10(obj, dofobj, ivarobj, &
                    spacecompoobj, timecompoobj, VALUE, dofvalue, ivarvalue, &
                                        spacecompovalue, timecompovalue)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    !! physical variable for obj
    INTEGER(I4B), INTENT(IN) :: spacecompoobj
    !! space component for obj
    INTEGER(I4B), INTENT(IN) :: timecompoobj(:)
    !! time component for obj
    CLASS(RealVector_), INTENT(INOUT) :: VALUE
    !! values to be returned
    TYPE(DOF_), INTENT(IN) :: dofvalue
    !! dof value
    INTEGER(I4B), INTENT(IN) :: ivarvalue
    !! physical variable for value
    INTEGER(I4B), INTENT(IN) :: spacecompovalue
    !! space compoenent for value
    INTEGER(I4B), INTENT(IN) :: timecompovalue(:)
    !! time component for value
  END SUBROUTINE obj_getvalue10
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Jan 2022
! summary:         Returns a vector of real from [[RealVector_]]
!
!# Introduction
!
! This routine returns a RealVector from a subset of another
! RealVector.
!
! Both obj and value should be allocated.

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_getvalue11(obj, dofobj, ivarobj, &
                    spacecompoobj, timecompoobj, VALUE, dofvalue, ivarvalue, &
                                        spacecompovalue, timecompovalue)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    !! physical variable for obj
    INTEGER(I4B), INTENT(IN) :: spacecompoobj(:)
    !! space component for obj
    INTEGER(I4B), INTENT(IN) :: timecompoobj
    !! time component for obj
    CLASS(RealVector_), INTENT(INOUT) :: VALUE
    !! values to be returned
    TYPE(DOF_), INTENT(IN) :: dofvalue
    !! dof value
    INTEGER(I4B), INTENT(IN) :: ivarvalue
    !! physical variable for value
    INTEGER(I4B), INTENT(IN) :: spacecompovalue(:)
    !! psace component for value
    INTEGER(I4B), INTENT(IN) :: timecompovalue
    !! time component for value
  END SUBROUTINE obj_getvalue11
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the values of degrees of freedom in a single vector
!
!# Introduction
!
! This subroutine extracts the values from `val` corresponding to
! degrees of freedom specified by `idof(:)` and return it in `V`
!
! - `StorageFMT` can be 'Nodes_FMT' or `DOF_FMT`. It specify the storage
! format of returned vector.

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_getvalue12(obj, dofobj, idof, VALUE, &
                                        storageFMT, nodenum)
    CLASS(RealVector_), INTENT(IN) :: obj
    !! obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! idof for obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! nodenum
  END SUBROUTINE obj_getvalue12
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
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
  MODULE PURE SUBROUTINE obj_getvalue13(obj, dofobj, idof, VALUE, &
                                        storageFMT)
    CLASS(RealVector_), INTENT(IN) :: obj
    !! obj to extract values
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! idof for obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! stroage format
  END SUBROUTINE obj_getvalue13
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
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
  MODULE PURE SUBROUTINE obj_getvalue14(obj, dofobj, idof, VALUE, &
                                        force3D)
    CLASS(RealVector_), INTENT(IN) :: obj
    !! obj to extract values
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! idof for obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    !! values to be returned
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
    !! force 3D
  END SUBROUTINE obj_getvalue14
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   20 Jan 2022
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE PURE SUBROUTINE realVec_getvalue15(obj, dofobj, ivar, idof, &
                                            VALUE, nodenum)
    CLASS(RealVector_), INTENT(IN) :: obj
    !! obj whose value is to be extracted
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable for obj
    INTEGER(I4B), INTENT(IN) :: idof
    !! idof for obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! values to be returned
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node number
  END SUBROUTINE realVec_getvalue15
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         20 Jan 2022
! summary:         This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE PURE SUBROUTINE realVec_getvalue16(obj, dofobj, ivar, idof, &
                                            VALUE, nodenum)
    CLASS(RealVector_), INTENT(IN) :: obj
    !! obj whose value is to be extracted
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable for obj
    INTEGER(I4B), INTENT(IN) :: idof
    !! idof for obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
  END SUBROUTINE realVec_getvalue16
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         20 Jan 2022
! summary:         This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE PURE SUBROUTINE realVec_getvalue17(obj, dofobj, ivar, VALUE, nodenum)
    CLASS(RealVector_), INTENT(IN) :: obj
    !! obj whose value is to be extracted
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable for obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
  END SUBROUTINE realVec_getvalue17
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE PURE SUBROUTINE realVec_getvalue18(obj, dofobj, ivar, spacecompo, &
                                            timecompo, VALUE, nodenum)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
  END SUBROUTINE realVec_getvalue18
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         20 Jan 2022
! summary:         This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE PURE SUBROUTINE realVec_getvalue19(obj, dofobj, VALUE, idof)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE realVec_getvalue19
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         20 Jan 2022
! summary:         This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE PURE SUBROUTINE realVec_getvalue20(obj, dofobj, VALUE, ivar, idof)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE realVec_getvalue20
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         20 Jan 2022
! summary:         This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE PURE SUBROUTINE realVec_getvalue21(obj, dofobj, VALUE, ivar, &
                                            spacecompo, timecompo)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE realVec_getvalue21
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
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
  MODULE PURE SUBROUTINE obj_getvalue22(obj, dofobj, idof, VALUE, nodenum)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
  END SUBROUTINE obj_getvalue22
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
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
  MODULE PURE SUBROUTINE obj_getvalue23(obj, dofobj, idof, VALUE)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
  END SUBROUTINE obj_getvalue23
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   getvalue@GetValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 May 2022
! summary: copy a realvector into another realvector

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_getvalue24(obj, VALUE)
    CLASS(RealVector_), INTENT(IN) :: obj
    CLASS(RealVector_), INTENT(INOUT) :: VALUE
  END SUBROUTINE obj_getvalue24
END INTERFACE GetValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE RealVector_GetValueMethods
