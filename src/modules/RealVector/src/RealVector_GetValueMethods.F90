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
PUBLIC :: GetValue_

INTERFACE GetValue_
MODULE PROCEDURE obj_GetValue1, obj_GetValue2, obj_GetValue3, obj_GetValue4, &
    obj_GetValue5, obj_GetValue6, obj_GetValue7, obj_GetValue8, &
    obj_GetValue9, obj_GetValue10, obj_GetValue11, obj_GetValue15, &
    obj_GetValue24
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                    GetValue
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
!
!@note
!   We call set method
!@endnote

INTERFACE GetValue
  MODULE SUBROUTINE obj_GetValue1(obj, VALUE, istart, iend, stride)
    TYPE(RealVector_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    TYPE(RealVector_), INTENT(INOUT) :: VALUE
  END SUBROUTINE obj_GetValue1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
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
!
!@note
!   We call set method
!@endnote

INTERFACE GetValue
  MODULE SUBROUTINE obj_GetValue2(obj, dofobj, VALUE, idof)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    TYPE(RealVector_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_GetValue2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
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
  MODULE SUBROUTINE obj_GetValue3(obj, dofobj, VALUE, ivar, idof)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    TYPE(RealVector_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_GetValue3
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
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
  MODULE SUBROUTINE obj_GetValue4(obj, dofobj, VALUE, ivar, &
                                  spaceCompo, timeCompo)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    TYPE(RealVector_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE obj_GetValue4
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
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
  MODULE SUBROUTINE obj_GetValue5(obj, dofobj, idofobj, &
                                  VALUE, dofvalue, idofvalue)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! Real vector whose value is to be extracted
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! DOF for obj
    INTEGER(I4B), INTENT(IN) :: idofobj
    !! idof for obj
    TYPE(RealVector_), INTENT(INOUT) :: VALUE
    !! real vector to be returned
    TYPE(DOF_), INTENT(IN) :: dofvalue
    !! dof for value
    INTEGER(I4B), INTENT(IN) :: idofvalue
    !! idof for value
  END SUBROUTINE obj_GetValue5
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
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
!
!@note
! The size of idofobj and idofvalue should be equal.
!@endnote

INTERFACE GetValue
  MODULE SUBROUTINE obj_GetValue6(obj, dofobj, idofobj, &
                                  VALUE, dofvalue, idofvalue)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! Real vector whose value is to be extracted
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! DOF for obj
    INTEGER(I4B), INTENT(IN) :: idofobj(:)
    !! idof for obj
    TYPE(RealVector_), INTENT(INOUT) :: VALUE
    !! values to be returned
    TYPE(DOF_), INTENT(IN) :: dofvalue
    !! dof for value
    INTEGER(I4B), INTENT(IN) :: idofvalue(:)
    !! idof for value
  END SUBROUTINE obj_GetValue6
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
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
  MODULE SUBROUTINE obj_GetValue7(obj, dofobj, ivarobj, idofobj, &
                                  VALUE, dofvalue, ivarvalue, idofvalue)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object for obj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    !! physical variable for obj
    INTEGER(I4B), INTENT(IN) :: idofobj
    !! local degree of freedom of physical variable for obj
    TYPE(RealVector_), INTENT(INOUT) :: VALUE
    TYPE(DOF_), INTENT(IN) :: dofvalue
    !! degree of freedom object for value
    INTEGER(I4B), INTENT(IN) :: ivarvalue
    !! physical variable for value
    INTEGER(I4B), INTENT(IN) :: idofvalue
    !! local degree of freedom of physical variable for value
  END SUBROUTINE obj_GetValue7
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
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
!
!@note
!   The size of idofobj and idofvalue should be equal.
!@endnote

INTERFACE GetValue
  MODULE SUBROUTINE obj_GetValue8(obj, dofobj, ivarobj, idofobj, &
                                  VALUE, dofvalue, ivarvalue, idofvalue)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! Real vector whose value is to be extracted
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object for obj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    !! physical variable for obj
    INTEGER(I4B), INTENT(IN) :: idofobj(:)
    !! local degree of freedom of physical variable for obj
    TYPE(RealVector_), INTENT(INOUT) :: VALUE
    !! values to be returned
    TYPE(DOF_), INTENT(IN) :: dofvalue
    !! degree of freedom object for value
    INTEGER(I4B), INTENT(IN) :: ivarvalue
    !! physical variable for value
    INTEGER(I4B), INTENT(IN) :: idofvalue(:)
    !! local degree of freedom of physical variable for value
  END SUBROUTINE obj_GetValue8
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
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
  MODULE SUBROUTINE obj_GetValue9(obj, dofobj, ivarobj, &
                    spaceCompoObj, timeCompoObj, VALUE, dofvalue, ivarvalue, &
                                  spaceCompoValue, timeCompoValue)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    !! physical variable for obj
    INTEGER(I4B), INTENT(IN) :: spaceCompoObj
    !! space component for obj
    INTEGER(I4B), INTENT(IN) :: timeCompoObj
    !! time component for obj
    TYPE(RealVector_), INTENT(INOUT) :: VALUE
    !! values to be returned
    TYPE(DOF_), INTENT(IN) :: dofvalue
    !! dof for value
    INTEGER(I4B), INTENT(IN) :: ivarvalue
    !! physical variable for value
    INTEGER(I4B), INTENT(IN) :: spaceCompoValue
    !! space component for value
    INTEGER(I4B), INTENT(IN) :: timeCompoValue
    !! time component for value
  END SUBROUTINE obj_GetValue9
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
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
  MODULE SUBROUTINE obj_GetValue10(obj, dofobj, ivarobj, &
                    spaceCompoObj, timeCompoObj, VALUE, dofvalue, ivarvalue, &
                                   spaceCompoValue, timeCompoValue)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    !! physical variable for obj
    INTEGER(I4B), INTENT(IN) :: spaceCompoObj
    !! space component for obj
    INTEGER(I4B), INTENT(IN) :: timeCompoObj(:)
    !! time component for obj
    TYPE(RealVector_), INTENT(INOUT) :: VALUE
    !! values to be returned
    TYPE(DOF_), INTENT(IN) :: dofvalue
    !! dof value
    INTEGER(I4B), INTENT(IN) :: ivarvalue
    !! physical variable for value
    INTEGER(I4B), INTENT(IN) :: spaceCompoValue
    !! space compoenent for value
    INTEGER(I4B), INTENT(IN) :: timeCompoValue(:)
    !! time component for value
  END SUBROUTINE obj_GetValue10
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                   GetValue
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
  MODULE SUBROUTINE obj_GetValue11(obj, dofobj, ivarobj, &
                    spaceCompoObj, timeCompoObj, VALUE, dofvalue, ivarvalue, &
                                   spaceCompoValue, timeCompoValue)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    !! physical variable for obj
    INTEGER(I4B), INTENT(IN) :: spaceCompoObj(:)
    !! space component for obj
    INTEGER(I4B), INTENT(IN) :: timeCompoObj
    !! time component for obj
    TYPE(RealVector_), INTENT(INOUT) :: VALUE
    !! values to be returned
    TYPE(DOF_), INTENT(IN) :: dofvalue
    !! dof value
    INTEGER(I4B), INTENT(IN) :: ivarvalue
    !! physical variable for value
    INTEGER(I4B), INTENT(IN) :: spaceCompoValue(:)
    !! psace component for value
    INTEGER(I4B), INTENT(IN) :: timeCompoValue
    !! time component for value
  END SUBROUTINE obj_GetValue11
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
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
  MODULE SUBROUTINE obj_GetValue12(obj, dofobj, idof, VALUE, &
                                   storageFMT, nodenum)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj to extract values
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom for obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! global degree of freedom for obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! nodenum
  END SUBROUTINE obj_GetValue12
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
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
  MODULE SUBROUTINE obj_GetValue13(obj, dofobj, idof, VALUE, &
                                   storageFMT)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj to extract values
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! idof for obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! stroage format
  END SUBROUTINE obj_GetValue13
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
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
  MODULE SUBROUTINE obj_GetValue14(obj, dofobj, idof, VALUE, &
                                   force3D)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj to extract values
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! idof for obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    !! values to be returned
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
    !! force 3D
  END SUBROUTINE obj_GetValue14
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE SUBROUTINE obj_GetValue15(obj, dofobj, ivar, idof, &
                                   VALUE, nodenum)
    TYPE(RealVector_), INTENT(IN) :: obj
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
  END SUBROUTINE obj_GetValue15
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE SUBROUTINE obj_GetValue16(obj, dofobj, ivar, idof, &
                                   VALUE, nodenum)
    TYPE(RealVector_), INTENT(IN) :: obj
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
  END SUBROUTINE obj_GetValue16
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE SUBROUTINE obj_GetValue17(obj, dofobj, ivar, VALUE, nodenum)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj whose value is to be extracted
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable for obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
  END SUBROUTINE obj_GetValue17
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                  GetValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE SUBROUTINE obj_GetValue18(obj, dofobj, ivar, spaceCompo, &
                                   timeCompo, VALUE, nodenum)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
  END SUBROUTINE obj_GetValue18
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE SUBROUTINE obj_GetValue19(obj, dofobj, VALUE, idof)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_GetValue19
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                  GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE SUBROUTINE obj_GetValue20(obj, dofobj, VALUE, ivar, idof)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_GetValue20
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue
  MODULE SUBROUTINE obj_GetValue21(obj, dofobj, VALUE, ivar, &
                                   spaceCompo, timeCompo)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE obj_GetValue21
END INTERFACE GetValue

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
  MODULE SUBROUTINE obj_GetValue22(obj, dofobj, idof, VALUE, nodenum)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
  END SUBROUTINE obj_GetValue22
END INTERFACE GetValue

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
  MODULE SUBROUTINE obj_GetValue23(obj, dofobj, idof, VALUE)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
  END SUBROUTINE obj_GetValue23
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 May 2022
! summary: copy a realvector into another realvector

INTERFACE GetValue
  MODULE SUBROUTINE obj_GetValue24(obj, VALUE)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(RealVector_), INTENT(INOUT) :: VALUE
  END SUBROUTINE obj_GetValue24
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                                   GetValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: Returns the values of degrees of freedom in a single vector
!
!# Introduction
!
! This routine is similar to the ob_GetValue12 but it does not allocate
! extra memory for value.

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_12(obj, dofobj, idof, VALUE, &
                                    tsize, storageFMT, nodenum)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! idof for obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total entries written to value
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! nodenum
  END SUBROUTINE obj_GetValue_12
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: Returns the values of degrees of freedom in a single vector
!
!# Introduction
!
! This routine is similar to the ob_GetValue13 but it does not allocate
! extra memory for value.

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_13(obj, dofobj, idof, VALUE, &
                                    tsize, storageFMT)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj to extract values
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! idof for obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total entries written to value
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! stroage format
  END SUBROUTINE obj_GetValue_13
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: Returns the values of degrees of freedom in a single vector
!
!# Introduction
!
! This routine is similar to the ob_GetValue14 but it does not allocate
! extra memory for value.

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_14(obj, dofobj, idof, VALUE, &
                                    nrow, ncol, force3D)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj to extract values
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! idof for obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! values to be returned
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written to value
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
    !! force 3D
  END SUBROUTINE obj_GetValue_14
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_16(obj, dofobj, ivar, idof, &
                                    VALUE, tsize, nodenum)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj whose value is to be extracted
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable for obj
    INTEGER(I4B), INTENT(IN) :: idof
    !! idof for obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size written to value
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
  END SUBROUTINE obj_GetValue_16
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: This function returns a vector of real from [[RealVector_]]
!
!# Introduction
!
!@note
! This routine first computes the IDOF and then
!   This routine calls obj_GetValue_12
!@endnote

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_17(obj, dofobj, ivar, VALUE, &
                                    tsize, nodenum)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj whose value is to be extracted
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable for obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size written to value
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
  END SUBROUTINE obj_GetValue_17
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                  GetValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_18(obj, dofobj, ivar, spaceCompo, &
                                    timeCompo, VALUE, tsize, nodenum)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom for obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable for obj
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component for obj
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component for obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size written to value
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
  END SUBROUTINE obj_GetValue_18
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_19(obj, dofobj, VALUE, tsize, idof)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_GetValue_19
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_20(obj, dofobj, VALUE, tsize, &
                                    ivar, idof)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_GetValue_20
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: This function returns a vector of real from [[RealVector_]]

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_21(obj, dofobj, VALUE, tsize, ivar, &
                                    spaceCompo, timeCompo)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE obj_GetValue_21
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                GetValue_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-25
! summary: Returns the values of degrees of freedom in a single vector
!
!# Introduction
! This subroutine extracts the values from `val` corresponding to
! degrees of freedom specified by `idof(:)` and return it in `V`
!
! - `StorageFMT` can be 'Nodes_FMT' or `DOF_FMT`. It specify the storage
! format of returned vector.

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_22(obj, dofobj, idof, VALUE, &
                                    tsize, nodenum)
    TYPE(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
  END SUBROUTINE obj_GetValue_22
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                  GetValue
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

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_23(obj, dofobj, idof, VALUE, tsize)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj to extract values
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom for obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! idof for obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written to value
  END SUBROUTINE obj_GetValue_23
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                    GetValue
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

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_24(obj, dofobj, idof, VALUE, nrow, ncol, &
                                    storageFMT, nodenum)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj to extract values
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! idof for obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! values to be returned
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written to value
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written to value
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format can be DOF_FMT or Nodes_FMT
    !! if DOF_FMT then nrow size(nodenum) and ncol size(idof)
    !! if Nodes_FMT then nrow is size(idof) and ncol is size(nodenum)
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node numbers
  END SUBROUTINE obj_GetValue_24
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                    GetValue
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

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_25(obj, dofobj, idof, VALUE, nrow, ncol, &
                                    storageFMT)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj to extract values
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! dof for obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    !! idof for obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! values to be returned
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written to value
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written to value
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format can be DOF_FMT or Nodes_FMT
    !! if DOF_FMT then nrow size(nodenum) and ncol size(idof)
    !! if Nodes_FMT then nrow is size(idof) and ncol is size(nodenum)
  END SUBROUTINE obj_GetValue_25
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Get multiple values

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_26(obj, nodenum, VALUE, tsize)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj to extract values
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! index
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size written to value
  END SUBROUTINE obj_GetValue_26
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Get multiple values

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_27(obj, istart, iend, stride, VALUE, tsize)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj to extract values
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! index
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size written to value
  END SUBROUTINE obj_GetValue_27
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Get multiple values

INTERFACE GetValue_
  MODULE SUBROUTINE obj_GetValue_28(obj, istart, iend, stride, VALUE, &
                                tsize, istart_value, iend_value, stride_value)
    TYPE(RealVector_), INTENT(IN) :: obj
    !! obj to extract values
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! index
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size written to value
    INTEGER(I4B), INTENT(IN) :: istart_value, iend_value, stride_value
  END SUBROUTINE obj_GetValue_28
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE RealVector_GetValueMethods
