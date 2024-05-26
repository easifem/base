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

MODULE DOF_GetMethods
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: DOF_
IMPLICIT NONE
PRIVATE

PUBLIC :: OPERATOR(.DOFStartIndex.)
PUBLIC :: OPERATOR(.DOFEndIndex.)
PUBLIC :: OPERATOR(.tNodes.)
PUBLIC :: OPERATOR(.tNames.)
PUBLIC :: OPERATOR(.tDOF.)
PUBLIC :: OPERATOR(.tspacecomponents.)
PUBLIC :: OPERATOR(.spacecomponents.)
PUBLIC :: OPERATOR(.timecomponents.)
PUBLIC :: OPERATOR(.ttimecomponents.)
PUBLIC :: OPERATOR(.EQ.)
PUBLIC :: OPERATOR(.NE.)
PUBLIC :: OPERATOR(.Names.)
PUBLIC :: GetIDOF
PUBLIC :: SIZE
PUBLIC :: GetNodeLoc
PUBLIC :: GetIndex

!----------------------------------------------------------------------------
!                                                  DOFStartIndex@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: returns obj%map( ivar, 5 )

INTERFACE
  MODULE PURE FUNCTION obj_DOFStartIndex(obj, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B) :: ans
  END FUNCTION obj_DOFStartIndex
END INTERFACE

INTERFACE OPERATOR(.DOFStartIndex.)
  MODULE PROCEDURE obj_DOFStartIndex
END INTERFACE OPERATOR(.DOFStartIndex.)

!----------------------------------------------------------------------------
!                                                  DOFEndIndex@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: returns obj%map( ivar+1, 5 ) - 1

INTERFACE OPERATOR(.DOFEndIndex.)
  MODULE PURE FUNCTION obj_DOFEndIndex(obj, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B) :: ans
  END FUNCTION obj_DOFEndIndex
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tNodes@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the total length of the vector

INTERFACE Size
  MODULE PURE FUNCTION obj_tNodes1(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_tNodes1
END INTERFACE Size

INTERFACE OPERATOR(.tNodes.)
  MODULE PROCEDURE obj_tNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tNodes@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This function returns the total number of nodes
!
!# Introduction
!
! This function returns the total number of nodes for a given degree of
! freedom number
! idof should be lesser than the total degree of freedom

INTERFACE Size
  MODULE PURE FUNCTION obj_tNodes2(obj, idof) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B) :: ans
  END FUNCTION obj_tNodes2
END INTERFACE Size

INTERFACE OPERATOR(.tNodes.)
  MODULE PROCEDURE obj_tNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tNodes@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This function returns the total number of nodes
!
!# Introduction
!
! This function returns the total number of nodes for a given degree of
! freedom number
! idof should be lesser than the total degree of freedom

INTERFACE Size
  MODULE PURE FUNCTION obj_tNodes3(obj, varname) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: varname
    INTEGER(I4B) :: ans
  END FUNCTION obj_tNodes3
END INTERFACE Size

INTERFACE OPERATOR(.tNodes.)
  MODULE PROCEDURE obj_tNodes3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tNodes@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This function returns the total number of nodes
!
!# Introduction
!
! This function returns the total number of nodes for a given degree of
! freedom number
! idof should be lesser than the total degree of freedom

INTERFACE Size
  MODULE PURE FUNCTION obj_tNodes4(obj, idof) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    INTEGER(I4B) :: ans
  END FUNCTION obj_tNodes4
END INTERFACE Size

INTERFACE OPERATOR(.tNodes.)
  MODULE PROCEDURE obj_tNodes4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             tDOF@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This function returns the total number of degree of freedom

INTERFACE OPERATOR(.tDOF.)
  MODULE PURE FUNCTION obj_tdof1(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_tdof1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This subroutine returns the total number of degrees of freedom
!
!# Introduction
! This function returns the total number of degrees of freedom in a
! physical variable.
! The physical variable is specified by using its name.

INTERFACE OPERATOR(.tDOF.)
  MODULE PURE FUNCTION obj_tdof2(obj, Name) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    CHARACTER(1), INTENT(IN) :: Name
    INTEGER(I4B) :: ans
  END FUNCTION obj_tdof2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This subroutine returns the total number of degrees of freedom
!
!# Introduction
! This function returns the total number of degrees of freedom in a
! physical variable.
! The physical variable is specified by using its name.

INTERFACE OPERATOR(.tDOF.)
  MODULE PURE FUNCTION obj_tdof3(obj, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B) :: ans
  END FUNCTION obj_tdof3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tNames@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the total number of names in dof object

INTERFACE OPERATOR(.tNames.)
  MODULE PURE FUNCTION obj_tNames(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_tNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Names@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the name of all physical variables stored in obj

INTERFACE OPERATOR(.Names.)
  MODULE PURE FUNCTION obj_names1(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    CHARACTER(1), ALLOCATABLE :: ans(:)
  END FUNCTION obj_names1
END INTERFACE OPERATOR(.Names.)

!----------------------------------------------------------------------------
!                                                           Names@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This function returns the name of a physical variable
!
!# Introduction
!
! This function returns the name of a physical variable
! The physical variable is given by its number ii, i.e., the first, second,
! third, and so on, physical variable.

INTERFACE OPERATOR(.Names.)
  MODULE PURE FUNCTION obj_names2(obj, ii) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ii
    CHARACTER(1) :: ans
  END FUNCTION obj_names2
END INTERFACE OPERATOR(.Names.)

!----------------------------------------------------------------------------
!                                                     NameToIndex@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Return the index of a physical variable

INTERFACE
  MODULE PURE FUNCTION NameToIndex(obj, Name) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    CHARACTER(1), INTENT(IN) :: Name
    INTEGER(I4B) :: ans
  END FUNCTION NameToIndex
END INTERFACE

!----------------------------------------------------------------------------
!                                                tspacecomponents@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the total physical variable which have space-compo

INTERFACE OPERATOR(.tspacecomponents.)
  MODULE PURE FUNCTION obj_tspacecomponents(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_tspacecomponents
END INTERFACE OPERATOR(.tspacecomponents.)

!----------------------------------------------------------------------------
!                                                spacecomponents@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the space components of each physical vars

INTERFACE OPERATOR(.spacecomponents.)
  MODULE PURE FUNCTION obj_spacecomponents1(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_spacecomponents1
END INTERFACE OPERATOR(.spacecomponents.)

!----------------------------------------------------------------------------
!                                                spacecomponents@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the space component of a given physical vars

INTERFACE OPERATOR(.spacecomponents.)
  MODULE PURE FUNCTION obj_spacecomponents2(obj, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B) :: ans
  END FUNCTION obj_spacecomponents2
END INTERFACE OPERATOR(.spacecomponents.)

!----------------------------------------------------------------------------
!                                                ttimecomponents@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the total physical var which has time compo

INTERFACE OPERATOR(.ttimecomponents.)
  MODULE PURE FUNCTION obj_ttimecomponents(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_ttimecomponents
END INTERFACE OPERATOR(.ttimecomponents.)

!----------------------------------------------------------------------------
!                                                timecomponents@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the timecompo

INTERFACE OPERATOR(.timecomponents.)
  MODULE PURE FUNCTION obj_timecomponents1(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_timecomponents1
END INTERFACE OPERATOR(.timecomponents.)

!----------------------------------------------------------------------------
!                                                timecomponents@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the timecompo

INTERFACE OPERATOR(.timecomponents.)
  MODULE PURE FUNCTION obj_timecomponents2(obj, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B) :: ans
  END FUNCTION obj_timecomponents2
END INTERFACE OPERATOR(.timecomponents.)

!----------------------------------------------------------------------------
!                                                               EQ@GetMethod
!----------------------------------------------------------------------------

INTERFACE OPERATOR(.EQ.)
  MODULE PURE FUNCTION obj_isEqual(obj1, obj2) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj1
    TYPE(DOF_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isEqual
END INTERFACE OPERATOR(.EQ.)

!----------------------------------------------------------------------------
!                                                               NE@GetMethod
!----------------------------------------------------------------------------

INTERFACE OPERATOR(.NE.)
  MODULE PURE FUNCTION obj_isNE(obj1, obj2) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj1
    TYPE(DOF_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isNE
END INTERFACE OPERATOR(.NE.)

!----------------------------------------------------------------------------
!                                                        GetIDOF@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-26
! summary: Get the idof from spacecompo, timecompo, tsapcecompo

INTERFACE GetIDOF
  MODULE PURE FUNCTION obj_GetIDOF1(spacecompo, timecompo, tspacecompo) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component
    INTEGER(I4B), INTENT(IN) :: tspacecompo
    !! total space component
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetIDOF1
END INTERFACE GetIDOF

!----------------------------------------------------------------------------
!                                                        GetIDOF@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-26
! summary: Get idof of a physical variable from space-time components

INTERFACE GetIDOF
  MODULE PURE FUNCTION obj_GetIDOF2(obj, ivar, spacecompo, timecompo) &
    RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of physical variable
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of physical variable
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetIDOF2
END INTERFACE GetIDOF

!----------------------------------------------------------------------------
!                                                        GetIDOF@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-26
! summary: Get idof of physical variable from space and time components

INTERFACE GetIDOF
  MODULE PURE FUNCTION obj_GetIDOF3(obj, ivar, spacecompo, timecompo) &
    RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of physical variable
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time components of physical variable
    INTEGER(I4B) :: ans(SIZE(timecompo))
    !! idof of each time component
  END FUNCTION obj_GetIDOF3
END INTERFACE GetIDOF

!----------------------------------------------------------------------------
!                                                        GetIDOF@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-26
! summary: Get idof of physical variable from space and time components

INTERFACE GetIDOF
  MODULE PURE FUNCTION obj_GetIDOF4(obj, ivar, spacecompo, timecompo) &
    RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! several space components of physical variable
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! a time component of a physical variable
    INTEGER(I4B) :: ans(SIZE(spacecompo))
    !! idof of each space component
  END FUNCTION obj_GetIDOF4
END INTERFACE GetIDOF

!----------------------------------------------------------------------------
!                                                        GetIDOF@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-26
! summary: Get idof of from space and time components
!
!# Introduction
!
!@note
!   This is an expert level routine and should be used with care.
!@endnote

INTERFACE GetIDOF
  MODULE PURE FUNCTION obj_GetIDOF5(spacecompo, timecompo, tspacecompo) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! several time components
    INTEGER(I4B), INTENT(IN) :: tspacecompo
    !! total time component
    INTEGER(I4B) :: ans(SIZE(timecompo))
    !! idof of each time component
  END FUNCTION obj_GetIDOF5
END INTERFACE GetIDOF

!----------------------------------------------------------------------------
!                                                        GetIDOF@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-26
! summary: Get idof from space-time components
!
!# Introduction
!
!@note
!   This is an expert level routine and should be used with care.
!@endnote

INTERFACE GetIDOF
  MODULE PURE FUNCTION obj_GetIDOF6(spacecompo, timecompo, tspacecompo) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! several space components
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! a time component
    INTEGER(I4B), INTENT(IN) :: tspacecompo
    !! total space components
    INTEGER(I4B) :: ans(SIZE(spacecompo))
    !! idof of each space component
  END FUNCTION obj_GetIDOF6
END INTERFACE GetIDOF

!----------------------------------------------------------------------------
!                                                        GetIDOF@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-26
! summary: Get idof of physical variable from its local idof
!
!# Introduction
!
! What is local idof and global idof?
! In this context, idof is local idof of a physical variable.
! ans is global idof of a physical variable's local idof.
!
! For example, consider velocity with 2 space-components and 1 time component.
! then Vx has local idof 1, Vy has local idof 2.
! But it may happen that Vx and Vy have different idof when they are stored in
! DOF object.

INTERFACE GetIDOF
  MODULE PURE FUNCTION obj_GetIDOF7(obj, ivar, idof) &
    RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local idof of a physical variable
    INTEGER(I4B) :: ans
    !! global idof of a physical variable
  END FUNCTION obj_GetIDOF7
END INTERFACE GetIDOF

!----------------------------------------------------------------------------
!                                                        GetIDOF@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-26
! summary: Get all idof of a physical variable

INTERFACE GetIDOF
  MODULE PURE FUNCTION obj_GetIDOF8(obj, ivar) &
    & RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global idofs of all the dofs of a physical variable
  END FUNCTION obj_GetIDOF8
END INTERFACE GetIDOF

!----------------------------------------------------------------------------
!                                                       GetNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node
!
!# Introduction
!
! - This routine is like [[DOF_Method:GetIndex]].
! - It returns the location of degree of freedom number `idof`
! at node number `nodenum`.
!
!@note
! `nodenum` should be lesser than the total number of nodes
! defined for dof number `idof`.
!@endnote
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE GetNodeLoc
  MODULE PURE FUNCTION obj_GetNodeLoc1(obj, nodenum, idof) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNodeLoc1
END INTERFACE GetNodeLoc

!----------------------------------------------------------------------------
!                                                       GetNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node
!
!# Introduction
!
! - This routine is like [[DOF_Method:GetIndex]].
! - It returns the location of degree of freedom number `idof`
! at node number `nodenum`.
!
!@note
! `nodenum` should be lesser than the total number of nodes
! defined for dof number `idof`.
!@endnote
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE GetNodeLoc
  MODULE PURE FUNCTION obj_GetNodeLoc2(obj, nodenum, idof) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B) :: ans(SIZE(nodenum))
  END FUNCTION obj_GetNodeLoc2
END INTERFACE GetNodeLoc

!----------------------------------------------------------------------------
!                                                       GetNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node
!
!# Introduction
!
! - This routine is like [[DOF_Method:GetIndex]].
! - It returns the location of degree of freedom number `idof`
! at node number `nodenum`.
!
!@note
! `nodenum` should be lesser than the total number of nodes
! defined for dof number `idof`.
!@endnote
!
!@note
! idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE GetNodeLoc
  MODULE PURE FUNCTION obj_GetNodeLoc3(obj, nodenum, idof) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: idof(:)
    INTEGER(I4B) :: ans(SIZE(idof))
  END FUNCTION obj_GetNodeLoc3
END INTERFACE GetNodeLoc

!----------------------------------------------------------------------------
!                                                       GetNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node
!
!# Introduction
!
! ans(1) : istart
! ans(2) : iend
! ans(3) : stride
!
! In this way a given degree of freedom `idof` will be located in
! vec(istart:iend:stride).
!
!@note
! In [[DOF_]] object, idofs are continuously numbered, so if there are two
! or more physical variables, then idof of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE GetNodeLoc
  MODULE PURE FUNCTION obj_GetNodeLoc4(obj, idof) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B) :: ans(3)
  END FUNCTION obj_GetNodeLoc4
END INTERFACE GetNodeLoc

!----------------------------------------------------------------------------
!                                                       GetNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE GetNodeLoc
  MODULE PURE FUNCTION obj_GetNodeLoc5(obj, nodenum, ivar, idof) &
    & RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNodeLoc5
END INTERFACE GetNodeLoc

!----------------------------------------------------------------------------
!                                                       GetNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE GetNodeLoc
  MODULE PURE FUNCTION obj_GetNodeLoc6(obj, nodenum, ivar, idof) &
    & RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B) :: ans(SIZE(nodenum))
  END FUNCTION obj_GetNodeLoc6
END INTERFACE GetNodeLoc

!----------------------------------------------------------------------------
!                                                       GetNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE GetNodeLoc
  MODULE PURE FUNCTION obj_GetNodeLoc7(obj, nodenum, ivar, spacecompo, &
    & timecompo) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNodeLoc7
END INTERFACE GetNodeLoc

!----------------------------------------------------------------------------
!                                                       GetNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE GetNodeLoc
  MODULE PURE FUNCTION obj_GetNodeLoc8(obj, nodenum, ivar, spacecompo, &
    & timecompo) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    INTEGER(I4B) :: ans(SIZE(nodenum))
  END FUNCTION obj_GetNodeLoc8
END INTERFACE GetNodeLoc

!----------------------------------------------------------------------------
!                                                       GetNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE GetNodeLoc
  MODULE PURE FUNCTION obj_GetNodeLoc9(obj, nodenum, ivar, idof) &
    & RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof(:)
    INTEGER(I4B) :: ans(SIZE(idof))
  END FUNCTION obj_GetNodeLoc9
END INTERFACE GetNodeLoc

!----------------------------------------------------------------------------
!                                                       GetNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE GetNodeLoc
  MODULE PURE FUNCTION obj_GetNodeLoc10(obj, nodenum, ivar, spacecompo, &
    & timecompo) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    INTEGER(I4B) :: ans(SIZE(timecompo))
  END FUNCTION obj_GetNodeLoc10
END INTERFACE GetNodeLoc

!----------------------------------------------------------------------------
!                                                       GetNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE GetNodeLoc
  MODULE PURE FUNCTION obj_GetNodeLoc11(obj, nodenum, ivar, spacecompo, &
    & timecompo) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    INTEGER(I4B) :: ans(SIZE(spacecompo))
  END FUNCTION obj_GetNodeLoc11
END INTERFACE GetNodeLoc

!----------------------------------------------------------------------------
!                                                       GetNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE GetNodeLoc
  MODULE PURE FUNCTION obj_GetNodeLoc12(obj, nodenum, ivar, spacecompo, &
    & timecompo) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    INTEGER(I4B) :: ans(SIZE(timecompo) * SIZE(nodenum))
  END FUNCTION obj_GetNodeLoc12
END INTERFACE GetNodeLoc

!----------------------------------------------------------------------------
!                                                       GetNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE GetNodeLoc
  MODULE PURE FUNCTION obj_GetNodeLoc13(obj, nodenum, ivar, spacecompo, &
    & timecompo) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    INTEGER(I4B) :: ans(SIZE(spacecompo) * SIZE(nodenum))
  END FUNCTION obj_GetNodeLoc13
END INTERFACE GetNodeLoc

!----------------------------------------------------------------------------
!                                                         GetIndex@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodenum`
!
!# Introduction
!
! - This function returns indices, representing the location of all degrees
! of freedom define on a given node number.
! - The size of these indices is equal to the total number of DOF in obj
! - In this way, ans(ii) represents the location of ii dof at node number
! nodenum
! - It is user's responsibility to ensure that for every physical variable
! the `nodenumber` is lesser than the total number of
! nodes defined for that physical variable.
! - The returned indiced can be used to extract values from an instance of
! [[RealVector_]] or fortran vector of real numbers.
!
!@note
! The size of returned vector `ans` will be the total number of
! degrees of freedom in the [[DOF_]] object
!@endnote

INTERFACE GetIndex
  MODULE PURE FUNCTION obj_GetIndex1(obj, nodenum) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetIndex1
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                         GetIndex@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodenum`
!
!# Introduction
!
! - This function returns indices, representing the locations of all the
! degrees of freedom of a given physical variable `ivar` at a given
! node number `nodenum`
! - The physical variable is defined by an `ivar`
! - The size of these indices is equal to the total number of DOF
! defined for the `ivar` physical variable.
! - It is user's responsibility to ensure that for the selected physical var
! the `nodenum` is lesser than the total number of
! nodes defined for that physical variable.

INTERFACE GetIndex
  MODULE PURE FUNCTION obj_GetIndex2(obj, nodenum, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetIndex2
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                         GetIndex@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodenum`
!
!# Introduction
!
! Same as [[obj_GetIndex2]], but physical variable is selected by
! it name.

INTERFACE GetIndex
  MODULE PURE FUNCTION obj_GetIndex3(obj, nodenum, varname) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    CHARACTER(1), INTENT(IN) :: varname
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetIndex3
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                         GetIndex@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodenum`
!
!# Introduction
!
! - This function returns indices, representing the location of all the
! degrees of freedom defined at node numbers specified by nodenum.
! - The size of these indices is equal to the total number of DOF in obj
! times the size of nodenum(:)

INTERFACE GetIndex
  MODULE PURE FUNCTION obj_GetIndex4(obj, nodenum) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetIndex4
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                         GetIndex@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodenum`
!
!# Introduction
!
! - This function returns indices, representing the location of all the
! degrees of freedom of physical variable given by ivar, at nodes given in
! nodenum.
! - The physical variable is defined by an `ivar`
! - The size of these indices is equal to the total number of DOF
! defined for the `ivar` physical variable times the size of nodenum.

INTERFACE GetIndex
  MODULE PURE FUNCTION obj_GetIndex5(obj, nodenum, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetIndex5
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                         GetIndex@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodenum`
!
!# Introduction
!
! - This function returns a vector of integers (indices) for a
! a given node number and a given physical Variable.
! - The physical variable is defined by an `varname`
! - The size of these indices is equal to the total number of DOF
! defined for the `varname` physical variable.
! - The returned indices represents the degrees of freedom of
! physical variable `varname` defined on each node.
! - It is user's responsibility to ensure that for the selected physical var
! the `nodenumber` is lesser than the total number of
! nodes defined for that physical variable.
! - The returned indices can be used for Getting the dof (all dof)
! defined on the nodenum for the given physical variable.
! - The returned indices can be used to extract values from an instance of
! [[RealVector_]] or fortran vector of real numbers.

INTERFACE GetIndex
  MODULE PURE FUNCTION obj_GetIndex6(obj, nodenum, varname) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    CHARACTER(1), INTENT(IN) :: varname
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetIndex6
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                         GetIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE GetIndex
  MODULE PROCEDURE obj_GetNodeLoc5
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                         GetIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE GetIndex
  MODULE PROCEDURE obj_GetNodeLoc6
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                         GetIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE GetIndex
  MODULE PROCEDURE obj_GetNodeLoc7
END INTERFACE GetIndex

!----------------------------------------------------------------------------
!                                                         GetIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE GetIndex
  MODULE PROCEDURE obj_GetNodeLoc8
END INTERFACE GetIndex

END MODULE DOF_GetMethods
