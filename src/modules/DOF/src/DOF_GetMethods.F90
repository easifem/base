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
USE GlobalData
USE BaseType
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
!                                                  DOFStartIndex@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: returns obj%map( ivar, 5 )

INTERFACE
  MODULE PURE FUNCTION dof_DOFStartIndex(obj, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B) :: ans
  END FUNCTION dof_DOFStartIndex
END INTERFACE

INTERFACE OPERATOR(.DOFStartIndex.)
  MODULE PROCEDURE dof_DOFStartIndex
END INTERFACE OPERATOR(.DOFStartIndex.)

!----------------------------------------------------------------------------
!                                                  DOFEndIndex@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: returns obj%map( ivar+1, 5 ) - 1

INTERFACE
  MODULE PURE FUNCTION dof_DOFEndIndex(obj, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B) :: ans
  END FUNCTION dof_DOFEndIndex
END INTERFACE

INTERFACE OPERATOR(.DOFEndIndex.)
  MODULE PROCEDURE dof_DOFEndIndex
END INTERFACE OPERATOR(.DOFEndIndex.)

!----------------------------------------------------------------------------
!                                                           tNodes@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the total length of the vector

INTERFACE
  MODULE PURE FUNCTION dof_tNodes1(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION dof_tNodes1
END INTERFACE

INTERFACE OPERATOR(.tNodes.)
  MODULE PROCEDURE dof_tNodes1
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE dof_tNodes1
END INTERFACE SIZE

!----------------------------------------------------------------------------
!                                                           tNodes@getMethod
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

INTERFACE
  MODULE PURE FUNCTION dof_tNodes2(obj, idof) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B) :: ans
  END FUNCTION dof_tNodes2
END INTERFACE

INTERFACE OPERATOR(.tNodes.)
  MODULE PROCEDURE dof_tNodes2
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE dof_tNodes2
END INTERFACE SIZE

!----------------------------------------------------------------------------
!                                                           tNodes@getMethod
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

INTERFACE
  MODULE PURE FUNCTION dof_tNodes3(obj, varname) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: varname
    INTEGER(I4B) :: ans
  END FUNCTION dof_tNodes3
END INTERFACE

INTERFACE OPERATOR(.tNodes.)
  MODULE PROCEDURE dof_tNodes3
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE dof_tNodes3
END INTERFACE SIZE

!----------------------------------------------------------------------------
!                                                           tNodes@getMethod
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

INTERFACE
  MODULE PURE FUNCTION dof_tNodes4(obj, idof) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof(:)
    INTEGER(I4B) :: ans
  END FUNCTION dof_tNodes4
END INTERFACE

INTERFACE OPERATOR(.tNodes.)
  MODULE PROCEDURE dof_tNodes4
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE dof_tNodes4
END INTERFACE SIZE

!----------------------------------------------------------------------------
!                                                             tDOF@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This function returns the total number of degree of freedom

INTERFACE
  MODULE PURE FUNCTION dof_tdof1(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION dof_tdof1
END INTERFACE

INTERFACE OPERATOR(.tDOF.)
  MODULE PROCEDURE dof_tdof1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tDOF@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This subroutine returns the total number of degrees of freedom
!
!# Introduction
! This function returns the total number of degrees of freedom in a
! physical variable.
! The physical variable is specified by using its name.

INTERFACE
  MODULE PURE FUNCTION dof_tdof2(obj, Name) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    CHARACTER(1), INTENT(IN) :: Name
    INTEGER(I4B) :: ans
  END FUNCTION dof_tdof2
END INTERFACE

INTERFACE OPERATOR(.tDOF.)
  MODULE PROCEDURE dof_tdof2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tDOF@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: This subroutine returns the total number of degrees of freedom
!
!# Introduction
! This function returns the total number of degrees of freedom in a
! physical variable.
! The physical variable is specified by using its name.

INTERFACE
  MODULE PURE FUNCTION dof_tdof3(obj, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B) :: ans
  END FUNCTION dof_tdof3
END INTERFACE

INTERFACE OPERATOR(.tDOF.)
  MODULE PROCEDURE dof_tdof3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           tNames@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the total number of names in dof object

INTERFACE
  MODULE PURE FUNCTION dof_tNames(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION dof_tNames
END INTERFACE

INTERFACE OPERATOR(.tNames.)
  MODULE PROCEDURE dof_tNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Names@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the name of all physical variables stored in obj

INTERFACE
  MODULE PURE FUNCTION dof_names1(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    CHARACTER(1), ALLOCATABLE :: ans(:)
  END FUNCTION dof_names1
END INTERFACE

INTERFACE OPERATOR(.Names.)
  MODULE PROCEDURE dof_names1
END INTERFACE OPERATOR(.Names.)

!----------------------------------------------------------------------------
!                                                           Names@getMethod
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

INTERFACE
  MODULE PURE FUNCTION dof_names2(obj, ii) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ii
    CHARACTER(1) :: ans
  END FUNCTION dof_names2
END INTERFACE

INTERFACE OPERATOR(.Names.)
  MODULE PROCEDURE dof_names2
END INTERFACE OPERATOR(.Names.)

!----------------------------------------------------------------------------
!                                                     NameToIndex@getMethod
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
!                                                tspacecomponents@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the total physical variable which have space-compo

INTERFACE
  MODULE PURE FUNCTION dof_tspacecomponents(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION dof_tspacecomponents
END INTERFACE

INTERFACE OPERATOR(.tspacecomponents.)
  MODULE PROCEDURE dof_tspacecomponents
END INTERFACE

!----------------------------------------------------------------------------
!                                                spacecomponents@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the space components of each physical vars

INTERFACE
  MODULE PURE FUNCTION dof_spacecomponents1(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION dof_spacecomponents1
END INTERFACE

INTERFACE OPERATOR(.spacecomponents.)
  MODULE PROCEDURE dof_spacecomponents1
END INTERFACE

!----------------------------------------------------------------------------
!                                                spacecomponents@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the space component of a given physical vars

INTERFACE
  MODULE PURE FUNCTION dof_spacecomponents2(obj, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B) :: ans
  END FUNCTION dof_spacecomponents2
END INTERFACE

INTERFACE OPERATOR(.spacecomponents.)
  MODULE PROCEDURE dof_spacecomponents2
END INTERFACE

!----------------------------------------------------------------------------
!                                                ttimecomponents@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the total physical var which has time compo

INTERFACE
  MODULE PURE FUNCTION dof_ttimecomponents(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION dof_ttimecomponents
END INTERFACE

INTERFACE OPERATOR(.ttimecomponents.)
  MODULE PROCEDURE dof_ttimecomponents
END INTERFACE

!----------------------------------------------------------------------------
!                                                timecomponents@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the timecompo

INTERFACE
  MODULE PURE FUNCTION dof_timecomponents1(obj) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION dof_timecomponents1
END INTERFACE

INTERFACE OPERATOR(.timecomponents.)
  MODULE PROCEDURE dof_timecomponents1
END INTERFACE

!----------------------------------------------------------------------------
!                                                timecomponents@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: Returns the timecompo

INTERFACE
  MODULE PURE FUNCTION dof_timecomponents2(obj, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B) :: ans
  END FUNCTION dof_timecomponents2
END INTERFACE

INTERFACE OPERATOR(.timecomponents.)
  MODULE PROCEDURE dof_timecomponents2
END INTERFACE

!----------------------------------------------------------------------------
!                                                               EQ@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION dof_isEqual(obj1, obj2) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj1
    TYPE(DOF_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION dof_isEqual
END INTERFACE

INTERFACE OPERATOR(.EQ.)
  MODULE PROCEDURE dof_isEqual
END INTERFACE OPERATOR(.EQ.)

!----------------------------------------------------------------------------
!                                                               NE@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION dof_isNE(obj1, obj2) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj1
    TYPE(DOF_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION dof_isNE
END INTERFACE

INTERFACE OPERATOR(.NE.)
  MODULE PROCEDURE dof_isNE
END INTERFACE OPERATOR(.NE.)

!----------------------------------------------------------------------------
!                                                        getIDOF@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION dof_getIDOF1(spacecompo, timecompo, tspacecompo) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    INTEGER(I4B), INTENT(IN) :: tspacecompo
    INTEGER(I4B) :: ans
  END FUNCTION dof_getIDOF1
END INTERFACE

INTERFACE getIDOF
  MODULE PROCEDURE dof_getIDOF1
END INTERFACE getIDOF

!----------------------------------------------------------------------------
!                                                        getIDOF@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION dof_getIDOF2(obj, ivar, spacecompo, timecompo) &
    & RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    INTEGER(I4B) :: ans
  END FUNCTION dof_getIDOF2
END INTERFACE

INTERFACE getIDOF
  MODULE PROCEDURE dof_getIDOF2
END INTERFACE getIDOF

!----------------------------------------------------------------------------
!                                                        getIDOF@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION dof_getIDOF3(obj, ivar, spacecompo, timecompo) &
    & RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    INTEGER(I4B) :: ans(SIZE(timecompo))
  END FUNCTION dof_getIDOF3
END INTERFACE

INTERFACE getIDOF
  MODULE PROCEDURE dof_getIDOF3
END INTERFACE getIDOF

!----------------------------------------------------------------------------
!                                                        getIDOF@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION dof_getIDOF4(obj, ivar, spacecompo, timecompo) &
    & RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    INTEGER(I4B) :: ans(SIZE(spacecompo))
  END FUNCTION dof_getIDOF4
END INTERFACE

INTERFACE getIDOF
  MODULE PROCEDURE dof_getIDOF4
END INTERFACE getIDOF

!----------------------------------------------------------------------------
!                                                        getIDOF@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION dof_getIDOF5(spacecompo, timecompo, tspacecompo) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    INTEGER(I4B), INTENT(IN) :: tspacecompo
    INTEGER(I4B) :: ans(SIZE(timecompo))
  END FUNCTION dof_getIDOF5
END INTERFACE

INTERFACE getIDOF
  MODULE PROCEDURE dof_getIDOF5
END INTERFACE getIDOF

!----------------------------------------------------------------------------
!                                                        getIDOF@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION dof_getIDOF6(spacecompo, timecompo, tspacecompo) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    INTEGER(I4B), INTENT(IN) :: tspacecompo
    INTEGER(I4B) :: ans(SIZE(spacecompo))
  END FUNCTION dof_getIDOF6
END INTERFACE

INTERFACE getIDOF
  MODULE PROCEDURE dof_getIDOF6
END INTERFACE getIDOF

!----------------------------------------------------------------------------
!                                                        getIDOF@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION dof_getIDOF7(obj, ivar, idof) &
    & RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B) :: ans
  END FUNCTION dof_getIDOF7
END INTERFACE

INTERFACE getIDOF
  MODULE PROCEDURE dof_getIDOF7
END INTERFACE getIDOF

!----------------------------------------------------------------------------
!                                                        getIDOF@GetMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION dof_getIDOF8(obj, ivar) &
    & RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION dof_getIDOF8
END INTERFACE

INTERFACE getIDOF
  MODULE PROCEDURE dof_getIDOF8
END INTERFACE getIDOF

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node
!
!# Introduction
!
! - This routine is like [[DOF_Method:getIndex]].
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

INTERFACE
  MODULE PURE FUNCTION dof_getNodeLoc1(obj, nodenum, idof) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B) :: ans
  END FUNCTION dof_getNodeLoc1
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc1
END INTERFACE getNodeLoc

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node
!
!# Introduction
!
! - This routine is like [[DOF_Method:getIndex]].
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

INTERFACE
  MODULE PURE FUNCTION dof_getNodeLoc2(obj, nodenum, idof) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B) :: ans(SIZE(nodenum))
  END FUNCTION dof_getNodeLoc2
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc2
END INTERFACE getNodeLoc

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node
!
!# Introduction
!
! - This routine is like [[DOF_Method:getIndex]].
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

INTERFACE
  MODULE PURE FUNCTION dof_getNodeLoc3(obj, nodenum, idof) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: idof(:)
    INTEGER(I4B) :: ans(SIZE(idof))
  END FUNCTION dof_getNodeLoc3
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc3
END INTERFACE getNodeLoc

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
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

INTERFACE
  MODULE PURE FUNCTION dof_getNodeLoc4(obj, idof) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B) :: ans(3)
  END FUNCTION dof_getNodeLoc4
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc4
END INTERFACE getNodeLoc

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE
  MODULE PURE FUNCTION dof_getNodeLoc5(obj, nodenum, ivar, idof) &
    & RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B) :: ans
  END FUNCTION dof_getNodeLoc5
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc5
END INTERFACE getNodeLoc

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE
  MODULE PURE FUNCTION dof_getNodeLoc6(obj, nodenum, ivar, idof) &
    & RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B) :: ans(SIZE(nodenum))
  END FUNCTION dof_getNodeLoc6
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc6
END INTERFACE getNodeLoc

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE
  MODULE PURE FUNCTION dof_getNodeLoc7(obj, nodenum, ivar, spacecompo, &
    & timecompo) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    INTEGER(I4B) :: ans
  END FUNCTION dof_getNodeLoc7
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc7
END INTERFACE getNodeLoc

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE
  MODULE PURE FUNCTION dof_getNodeLoc8(obj, nodenum, ivar, spacecompo, &
    & timecompo) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    INTEGER(I4B) :: ans(SIZE(nodenum))
  END FUNCTION dof_getNodeLoc8
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc8
END INTERFACE getNodeLoc

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE
  MODULE PURE FUNCTION dof_getNodeLoc9(obj, nodenum, ivar, idof) &
    & RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof(:)
    INTEGER(I4B) :: ans(SIZE(idof))
  END FUNCTION dof_getNodeLoc9
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc9
END INTERFACE getNodeLoc

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE
  MODULE PURE FUNCTION dof_getNodeLoc10(obj, nodenum, ivar, spacecompo, &
    & timecompo) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    INTEGER(I4B) :: ans(SIZE(timecompo))
  END FUNCTION dof_getNodeLoc10
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc10
END INTERFACE getNodeLoc

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE
  MODULE PURE FUNCTION dof_getNodeLoc11(obj, nodenum, ivar, spacecompo, &
    & timecompo) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    INTEGER(I4B) :: ans(SIZE(spacecompo))
  END FUNCTION dof_getNodeLoc11
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc11
END INTERFACE getNodeLoc

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE
  MODULE PURE FUNCTION dof_getNodeLoc12(obj, nodenum, ivar, spacecompo, &
    & timecompo) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    INTEGER(I4B) :: ans(SIZE(timecompo) * SIZE(nodenum))
  END FUNCTION dof_getNodeLoc12
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc12
END INTERFACE getNodeLoc

!----------------------------------------------------------------------------
!                                                       getNodeLoc@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the location of node

INTERFACE
  MODULE PURE FUNCTION dof_getNodeLoc13(obj, nodenum, ivar, spacecompo, &
    & timecompo) RESULT(ans)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    INTEGER(I4B) :: ans(SIZE(spacecompo) * SIZE(nodenum))
  END FUNCTION dof_getNodeLoc13
END INTERFACE

INTERFACE getNodeLoc
  MODULE PROCEDURE dof_getNodeLoc13
END INTERFACE getNodeLoc

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
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

INTERFACE
  MODULE PURE FUNCTION dof_getIndex1(obj, nodenum) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION dof_getIndex1
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE dof_getIndex1
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
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

INTERFACE
  MODULE PURE FUNCTION dof_getIndex2(obj, nodenum, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION dof_getIndex2
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE dof_getIndex2
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Returns the indices for node number `nodenum`
!
!# Introduction
!
! Same as [[dof_getIndex2]], but physical variable is selected by
! it name.

INTERFACE
  MODULE PURE FUNCTION dof_getIndex3(obj, nodenum, varname) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    CHARACTER(1), INTENT(IN) :: varname
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION dof_getIndex3
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE dof_getIndex3
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
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

INTERFACE
  MODULE PURE FUNCTION dof_getIndex4(obj, nodenum) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION dof_getIndex4
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE dof_getIndex4
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
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

INTERFACE
  MODULE PURE FUNCTION dof_getIndex5(obj, nodenum, ivar) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION dof_getIndex5
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE dof_getIndex5
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
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
! - The returned indices can be used for getting the dof (all dof)
! defined on the nodenum for the given physical variable.
! - The returned indices can be used to extract values from an instance of
! [[RealVector_]] or fortran vector of real numbers.

INTERFACE
  MODULE PURE FUNCTION dof_getIndex6(obj, nodenum, varname) RESULT(ans)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    CHARACTER(1), INTENT(IN) :: varname
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION dof_getIndex6
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE dof_getIndex6
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE getIndex
  MODULE PROCEDURE dof_getNodeLoc5
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE getIndex
  MODULE PROCEDURE dof_getNodeLoc6
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE getIndex
  MODULE PROCEDURE dof_getNodeLoc7
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE getIndex
  MODULE PROCEDURE dof_getNodeLoc8
END INTERFACE getIndex

END MODULE DOF_GetMethods
