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

!> author: Vikas Sharma, Ph. D.
! date:         1 March 2021
! summary:         This submodule contains IO methods for [[ReferenceElement_]]

SUBMODULE(ReferenceElement_Method) IOMethods
USE BaseMethod
CONTAINS

!----------------------------------------------------------------------------
!                                                                 MDEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE reftopo_MdEncode
TYPE(String), ALLOCATABLE :: astr(:, :)
INTEGER(I4B) :: n, ii, jj
TYPE(String) :: rh(3), ch(1)

rh(1) = "Element type"
rh(2) = "Xidimension"
rh(3) = "Nptrs"
ch(1) = ""

IF (ALLOCATED(obj%nptrs)) THEN
  n = SIZE(obj%nptrs)
  CALL reallocate(astr, 3, n)
  astr(1, 1) = ElementName(obj%name)
  DO ii = 2, n
    astr(1, ii) = ""
  END DO

  astr(2, 1) = tostring(obj%xidimension)
  DO ii = 2, n
    astr(2, ii) = ""
  END DO

  DO ii = 1, n
    astr(3, ii) = tostring(obj%nptrs(ii))
  END DO

ELSE

  n = 1
  CALL reallocate(astr, 3, n)
  astr(1, 1) = ElementName(obj%name)
  astr(2, 1) = tostring(obj%xidimension)
  astr(3, 1) = "NOT ALLOCATED"

END IF

ans = MdEncode(val=astr, rh=rh, ch=ch)

IF (ALLOCATED(astr)) DEALLOCATE (astr)

END PROCEDURE reftopo_MdEncode

!----------------------------------------------------------------------------
!                                                                 MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_MdEncode
!! Define internal variable
INTEGER(I4B) :: j, tsize, jj
LOGICAL(LGT) :: notFull0
TYPE(String) :: astr(20)
CHARACTER(1), PARAMETER, DIMENSION(3) :: xyz = ["x", "y", "z"]
TYPE(String) :: rowTitle(20), colTitle(1)

colTitle(1) = ""

rowTitle(1) = "Element type"; astr(1) = ElementName(obj%name)
rowTitle(2) = "Xidimension"; astr(2) = tostring(obj%xiDimension)
rowTitle(3) = "NSD"; astr(3) = tostring(obj%nsd)
rowTitle(4) = "tPoints"; astr(4) = tostring(obj%entityCounts(1))
rowTitle(5) = "tLines"; astr(5) = tostring(obj%entityCounts(2))
rowTitle(6) = "tSurfaces"; astr(6) = tostring(obj%entityCounts(3))
rowTitle(7) = "tVolumes"; astr(7) = tostring(obj%entityCounts(4))

tsize = SIZE(obj%xij, 1)
DO j = 1, tsize
  rowTitle(7 + j) = xyz(j)
END DO

ans = MdEncode(val=astr(1:7), rh=rowTitle(1:7), ch=colTitle)// &
    &  char_lf//"Nodal Coordinates:"//char_lf//char_lf// &
    &  MdEncode(obj%xij, rh=rowTitle(7 + 1:7 + tsize), ch=colTitle)

! pointTopology
DO j = 1, obj%entityCounts(1)
  ans = ans//"PointTopology( "//tostring(j)//" ) : "// &
  & char_lf//char_lf//MdEncode(obj%topology(j))
END DO

!! edgeTopology
tsize = obj%entityCounts(1)
DO j = 1, obj%entityCounts(2)
  ans = ans//"EdgeTopology( "//tostring(j)//" ) : "// &
  & char_lf//char_lf//MdEncode(obj%topology(tsize + j))
END DO

!! faceTopology
tsize = tsize + obj%entityCounts(2)
DO j = 1, obj%entityCounts(3)
  ans = ans//"FaceTopology( "//tostring(j)//" ) : "// &
  & char_lf//char_lf//MdEncode(obj%topology(tsize + j))
END DO

!! cellTopology
tsize = tsize + obj%entityCounts(3)
DO j = 1, obj%entityCounts(4)
  ans = ans//"CellTopology( "//tostring(j)//" ) : "// &
  & char_lf//char_lf//MdEncode(obj%topology(tsize + j))
END DO
END PROCEDURE refelem_MdEncode

!----------------------------------------------------------------------------
!                                                       refelem_ReactEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_ReactEncode
!! Define internal variable
INTEGER(I4B) :: j, tsize
LOGICAL(LGT) :: notFull0
TYPE(String) :: rowTitle(20), colTitle(1)
TYPE(String) :: astr(20)
CHARACTER(1), PARAMETER, DIMENSION(3) :: xyz = ["x", "y", "z"]

colTitle(1) = ""
rowTitle(1) = "Element type"; astr(1) = ElementName(obj%name)
rowTitle(2) = "Xidimension"; astr(2) = tostring(obj%xiDimension)
rowTitle(3) = "NSD"; astr(3) = tostring(obj%nsd)
rowTitle(4) = "tPoints"; astr(4) = tostring(obj%entityCounts(1))
rowTitle(5) = "tLines"; astr(5) = tostring(obj%entityCounts(2))
rowTitle(6) = "tSurfaces"; astr(6) = tostring(obj%entityCounts(3))
rowTitle(7) = "tVolumes"; astr(7) = tostring(obj%entityCounts(4))

tsize = SIZE(obj%xij, 1)
DO j = 1, tsize
  rowTitle(7 + j) = xyz(j)
END DO

ans = MdEncode(val=astr(1:7), rh=rowTitle(1:7), ch=colTitle)// &
    & char_lf//"Nodal Coordinates:"//char_lf//char_lf// &
    & MdEncode(obj%xij, rh=rowTitle(8:7 + tsize), ch=colTitle)

IF (obj%entityCounts(1) .GT. 0_I4B) THEN
  ans = ans//React_StartTabs()//char_lf

  !! pointTopology
  tsize = 0
  DO j = 1, obj%entityCounts(1)
    ans = ans//React_StartTabItem( &
    & VALUE=tostring(j), &
    & label="PointTopology( "//tostring(j)//" ) : ")//char_lf//  &
    & MdEncode(obj%topology(tsize + j))//char_lf  &
    & //React_EndTabItem()//char_lf
  END DO

  ans = ans//React_EndTabs()//char_lf
END IF

IF (obj%entityCounts(2) .GT. 0_I4B) THEN
  ans = ans//React_StartTabs()//char_lf

  !! edgeTopology
  tsize = obj%entityCounts(1) + tsize
  DO j = 1, obj%entityCounts(2)
    ans = ans//React_StartTabItem( &
    & VALUE=tostring(j), &
    & label="EdgeTopology( "//tostring(j)//" ) : ")//char_lf//  &
    & MdEncode(obj%topology(tsize + j))//char_lf  &
    & //React_EndTabItem()//char_lf
  END DO

  ans = ans//React_EndTabs()//char_lf
END IF

IF (obj%entityCounts(3) .GT. 0_I4B) THEN
  ans = ans//React_StartTabs()//char_lf

  !! edgeTopology
  tsize = obj%entityCounts(2) + tsize
  DO j = 1, obj%entityCounts(3)
    ans = ans//React_StartTabItem( &
    & VALUE=tostring(j), &
    & label="FacetTopology( "//tostring(j)//" ) : ")//char_lf//  &
    & MdEncode(obj%topology(tsize + j))//char_lf  &
    & //React_EndTabItem()//char_lf
  END DO

  ans = ans//React_EndTabs()//char_lf
END IF

IF (obj%entityCounts(4) .GT. 0_I4B) THEN
  ans = ans//React_StartTabs()//char_lf

  !! edgeTopology
  tsize = obj%entityCounts(3) + tsize
  DO j = 1, obj%entityCounts(4)
    ans = ans//React_StartTabItem( &
    & VALUE=tostring(j), &
    & label="CellTopology( "//tostring(j)//" ) : ")//char_lf//  &
    & MdEncode(obj%topology(tsize + j))//char_lf  &
    & //React_EndTabItem()//char_lf
  END DO

  ans = ans//React_EndTabs()//char_lf
END IF

END PROCEDURE refelem_ReactEncode

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE reftopo_Display
CALL Display(msg, unitno=unitno)
CALL Display("ElemType: "//TRIM(ElementName(obj%Name)), unitno=unitno)
CALL Display("XiDim: "//TRIM(INT2STR(obj%XiDimension)), unitno=unitno)
CALL Display(obj%Nptrs, "Nptrs: ", unitno=unitno)
END PROCEDURE reftopo_Display

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Display
! Define internal variable
INTEGER(I4B) :: I, j
CALL Display(msg, unitno=unitno)
CALL Display("DomainName : "//TRIM(obj%domainName), &
  & unitno=unitno)
CALL Display("ElemType : "//TRIM(ElementName(obj%Name)), &
  & unitno=unitno)
CALL Display(obj%XiDimension, "XiDimension :: ", &
  & unitno=unitno)
CALL Display(obj%NSD, "NSD : ", &
  & unitno=unitno)
CALL Display(obj%Order, "Order : ", &
  & unitno=unitno)
CALL Display(obj%EntityCounts(1), "EntityCounts(0) : ", &
  & unitno=unitno)
CALL Display(obj%EntityCounts(2), "EntityCounts(1) : ", &
  & unitno=unitno)
CALL Display(obj%EntityCounts(3), "EntityCounts(2) : ", &
  & unitno=unitno)
CALL Display(obj%EntityCounts(4), "EntityCounts(3) : ", &
  & unitno=unitno)

CALL BlankLines(nol=1, unitNo=unitNo)
DO j = 1, SIZE(obj%XiJ, 2)
  CALL EqualLine(unitNo=unitNo)
  CALL Display(obj%XiJ(:, j), "Node( "//TRIM(str(j, .TRUE.))//" ) : ", &
    & unitno=unitno)
  CALL BlankLines(nol=1, unitNo=unitNo)
END DO

DO j = 1, SIZE(obj%Topology)
  CALL EqualLine(unitNo=unitNo)
  CALL Display(obj%Topology(j), "Topology( "//TRIM(INT2STR(j))//" ) : ", &
    & unitno=unitno)
  CALL BlankLines(nol=1, unitNo=unitNo)
END DO

END PROCEDURE refelem_Display

END SUBMODULE IOMethods
