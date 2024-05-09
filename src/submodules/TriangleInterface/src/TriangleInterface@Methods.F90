! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(TriangleInterface) Methods
USE ISO_C_BINDING, ONLY: C_LOC, C_F_POINTER, C_ASSOCIATED
USE Display_Method, ONLY: MyDisplay => Display
IMPLICIT NONE

#include "./definemacro.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                         TriangleReport
!----------------------------------------------------------------------------

MODULE PROCEDURE TriangleDeallocate
CALL TriangleFree(obj)
CALL TriangleNullify(obj)
END PROCEDURE TriangleDeallocate

!----------------------------------------------------------------------------
!                                                         TriangleSetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE TriangleSetParam
IF (PRESENT(pointlist)) obj%pointlist = C_LOC(pointlist)
IF (PRESENT(pointattributelist)) obj%pointattributelist = &
  C_LOC(pointattributelist)
IF (PRESENT(pointmarkerlist)) obj%pointmarkerlist = C_LOC(pointmarkerlist)
IF (PRESENT(numberofpoints)) obj%numberofpoints = numberofpoints
IF (PRESENT(numberofpointattributes)) obj%numberofpointattributes = &
  numberofpointattributes
IF (PRESENT(trianglelist)) obj%trianglelist = C_LOC(trianglelist)
IF (PRESENT(triangleattributelist)) obj%triangleattributelist = &
  C_LOC(triangleattributelist)
IF (PRESENT(trianglearealist)) obj%trianglearealist = C_LOC(trianglearealist)
IF (PRESENT(neighborlist)) obj%neighborlist = C_LOC(neighborlist)
IF (PRESENT(numberoftriangles)) obj%numberoftriangles = numberoftriangles
IF (PRESENT(numberofcorners)) obj%numberofcorners = numberofcorners
IF (PRESENT(numberoftriangleattributes)) obj%numberoftriangleattributes = &
  numberoftriangleattributes
IF (PRESENT(segmentlist)) obj%segmentlist = C_LOC(segmentlist)
IF (PRESENT(segmentmarkerlist)) obj%segmentmarkerlist = &
  C_LOC(segmentmarkerlist)
IF (PRESENT(numberofsegments)) obj%numberofsegments = numberofsegments
IF (PRESENT(holelist)) obj%holelist = C_LOC(holelist)
IF (PRESENT(numberofholes)) obj%numberofholes = numberofholes
IF (PRESENT(regionlist)) obj%regionlist = C_LOC(regionlist)
IF (PRESENT(numberofregions)) obj%numberofregions = numberofregions
IF (PRESENT(edgelist)) obj%edgelist = C_LOC(edgelist)
IF (PRESENT(edgemarkerlist)) obj%edgemarkerlist = C_LOC(edgemarkerlist)
IF (PRESENT(numberofedges)) obj%numberofedges = numberofedges
IF (PRESENT(normlist)) obj%normlist = C_LOC(normlist)
END PROCEDURE TriangleSetParam

!----------------------------------------------------------------------------
!                                                         TriangleGetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE TriangleGetParam
C2F(pointlist, obj%numberofpoints)
C2F(pointattributelist, obj%numberofpointattributes * obj%numberofpoints)
C2F(pointmarkerlist, obj%numberofpoints)
SimpleSet(numberofpoints)
SimpleSet(numberofpointattributes)
C2F(trianglelist, obj%numberofcorners * obj%numberoftriangles)
C2F(triangleattributelist, obj%numberoftriangleattributes * obj%numberoftriangles)
C2F(trianglearealist, obj%numberoftriangles)
C2F(neighborlist, 3 * obj%numberoftriangles)
SimpleSet(numberoftriangles)
SimpleSet(numberofcorners)
SimpleSet(numberoftriangleattributes)
C2F(segmentlist, 2 * obj%numberofsegments)
C2F(segmentmarkerlist, obj%numberofsegments)
SimpleSet(numberofsegments)
C2F(holelist, 2 * obj%numberofholes)
SimpleSet(numberofholes)
C2F(regionlist, 4 * obj%numberofregions)
SimpleSet(numberofregions)
C2F(edgelist, 2 * obj%numberofedges)
C2F(edgemarkerlist, obj%numberofedges)
C2F(normlist, 2 * obj%numberofedges)
SimpleSet(numberofedges)
END PROCEDURE TriangleGetParam

!----------------------------------------------------------------------------
!                                                       TriangleNullify
!----------------------------------------------------------------------------

MODULE PROCEDURE TriangleNullify
MyNullify(pointlist)
MyNullify(pointattributelist)
MyNullify(pointmarkerlist)
MyNullify(trianglelist)
MyNullify(triangleattributelist)
MyNullify(trianglearealist)
MyNullify(neighborlist)
MyNullify(segmentlist)
MyNullify(segmentmarkerlist)
MyNullify(holelist)
MyNullify(regionlist)
MyNullify(edgelist)
MyNullify(edgemarkerlist)
MyNullify(normlist)
SimpleNull(numberofpoints)
SimpleNull(numberofpointattributes)
SimpleNull(numberoftriangles)
SimpleNull(numberofcorners)
SimpleNull(numberoftriangleattributes)
SimpleNull(numberofsegments)
SimpleNull(numberofholes)
SimpleNull(numberofregions)
SimpleNull(numberofedges)
END PROCEDURE TriangleNullify

!----------------------------------------------------------------------------
!                                                           Display
!----------------------------------------------------------------------------

MODULE PROCEDURE TriangleDisplay

CALL DisplayPtr("pointlist", obj%pointlist)
CALL DisplayPtr("pointattributelist", obj%pointattributelist)
CALL DisplayPtr("pointmarkerlist", obj%pointmarkerlist)
CALL DisplayPtr("trianglelist", obj%trianglelist)
CALL DisplayPtr("triangleattributelist", obj%triangleattributelist)
CALL DisplayPtr("trianglearealist", obj%trianglearealist)
CALL DisplayPtr("neighborlist", obj%neighborlist)
CALL DisplayPtr("segmentlist", obj%segmentlist)
CALL DisplayPtr("segmentmarkerlist", obj%segmentmarkerlist)
CALL DisplayPtr("holelist", obj%holelist)
CALL DisplayPtr("regionlist", obj%regionlist)
CALL DisplayPtr("edgelist", obj%edgelist)
CALL DisplayPtr("edgemarkerlist", obj%edgemarkerlist)
CALL DisplayPtr("normlist", obj%normlist)

CALL MyDisplay(obj%numberofpoints, "numberofpoints: ", unitno=unitno)
CALL MyDisplay(obj%numberofpointattributes,"numberofpointattributes: ",unitno=unitno)
CALL MyDisplay(obj%numberoftriangles, "numberoftriangles: ", unitno=unitno)
CALL MyDisplay(obj%numberofcorners, "numberofcorners: ", unitno=unitno)
CALL MyDisplay(obj%numberoftriangleattributes,"numberoftriangleattributes: ",unitno=unitno)
CALL MyDisplay(obj%numberofsegments, "numberofsegments: ", unitno=unitno)
CALL MyDisplay(obj%numberofholes, "numberofholes: ", unitno=unitno)
CALL MyDisplay(obj%numberofregions, "numberofregions: ", unitno=unitno)
CALL MyDisplay(obj%numberofedges, "numberofedges: ", unitno=unitno)

CONTAINS

SUBROUTINE DisplayPtr(myname, cptr)
  CHARACTER(*), INTENT(in) :: myname
  TYPE(C_PTR), INTENT(in) :: cptr

  LOGICAL(LGT) :: abool
  abool = C_ASSOCIATED(cptr)
  CALL MyDisplay(abool, myname//" ASSOCIATED: ", unitno=unitno)
END SUBROUTINE DisplayPtr

END PROCEDURE TriangleDisplay

#include "./undefinemacro.h"

END SUBMODULE Methods
