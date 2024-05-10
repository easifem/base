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

MODULE TriangleInterface
USE ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_PTR, C_NULL_PTR
USE GlobalData, ONLY: DFP, I4B, LGT

IMPLICIT NONE
PRIVATE

PUBLIC :: TriangulateIO_
PUBLIC :: Triangulate
PUBLIC :: TriangleReport
PUBLIC :: TriangleFree
PUBLIC :: TriangleDeallocate
PUBLIC :: TriangleSetParam
PUBLIC :: TriangleGetParam
PUBLIC :: TriangleNullify
PUBLIC :: TriangleDisplay
PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                           TriangulateIO_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-09
! summary: TringulateIO

TYPE, BIND(c) :: TriangulateIO_
  TYPE(C_PTR) :: pointlist = C_NULL_PTR
  TYPE(C_PTR) :: pointattributelist = C_NULL_PTR
  TYPE(C_PTR) :: pointmarkerlist = C_NULL_PTR
  INTEGER(C_INT) :: numberofpoints = 0
  INTEGER(C_INT) :: numberofpointattributes = 0

  TYPE(C_PTR) :: trianglelist = C_NULL_PTR
  TYPE(C_PTR) :: triangleattributelist = C_NULL_PTR
  TYPE(C_PTR) :: trianglearealist = C_NULL_PTR
  !! In
  TYPE(C_PTR) :: neighborlist = C_NULL_PTR
  !! Out
  INTEGER(C_INT) :: numberoftriangles = 0
  INTEGER(C_INT) :: numberofcorners = 0
  INTEGER(C_INT) :: numberoftriangleattributes = 0

  TYPE(C_PTR) :: segmentlist = C_NULL_PTR
  !! Inout
  TYPE(C_PTR) :: segmentmarkerlist = C_NULL_PTR
  !! Inout
  INTEGER(C_INT) :: numberofsegments = 0
  !! Inout

  TYPE(C_PTR) :: holelist = C_NULL_PTR
  !! In, but pointer to array copied out
  INTEGER(C_INT) :: numberofholes = 0
  !! In, but copied out

  TYPE(C_PTR) :: regionlist = C_NULL_PTR
  !! In, but pointer to array copied out
  INTEGER(C_INT) :: numberofregions = 0
  !! In but copied out

  TYPE(C_PTR) :: edgelist = C_NULL_PTR
  !! Out only
  TYPE(C_PTR) :: edgemarkerlist = C_NULL_PTR
  !! Not used with Voronoi diagram, out only
  TYPE(C_PTR) :: normlist = C_NULL_PTR
  !! Used only with Voronoi diagram, out only
  INTEGER(C_INT) :: numberofedges = 0
  !! Out only

END TYPE TriangulateIO_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  ! void triangulate(char *, struct triangulateio *, struct triangulateio *,
  ! struct triangulateio *)
  SUBROUTINE Triangulate(triswitches, in, out, vorout) &
    BIND(c, name='triangulate')
    IMPORT :: C_CHAR, TriangulateIO_
    CHARACTER(kind=C_CHAR), INTENT(IN) :: triswitches
    TYPE(TriangulateIO_), INTENT(INOUT) :: in
    TYPE(TriangulateIO_), INTENT(INOUT) :: out
    TYPE(TriangulateIO_), INTENT(INOUT) :: vorout
  END SUBROUTINE Triangulate
END INTERFACE

!----------------------------------------------------------------------------
!                                                             TriangleReport
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE TriangleReport(io, markers, reporttriangles, &
                  reportneighbors, reportsegments, reportedges, reportnorms) &
    BIND(c, name="report")
    IMPORT :: TriangulateIO_, C_INT
    TYPE(TriangulateIO_), INTENT(IN) :: io
    INTEGER(C_INT), VALUE, INTENT(IN) :: markers
    INTEGER(C_INT), VALUE, INTENT(IN) :: reporttriangles
    INTEGER(C_INT), VALUE, INTENT(IN) :: reportneighbors
    INTEGER(C_INT), VALUE, INTENT(IN) :: reportsegments
    INTEGER(C_INT), VALUE, INTENT(IN) :: reportedges
    INTEGER(C_INT), VALUE, INTENT(IN) :: reportnorms
  END SUBROUTINE TriangleReport
END INTERFACE

!----------------------------------------------------------------------------
!                                                             TriangleReport
!----------------------------------------------------------------------------

INTERFACE TriangleFree
  SUBROUTINE TriangleFree1(io) BIND(c, name="trianglefree")
    IMPORT :: TriangulateIO_
    TYPE(TriangulateIO_), INTENT(INOUT) :: io
  END SUBROUTINE TriangleFree1
END INTERFACE TriangleFree

!----------------------------------------------------------------------------
!                                                             TriangleReport
!----------------------------------------------------------------------------

INTERFACE TriangleFree
  SUBROUTINE TriangleFree2(io) BIND(c, name="trifree")
    IMPORT :: C_PTR
    TYPE(C_PTR), VALUE, INTENT(IN) :: io
  END SUBROUTINE TriangleFree2
END INTERFACE TriangleFree

!----------------------------------------------------------------------------
!                                                     TriangleReport@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE TriangleDeallocate(obj)
    TYPE(TriangulateIO_), INTENT(INOUT) :: obj
  END SUBROUTINE TriangleDeallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                      TriangleSetPointList
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE TriangleSetParam(obj, pointList, pointattributelist, &
                   pointmarkerlist, numberofpoints, numberofpointattributes, &
        trianglelist, triangleattributelist, trianglearealist, neighborlist, &
numberoftriangles, numberofcorners, numberoftriangleattributes, segmentlist, &
   segmentmarkerlist, numberofsegments, holelist, numberofholes, regionlist, &
           numberofregions, edgelist, edgemarkerlist, normlist, numberofedges)
    TYPE(TriangulateIO_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, TARGET, INTENT(IN) :: pointList(:)
    REAL(DFP), OPTIONAL, TARGET, INTENT(IN) :: pointattributelist(:)
    INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: pointmarkerlist(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberofpoints
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberofpointattributes
    INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: trianglelist(:)
    REAL(DFP), OPTIONAL, TARGET, INTENT(IN) :: triangleattributelist(:)
    REAL(DFP), OPTIONAL, TARGET, INTENT(IN) :: trianglearealist(:)
    INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: neighborlist(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberoftriangles
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberofcorners
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberoftriangleattributes
    INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: segmentlist(:)
    INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: segmentmarkerlist(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberofsegments
    REAL(DFP), OPTIONAL, TARGET, INTENT(IN) :: holelist(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberofholes
    REAL(DFP), OPTIONAL, TARGET, INTENT(IN) :: regionlist(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberofregions
    INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: edgelist(:)
    INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: edgemarkerlist(:)
    REAL(DFP), OPTIONAL, TARGET, INTENT(IN) :: normlist(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numberofedges
  END SUBROUTINE TriangleSetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                      TriangleSetPointList
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE TriangleGetParam(obj, pointlist, pointattributelist, &
                   pointmarkerlist, numberofpoints, numberofpointattributes, &
        trianglelist, triangleattributelist, trianglearealist, neighborlist, &
numberoftriangles, numberofcorners, numberoftriangleattributes, segmentlist, &
   segmentmarkerlist, numberofsegments, holelist, numberofholes, regionlist, &
           numberofregions, edgelist, edgemarkerlist, normlist, numberofedges)
    TYPE(TriangulateIO_), INTENT(IN) :: obj
    REAL(DFP), OPTIONAL, POINTER, INTENT(INOUT) :: pointlist(:)
    REAL(DFP), OPTIONAL, POINTER, INTENT(INOUT) :: pointattributelist(:)
    INTEGER(I4B), OPTIONAL, POINTER, INTENT(INOUT) :: pointmarkerlist(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: numberofpoints
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: numberofpointattributes
    INTEGER(I4B), OPTIONAL, POINTER, INTENT(INOUT) :: trianglelist(:)
    REAL(DFP), OPTIONAL, POINTER, INTENT(INOUT) :: triangleattributelist(:)
    REAL(DFP), OPTIONAL, POINTER, INTENT(INOUT) :: trianglearealist(:)
    INTEGER(I4B), OPTIONAL, POINTER, INTENT(INOUT) :: neighborlist(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: numberoftriangles
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: numberofcorners
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: numberoftriangleattributes
    INTEGER(I4B), OPTIONAL, POINTER, INTENT(INOUT) :: segmentlist(:)
    INTEGER(I4B), OPTIONAL, POINTER, INTENT(INOUT) :: segmentmarkerlist(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: numberofsegments
    REAL(DFP), OPTIONAL, POINTER, INTENT(INOUT) :: holelist(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: numberofholes
    REAL(DFP), OPTIONAL, POINTER, INTENT(INOUT) :: regionlist(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: numberofregions
    INTEGER(I4B), OPTIONAL, POINTER, INTENT(INOUT) :: edgelist(:)
    INTEGER(I4B), OPTIONAL, POINTER, INTENT(INOUT) :: edgemarkerlist(:)
    REAL(DFP), OPTIONAL, POINTER, INTENT(INOUT) :: normlist(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: numberofedges
  END SUBROUTINE TriangleGetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                       TriangleNullify
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE TriangleNullify(obj)
    TYPE(TriangulateIO_), INTENT(INOUT) :: obj
  END SUBROUTINE TriangleNullify
END INTERFACE

!----------------------------------------------------------------------------
!                                                     TriangleDisplay
!----------------------------------------------------------------------------

INTERFACE Display
  MODULE SUBROUTINE TriangleDisplay(obj, msg, unitno)
    TYPE(TriangulateIO_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE TriangleDisplay
END INTERFACE Display

END MODULE TriangleInterface
