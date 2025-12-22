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
! date:         23 Feb 2021
! summary: [[BoundingBox_Method]] module consists method for[[BoundingBox_]]
!
!# Introduction
! This module consists method for data type [[BoundingBox_]]. These methods
! are included in following submoudles:
!- `BoundingBox_Method@Constructor`

MODULE BoundingBox_Method
USE GlobalData, ONLY: DFP, I4B, LGT, stdout
USE BaseType, ONLY: BoundingBox_
USE tomlf, ONLY: toml_table
IMPLICIT NONE

PUBLIC :: OPERATOR(.Center.)
PUBLIC :: OPERATOR(.Intersection.)
PUBLIC :: OPERATOR(.Nptrs.)
PUBLIC :: OPERATOR(.UNION.)
PUBLIC :: OPERATOR(.Xmax.)
PUBLIC :: OPERATOR(.Xmin.)
PUBLIC :: OPERATOR(.Ymax.)
PUBLIC :: OPERATOR(.Ymin.)
PUBLIC :: OPERATOR(.Zmax.)
PUBLIC :: OPERATOR(.Zmin.)
PUBLIC :: OPERATOR(.isInside.)
PUBLIC :: OPERATOR(.isIntersect.)

PUBLIC :: ASSIGNMENT(=)

PUBLIC :: Initiate
PUBLIC :: Copy
PUBLIC :: BoundingBox
PUBLIC :: BoundingBox_Pointer
PUBLIC :: DEALLOCATE
PUBLIC :: Reallocate
PUBLIC :: Display

PUBLIC :: isIntersectInX
PUBLIC :: isIntersectInY
PUBLIC :: isIntersectInZ
PUBLIC :: isIntersect
PUBLIC :: isEmpty
PUBLIC :: Intersection
PUBLIC :: Union
PUBLIC :: Center
PUBLIC :: isInside
PUBLIC :: GetDiameter
PUBLIC :: GetRadius
PUBLIC :: GetDiameterSqr
PUBLIC :: GetRadiusSqr
PUBLIC :: GetValue
PUBLIC :: Append

PRIVATE

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function Initiatea an instance of [[BoundingBox_]].
!
!# Introduction
! This function Initiates an instance of [[BoundingBox_]].
!- `NSD` is the spatial dimension
!- `lim` is vector of real numbers (length=6)
!- `lim(1)` => xmin
!- `lim(2)` => ymin
!- `lim(3)` => zmin
!- `lim(4)` => xmax
!- `lim(5)` => ymax
!- `lim(6)` => zmax
!
!### Usage
!
!```fortran
! subroutine test
!   type(BoundingBox_) :: obj
!   call Initiate( obj, nsd = 2, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.
! 0_DFP, 0.0_DFP] )
!   call display( obj, msg="test1" )
! end subroutine test
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE Initiate_1(obj, nsd, lim)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    !! Instance of bounding box
    INTEGER(I4B), INTENT(IN) :: NSD
    !! Spatial dimension
    REAL(DFP), INTENT(IN) :: lim(6)
    !! Extent of bounding box
  END SUBROUTINE Initiate_1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary: Initiate the instance of [[BoundingBox_]] from the another box
!
!# Introduction
!
! This subroutine Initiate the instance of [[BoundingBox_]] from another
! instance. It is basically a copy command.
!
!### Usage
!
!```fortran
! subroutine test2
!   type(BoundingBox_) :: obj, obj2
!   call Initiate( obj, 2, [0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.
! 0_DFP] )
!   call Initiate(obj2, obj)
!   call display( obj2, msg="test2")
! end subroutine test2
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE Initiate_2(obj, Anotherobj)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    TYPE(BoundingBox_), INTENT(IN) :: Anotherobj
  END SUBROUTINE Initiate_2
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE Initiate_2
END INTERFACE

INTERFACE Copy
  MODULE PROCEDURE Initiate_2
END INTERFACE Copy

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary: Initiate the instance of [[BoundingBox_]] from the another box

INTERFACE Initiate
  MODULE PURE SUBROUTINE Initiate_3(obj, Anotherobj)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj(:)
    TYPE(BoundingBox_), INTENT(IN) :: Anotherobj(:)
  END SUBROUTINE Initiate_3
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE Initiate_3
END INTERFACE

INTERFACE Copy
  MODULE PROCEDURE Initiate_3
END INTERFACE Copy

!----------------------------------------------------------------------------
!                                                Append@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary: Initiate the instance of [[BoundingBox_]] from the another box

INTERFACE Append
  MODULE PURE SUBROUTINE Append_1(obj, VALUE)
    TYPE(BoundingBox_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    TYPE(BoundingBox_), INTENT(IN) :: VALUE(:)
  END SUBROUTINE Append_1
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                     BoundingBox@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         Function to create [[BoundingBox_]] instance
!
!# Introduction
! This function Initiates an instance of [[BoundingBox_]].
!- `NSD` is the spatial dimension
!- `lim` is vector of real numbers (length=6)
!- `lim(1)` => xmin
!- `lim(2)` => ymin
!- `lim(3)` => zmin
!- `lim(4)` => xmax
!- `lim(5)` => ymax
!- `lim(6)` => zmax
!
!### Usage
!```fortran
! subroutine test3
!   type(BoundingBox_) :: obj
!   obj = BoundingBox( nsd = 2, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.
! 0_DFP, 0.0_DFP] )
!   call display( obj, msg="test1" )
! end subroutine test3
!```

INTERFACE BoundingBox
  MODULE PURE FUNCTION Constructor1(nsd, lim) RESULT(Ans)
    TYPE(BoundingBox_) :: Ans
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN) :: lim(6)
  END FUNCTION Constructor1
END INTERFACE BoundingBox

!----------------------------------------------------------------------------
!                                                     BoundingBox@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function creates an instance of [[BoundingBox_]]
!
!# Introduction
!This function creates an intance of [[BoundingBox_]].
!
!### Usage
!```fortran
! subroutine test4
!   type(BoundingBox_) :: obj, obj2
!   call Initiate( obj, 2, [0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.
! 0_DFP] )
!   obj2 = BoundingBox(obj)
!   call display( obj2, msg="test2")
! end subroutine test4
!```

INTERFACE BoundingBox
  MODULE PURE FUNCTION Constructor2(Anotherobj) RESULT(Ans)
    TYPE(BoundingBox_) :: Ans
    TYPE(BoundingBox_), INTENT(IN) :: Anotherobj
  END FUNCTION Constructor2
END INTERFACE BoundingBox

!----------------------------------------------------------------------------
!                                                                BoundingBox
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function creates an instance of [[BoundingBox_]]
!
!# Introduction
! This function creates an instance of [[BoundingBox_]]. In this function NSD
! is determined from SIZE(xij, 1).
!
!### Usage
!```fortran
! subroutine test5
!   type(BoundingBox_) :: obj
!   obj = boundingBox(RESHAPE([0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.
! 0_DFP], [2,3]))
!   call display(obj, "test5")
! end subroutine test5
!```

INTERFACE BoundingBox
  MODULE PURE FUNCTION Constructor3(xij) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Nodal coordinates xij( 1:nsd, 1:tnodes )
    TYPE(BoundingBox_) :: Ans
    !!
  END FUNCTION Constructor3
END INTERFACE BoundingBox

!----------------------------------------------------------------------------
!                                            BoundingBox_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function returns the pointer to [[BoundingBox_]] instance
!
!# Introduction
!
! This function returns the pointer to [[BoundingBox_]] instance.
!- `NSD` is the spatial dimension
!- `lim` is vector of real numbers (length=6)
!- `lim(1)` => xmin
!- `lim(2)` => ymin
!- `lim(3)` => zmin
!- `lim(4)` => xmax
!- `lim(5)` => ymax
!- `lim(6)` => zmax
!
!### Usage
!
!```fortran
! subroutine test6
!   type(BoundingBox_) :: obj
!   type(BoundingBox_), pointer :: obj2
!   call Initiate( obj, 2, [0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.
! 0_DFP] )
!   obj2 => BoundingBox_Pointer(obj)
!   call display( obj2, msg="test6")
! end subroutine test6
!```

INTERFACE BoundingBox_Pointer
  MODULE FUNCTION Constructor_1(nsd, lim) RESULT(Ans)
    TYPE(BoundingBox_), POINTER :: Ans
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN) :: lim(6)
  END FUNCTION Constructor_1
END INTERFACE BoundingBox_Pointer

!----------------------------------------------------------------------------
!                                    BoundingBox_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: This function returns the pointer to an instance of [[BoundingBox_]]
!
!# Introduction
! This function returns the pointer to an instance of [[BoundingBox_]] by
! copying contents from `Anotherobj`
!
!### Usage
!```fortran
! subroutine test7
!   type(BoundingBox_), pointer :: obj
!   obj => BoundingBox_Pointer(nsd=3, lim=[0.0_DFP, 1.0_DFP, 0.0_DFP, 1.
! 0_DFP, 0.0_DFP, 0.0_DFP])
!   call display(obj, "test7")
! end subroutine test7
!```

INTERFACE BoundingBox_Pointer
  MODULE FUNCTION Constructor_2(Anotherobj) RESULT(Ans)
    TYPE(BoundingBox_), POINTER :: Ans
    TYPE(BoundingBox_), INTENT(IN) :: Anotherobj
  END FUNCTION Constructor_2
END INTERFACE BoundingBox_Pointer

!----------------------------------------------------------------------------
!                                                Deallocate@Constructor
!----------------------------------------------------------------------------

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE BB_Deallocate(obj)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
  END SUBROUTINE BB_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                     Deallocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Deallocate vector of bounding box

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE BB_Deallocate2(obj)
    TYPE(BoundingBox_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE BB_Deallocate2
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                     Reallocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-25
! summary: Reallocate the bounding box if necessary

INTERFACE Reallocate
  MODULE PURE SUBROUTINE obj_Reallocate(obj, tsize)
    TYPE(BoundingBox_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: tsize
  END SUBROUTINE obj_Reallocate
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                        Display@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This subroutine displays the content of [[BoundingBox_]]

INTERFACE Display
  MODULE SUBROUTINE display_obj(obj, msg, unitno)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE display_obj
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                         setXmin@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary:         This subroutine set the Xmin in bounding box

INTERFACE
  MODULE PURE SUBROUTINE setXmin(obj, Val)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val
  END SUBROUTINE setXmin
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setXmax@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This subroutine set the Xmax in bounding box

INTERFACE
  MODULE PURE SUBROUTINE setXmax(obj, Val)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val
  END SUBROUTINE setXmax
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setYmin@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This subroutine set the Ymin in bounding box

INTERFACE
  MODULE PURE SUBROUTINE setYmin(obj, Val)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val
  END SUBROUTINE setYmin
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setYmax@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This subroutine set the Ymax of bounding box

INTERFACE
  MODULE PURE SUBROUTINE setYmax(obj, Val)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val
  END SUBROUTINE setYmax
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setZmin@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This subroutine set the Zmin of bounding box

INTERFACE
  MODULE PURE SUBROUTINE setZmin(obj, Val)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val
  END SUBROUTINE setZmin
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setZmax@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This subroutine set the Zmax of bounding box

INTERFACE
  MODULE PURE SUBROUTINE setZmax(obj, Val)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: Val
  END SUBROUTINE setZmax
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getXmin@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function returns the xmin
!
!### Usage
!
!```fortran
! xmin = .xmin. obj
!```

INTERFACE OPERATOR(.Xmin.)
  MODULE PURE FUNCTION getXmin(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    REAL(DFP) :: Ans
  END FUNCTION getXmin
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getXmax@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function returns the xmax
!
!### Usage
!
!```fortran
! xmax = .xmax. obj
!```

INTERFACE OPERATOR(.Xmax.)
  MODULE PURE FUNCTION getXmax(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    REAL(DFP) :: Ans
  END FUNCTION getXmax
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getYmin@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function returns the ymin
!
!### Usage
!
!```fortran
! ymin = .ymin. obj
!```

INTERFACE OPERATOR(.Ymin.)
  MODULE PURE FUNCTION getYmin(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    REAL(DFP) :: Ans
  END FUNCTION getYmin
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getYmax@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function returns the ymax
!
!### Usage
!
!```fortran
! ymax = .ymax. obj
!```

INTERFACE OPERATOR(.Ymax.)
  MODULE PURE FUNCTION getYmax(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    REAL(DFP) :: Ans
  END FUNCTION getYmax
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getZmin@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function returns the zmin
!
!### Usage
!
!```fortran
! zmin = .zmin. obj
!```

INTERFACE OPERATOR(.Zmin.)
  MODULE PURE FUNCTION getZmin(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    REAL(DFP) :: Ans
  END FUNCTION getZmin
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getZmax@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function returns the zmax
!
!### Usage
!
!```fortran
! zmax = .zmax. obj
!```

INTERFACE OPERATOR(.Zmax.)
  MODULE PURE FUNCTION getZmax(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    REAL(DFP) :: Ans
  END FUNCTION getZmax
END INTERFACE

!----------------------------------------------------------------------------
!                                                  isIntersectInX@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function checks if two bounding boxes interesect in x direction
!
!@todo
!### Usage
!@endtodo

INTERFACE isIntersectInX
  MODULE PURE FUNCTION is_intersect_in_X(obj, obj2) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj, obj2
    LOGICAL(LGT) :: Ans
  END FUNCTION is_intersect_in_X
END INTERFACE isIntersectInX

!----------------------------------------------------------------------------
!                                                  isIntersectInY@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function checks if two bounding boxes interesect in y direction
!
!@todo
!### Usage
!@endtodo

INTERFACE isIntersectInY
  MODULE PURE FUNCTION is_intersect_in_Y(obj, obj2) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj, obj2
    LOGICAL(LGT) :: Ans
  END FUNCTION is_intersect_in_Y
END INTERFACE isIntersectInY

!----------------------------------------------------------------------------
!                                                 isIntersectInZ@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function checks if two bounding boxes interesect in z direction
!
!@todo
!### Usage
!@endtodo

INTERFACE isIntersectInZ
  MODULE PURE FUNCTION is_intersect_in_Z(obj, obj2) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj, obj2
    LOGICAL(LGT) :: Ans
  END FUNCTION is_intersect_in_Z
END INTERFACE isIntersectInZ

!----------------------------------------------------------------------------
!                                                    isIntersect@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-12
! summary: This function checks if two bounding boxes interesect each other

INTERFACE OPERATOR(.isIntersect.)
  MODULE PURE FUNCTION is_intersect(obj, obj2) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj, obj2
    LOGICAL(LGT) :: Ans
  END FUNCTION is_intersect
END INTERFACE

INTERFACE isIntersect
  MODULE PROCEDURE is_intersect
END INTERFACE isIntersect

!----------------------------------------------------------------------------
!                                                         isEmpty@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-11
! summary:  Checks if bounding box is empty

INTERFACE isEmpty
  MODULE PURE FUNCTION bbox_isEmpty(obj) RESULT(ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION bbox_isEmpty
END INTERFACE isEmpty

!----------------------------------------------------------------------------
!                                                  getIntersection@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: This function returns the intersection bounding
! box of two bounding box
!
!# Introduction
! This function returns the bounding box which is formed by the
! intersection of two bounding box
!
!@todo
![] add usage
!@endtodo

INTERFACE OPERATOR(.Intersection.)
  MODULE PURE FUNCTION get_intersection(obj, obj2) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj, obj2
    TYPE(BoundingBox_) :: Ans
  END FUNCTION get_intersection
END INTERFACE

INTERFACE Intersection
  MODULE PROCEDURE get_intersection
END INTERFACE Intersection

!----------------------------------------------------------------------------
!                                                         getUnion@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: This function returns the union of two bounding box
!
!# Introduction
! This function returns the bounding box which is formed by the union
! of two bounding box.
!
!@todo
![] add usage
!@endtodo

INTERFACE OPERATOR(.UNION.)
  MODULE PURE FUNCTION get_Union(obj, obj2) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj, obj2
    TYPE(BoundingBox_) :: Ans
  END FUNCTION get_Union
END INTERFACE

INTERFACE Union
  MODULE PROCEDURE get_Union
END INTERFACE Union

!----------------------------------------------------------------------------
!                                                        getCenter@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This function returns the center of bounding box
!
!# Introduction
!
!This function returns the centern of bounding box.
!
!@todo
![] add usage
!@endtodo

INTERFACE OPERATOR(.Center.)
  MODULE PURE FUNCTION get_Center(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    REAL(DFP) :: Ans(3)
  END FUNCTION get_Center
END INTERFACE

INTERFACE Center
  MODULE PROCEDURE get_Center
END INTERFACE Center

!----------------------------------------------------------------------------
!                                                         isInside@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: This function checks if a point is inside the bounding box or not
!
!# Introduction
!
! This function checks if a point is inside a bounding box or not
!
!@todo
![] add usage
!@endtodo

INTERFACE OPERATOR(.isInside.)
  MODULE PURE FUNCTION is_Inside(obj, Val) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: Val(:)
    LOGICAL(LGT) :: Ans
  END FUNCTION is_Inside
END INTERFACE

INTERFACE isInside
  MODULE PROCEDURE is_Inside
END INTERFACE isInside

!----------------------------------------------------------------------------
!                                                         getNptrs@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-11
! summary: This function returns the node numbers located inside
! the bounding box
!
!# Introduction
!
! This function returns the list of node numbers which are inside
! the bounding box

INTERFACE OPERATOR(.Nptrs.)
  MODULE PURE FUNCTION get_nptrs(obj, xij) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), ALLOCATABLE :: Ans(:)
  END FUNCTION get_nptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetDiameter@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2022
! summary: Returns the diameter of the box

INTERFACE GetDiameter
  MODULE PURE FUNCTION bbox_GetDiameter(obj) RESULT(ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION bbox_GetDiameter
END INTERFACE GetDiameter

!----------------------------------------------------------------------------
!                                                    GetRadius@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Returns the Radius of the box

INTERFACE GetRadius
  MODULE PURE FUNCTION bbox_GetRadius(obj) RESULT(ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION bbox_GetRadius
END INTERFACE GetRadius

!----------------------------------------------------------------------------
!                                                   GetDiameterSqr@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Returns the diameter of the box

INTERFACE GetDiameterSqr
  MODULE PURE FUNCTION bbox_GetDiameterSqr(obj) RESULT(ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION bbox_GetDiameterSqr
END INTERFACE GetDiameterSqr

!----------------------------------------------------------------------------
!                                                   GetRadiusSqr@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Returns the Radius of the box

INTERFACE GetRadiusSqr
  MODULE PURE FUNCTION bbox_GetRadiusSqr(obj) RESULT(ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION bbox_GetRadiusSqr
END INTERFACE GetRadiusSqr

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_bbox_r0(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    !! We dont need table here, so this argument is ignored.
    TYPE(BoundingBox_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_bbox_r0
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_bbox_r1(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    TYPE(BoundingBox_), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_bbox_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BoundingBox_Method
