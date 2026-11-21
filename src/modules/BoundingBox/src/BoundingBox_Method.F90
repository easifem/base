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
! date: 2021-02-23
! summary: Methods for the data type [[BoundingBox_]]
!
!# Introduction
! This module has the methods for the data type [[BoundingBox_]]. The
! implementations are in these submodules:
!
! - `BoundingBox_Method@ConstructorMethods`
! - `BoundingBox_Method@GetMethods`
! - `BoundingBox_Method@SetMethods`
! - `BoundingBox_Method@IOMethods`
! - `BoundingBox_Method@TomlMethods`

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
! date: 2021-02-23
! summary:         This subroutine initiates an instance of [[BoundingBox_]].
!
! This subroutine sets the spatial dimension `nsd` and the extent of
! `obj` from `lim`.
! It also sets the box lengths `obj%l` to
! `[xmax - xmin, ymax - ymin, zmax - zmin]`.
!
! The array `lim` holds the extent in this order:
! `[xmin, xmax, ymin, ymax, zmin, zmax]`.
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Initiate_test_1.F90)

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
! date: 2021-02-23
! summary: Copies a [[BoundingBox_]] into another [[BoundingBox_]]
!
! This subroutine copies `nsd`, `box` and `l` from `Anotherobj` into `obj`.
! It also works as the assignment `obj = Anotherobj` and as `Copy`.
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Initiate_test_1.F90)

INTERFACE Initiate
  MODULE PURE SUBROUTINE Initiate_2(obj, Anotherobj)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    !! Bounding box to fill
    TYPE(BoundingBox_), INTENT(IN) :: Anotherobj
    !! Bounding box to copy
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
! date: 2021-02-23
! summary: Copies a vector of boxes into another vector of boxes
!
! This subroutine copies `Anotherobj(ii)` into `obj(ii)` for each
! entry of `Anotherobj`.
! It also works as the assignment `obj = Anotherobj` and as `Copy`.
!
! @warning
! The routine does not check sizes. `obj` must have at least
! `SIZE(Anotherobj)` entries.
! @endwarning
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Initiate_test_1.F90)

INTERFACE Initiate
  MODULE PURE SUBROUTINE Initiate_3(obj, Anotherobj)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj(:)
    !! Vector of boxes to fill, `SIZE(obj) >= SIZE(Anotherobj)`
    TYPE(BoundingBox_), INTENT(IN) :: Anotherobj(:)
    !! Vector of boxes to copy
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
! date: 2021-02-23
! summary: Appends a vector of boxes at the end of a vector of boxes
!
! This subroutine adds the boxes in `VALUE` after the last entry of `obj`.
! If `obj` is not allocated, it allocates `obj` with `SIZE(VALUE)`
! and copies `VALUE` into it.
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Append_test_1.F90)

INTERFACE Append
  MODULE PURE SUBROUTINE Append_1(obj, VALUE)
    TYPE(BoundingBox_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Vector of boxes, grows by `SIZE(VALUE)`
    TYPE(BoundingBox_), INTENT(IN) :: VALUE(:)
    !! Boxes to append
  END SUBROUTINE Append_1
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                     BoundingBox@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary:         Function to create [[BoundingBox_]] instance
!
! This function returns a [[BoundingBox_]] with spatial dimension `nsd`
! and the extent given in `lim`.
!
! The array `lim` holds the extent in this order:
! `[xmin, xmax, ymin, ymax, zmin, zmax]`.
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/BoundingBox_test_1.F90)
!
! See also: [[BoundingBox_Method:Initiate]]

INTERFACE BoundingBox
  MODULE PURE FUNCTION Constructor1(nsd, lim) RESULT(Ans)
    TYPE(BoundingBox_) :: Ans
    !! Bounding box
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Spatial dimension
    REAL(DFP), INTENT(IN) :: lim(6)
    !! Extent `[xmin, xmax, ymin, ymax, zmin, zmax]`
  END FUNCTION Constructor1
END INTERFACE BoundingBox

!----------------------------------------------------------------------------
!                                                     BoundingBox@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary:         This function creates an instance of [[BoundingBox_]]
!
! This function returns a copy of `Anotherobj`.
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/BoundingBox_test_1.F90)
!
! See also: [[BoundingBox_Method:Initiate]]

INTERFACE BoundingBox
  MODULE PURE FUNCTION Constructor2(Anotherobj) RESULT(Ans)
    TYPE(BoundingBox_) :: Ans
    !! Copy of `Anotherobj`
    TYPE(BoundingBox_), INTENT(IN) :: Anotherobj
    !! Bounding box to copy
  END FUNCTION Constructor2
END INTERFACE BoundingBox

!----------------------------------------------------------------------------
!                                                                BoundingBox
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary:         This function creates an instance of [[BoundingBox_]]
!
! This function returns the smallest [[BoundingBox_]] that contains
! all the points in `xij`.
!
! The spatial dimension `nsd` is `SIZE(xij, 1)`.
! Directions beyond `nsd` have zero limits.
!
! @note
! `nsd` must be 1, 2 or 3. For a larger value, all limits stay zero.
! @endnote
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/BoundingBox_test_1.F90)
!
! See also: [[BoundingBox_Method:Initiate]]

INTERFACE BoundingBox
  MODULE PURE FUNCTION Constructor3(xij) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Nodal coordinates `xij(1:nsd, 1:tnodes)`
    TYPE(BoundingBox_) :: Ans
    !! Bounding box of the points
  END FUNCTION Constructor3
END INTERFACE BoundingBox

!----------------------------------------------------------------------------
!                                            BoundingBox_Pointer@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Returns a pointer to a new [[BoundingBox_]] built from limits
!
! This function allocates a new [[BoundingBox_]].
! It sets the box from `nsd` and `lim`, as [[BoundingBox_Method:Initiate]]
! does. It returns a pointer to the new box.
!
! The array `lim` holds the extent in this order:
! `[xmin, xmax, ymin, ymax, zmin, zmax]`.
!
! @warning
! The caller owns the pointer. Call `DEALLOCATE` on it when you no
! longer need it. Otherwise the memory leaks.
! @endwarning
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/BoundingBox_Pointer_test_1.F90)
!
! See also: [[BoundingBox_Method:Initiate]]

INTERFACE BoundingBox_Pointer
  MODULE FUNCTION Constructor_1(nsd, lim) RESULT(Ans)
    TYPE(BoundingBox_), POINTER :: Ans
    !! Newly allocated bounding box
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Spatial dimension
    REAL(DFP), INTENT(IN) :: lim(6)
    !! Extent `[xmin, xmax, ymin, ymax, zmin, zmax]`
  END FUNCTION Constructor_1
END INTERFACE BoundingBox_Pointer

!----------------------------------------------------------------------------
!                                    BoundingBox_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Returns a pointer to a new copy of a [[BoundingBox_]]
!
! This function allocates a new [[BoundingBox_]].
! It copies `nsd`, `box` and `l` from `Anotherobj` into it.
! It returns a pointer to the new box.
!
! @warning
! The caller owns the pointer. Call `DEALLOCATE` on it when you no
! longer need it. Otherwise the memory leaks.
! @endwarning
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/BoundingBox_Pointer_test_1.F90)
!
! See also: [[BoundingBox_Method:Initiate]]

INTERFACE BoundingBox_Pointer
  MODULE FUNCTION Constructor_2(Anotherobj) RESULT(Ans)
    TYPE(BoundingBox_), POINTER :: Ans
    !! Newly allocated copy of `Anotherobj`
    TYPE(BoundingBox_), INTENT(IN) :: Anotherobj
    !! Bounding box to copy
  END FUNCTION Constructor_2
END INTERFACE BoundingBox_Pointer

!----------------------------------------------------------------------------
!                                                Deallocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary: Resets a bounding box to its empty state
!
! Sets `obj%nsd`, `obj%box` and `obj%l` to zero.
! The type has no allocatable components, so it frees no memory.
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Deallocate_test_1.F90)
!
! See also: [[BoundingBox_Method:Initiate]]

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE BB_Deallocate(obj)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    !! Bounding box to reset
  END SUBROUTINE BB_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                     Deallocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Deallocate vector of bounding box
!
! Resets each element of `obj`, then deallocates the vector.
! If `obj` is not allocated, it does nothing.
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Deallocate_test_1.F90)
!
! See also: [[BoundingBox_Method:Reallocate]]

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE BB_Deallocate2(obj)
    TYPE(BoundingBox_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Vector of bounding boxes to deallocate
  END SUBROUTINE BB_Deallocate2
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                     Reallocate@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-25
! summary: Reallocate the bounding box if necessary
!
! Makes `obj` a vector of size `tsize`.
!
! - If `obj` is not allocated, it allocates `obj(tsize)`.
! - If the size of `obj` differs from `tsize`, it deallocates `obj`
!   and allocates it again.
! - If the size already equals `tsize`, nothing changes.
!
! @note
! A new allocation discards the old values.
! The new elements have default values.
! When the size is already `tsize`, the values stay as they are.
! @endnote
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Reallocate_test_1.F90)
!
! See also: [[BoundingBox_Method:Deallocate]]

INTERFACE Reallocate
  MODULE PURE SUBROUTINE obj_Reallocate(obj, tsize)
    TYPE(BoundingBox_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Vector of bounding boxes
    INTEGER(I4B), INTENT(IN) :: tsize
    !! Required size of `obj`
  END SUBROUTINE obj_Reallocate
END INTERFACE Reallocate

!----------------------------------------------------------------------------
!                                                        Display@Constructor
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary:         This subroutine displays the content of [[BoundingBox_]]
!
! Writes `msg`, then these values of `obj`, to the unit `unitNo`:
! `NSD`, `Xmin`, `Xmax`, `Ymin`, `Ymax`, `Zmin`, `Zmax`, `Lx`, `Ly`
! and `Lz`.
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Display_test_1.F90)

INTERFACE Display
  MODULE SUBROUTINE display_obj(obj, msg, unitno)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box to display
    CHARACTER(*), INTENT(IN) :: msg
    !! Message written before the values
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! Output unit; default is standard output
  END SUBROUTINE display_obj
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                         setXmin@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary:         This subroutine set the Xmin in bounding box
!
! Sets `obj%box(1, 1)` to `Val`.
! This routine is not public.
!
! @warning
! It does not update the lengths `obj%l`. Later calls that read `obj%l`
! give wrong results.
! @endwarning

INTERFACE
  MODULE PURE SUBROUTINE setXmin(obj, Val)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    !! Bounding box to change
    REAL(DFP), INTENT(IN) :: Val
    !! New value of Xmin
  END SUBROUTINE setXmin
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setXmax@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary:         This subroutine set the Xmax in bounding box
!
! Sets `obj%box(2, 1)` to `Val`.
! This routine is not public.
!
! @warning
! It does not update the lengths `obj%l`. Later calls that read `obj%l`
! give wrong results.
! @endwarning

INTERFACE
  MODULE PURE SUBROUTINE setXmax(obj, Val)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    !! Bounding box to change
    REAL(DFP), INTENT(IN) :: Val
    !! New value of Xmax
  END SUBROUTINE setXmax
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setYmin@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary:         This subroutine set the Ymin in bounding box
!
! Sets `obj%box(1, 2)` to `Val`.
! This routine is not public.
!
! @warning
! It does not update the lengths `obj%l`. Later calls that read `obj%l`
! give wrong results.
! @endwarning

INTERFACE
  MODULE PURE SUBROUTINE setYmin(obj, Val)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    !! Bounding box to change
    REAL(DFP), INTENT(IN) :: Val
    !! New value of Ymin
  END SUBROUTINE setYmin
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setYmax@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary:         This subroutine set the Ymax of bounding box
!
! Sets `obj%box(2, 2)` to `Val`.
! This routine is not public.
!
! @warning
! It does not update the lengths `obj%l`. Later calls that read `obj%l`
! give wrong results.
! @endwarning

INTERFACE
  MODULE PURE SUBROUTINE setYmax(obj, Val)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    !! Bounding box to change
    REAL(DFP), INTENT(IN) :: Val
    !! New value of Ymax
  END SUBROUTINE setYmax
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setZmin@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary:         This subroutine set the Zmin of bounding box
!
! Sets `obj%box(1, 3)` to `Val`.
! This routine is not public.
!
! @warning
! It does not update the lengths `obj%l`. Later calls that read `obj%l`
! give wrong results.
! @endwarning

INTERFACE
  MODULE PURE SUBROUTINE setZmin(obj, Val)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    !! Bounding box to change
    REAL(DFP), INTENT(IN) :: Val
    !! New value of Zmin
  END SUBROUTINE setZmin
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setZmax@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary:         This subroutine set the Zmax of bounding box
!
! Sets `obj%box(2, 3)` to `Val`.
! This routine is not public.
!
! @warning
! It does not update the lengths `obj%l`. Later calls that read `obj%l`
! give wrong results.
! @endwarning

INTERFACE
  MODULE PURE SUBROUTINE setZmax(obj, Val)
    TYPE(BoundingBox_), INTENT(INOUT) :: obj
    !! Bounding box to change
    REAL(DFP), INTENT(IN) :: Val
    !! New value of Zmax
  END SUBROUTINE setZmax
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getXmin@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Returns the lower limit of the box in the x direction
!
! Returns `obj%box(1, 1)`.
! Use it as the operator `.Xmin.`.
!
! Returns:
!   Ans : REAL(DFP) - Xmin of the box
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Xmin_test_1.F90)
!
! See also: [[getXmax]]

INTERFACE OPERATOR(.Xmin.)
  MODULE PURE FUNCTION getXmin(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box to query
    REAL(DFP) :: Ans
    !! Xmin of the box
  END FUNCTION getXmin
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getXmax@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Returns the upper limit of the box in the x direction
!
! Returns `obj%box(2, 1)`.
! Use it as the operator `.Xmax.`.
!
! Returns:
!   Ans : REAL(DFP) - Xmax of the box
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Xmin_test_1.F90)
!
! See also: [[getXmin]]

INTERFACE OPERATOR(.Xmax.)
  MODULE PURE FUNCTION getXmax(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box to query
    REAL(DFP) :: Ans
    !! Xmax of the box
  END FUNCTION getXmax
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getYmin@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Returns the lower limit of the box in the y direction
!
! Returns `obj%box(1, 2)`.
! Use it as the operator `.Ymin.`.
!
! Returns:
!   Ans : REAL(DFP) - Ymin of the box
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Xmin_test_1.F90)
!
! See also: [[getYmax]]

INTERFACE OPERATOR(.Ymin.)
  MODULE PURE FUNCTION getYmin(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box to query
    REAL(DFP) :: Ans
    !! Ymin of the box
  END FUNCTION getYmin
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getYmax@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Returns the upper limit of the box in the y direction
!
! Returns `obj%box(2, 2)`.
! Use it as the operator `.Ymax.`.
!
! Returns:
!   Ans : REAL(DFP) - Ymax of the box
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Xmin_test_1.F90)
!
! See also: [[getYmin]]

INTERFACE OPERATOR(.Ymax.)
  MODULE PURE FUNCTION getYmax(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box to query
    REAL(DFP) :: Ans
    !! Ymax of the box
  END FUNCTION getYmax
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getZmin@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Returns the lower limit of the box in the z direction
!
! Returns `obj%box(1, 3)`.
! Use it as the operator `.Zmin.`.
!
! Returns:
!   Ans : REAL(DFP) - Zmin of the box
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Xmin_test_1.F90)
!
! See also: [[getZmax]]

INTERFACE OPERATOR(.Zmin.)
  MODULE PURE FUNCTION getZmin(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box to query
    REAL(DFP) :: Ans
    !! Zmin of the box
  END FUNCTION getZmin
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getZmax@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Returns the upper limit of the box in the z direction
!
! Returns `obj%box(2, 3)`.
! Use it as the operator `.Zmax.`.
!
! Returns:
!   Ans : REAL(DFP) - Zmax of the box
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Xmin_test_1.F90)
!
! See also: [[getZmin]]

INTERFACE OPERATOR(.Zmax.)
  MODULE PURE FUNCTION getZmax(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box to query
    REAL(DFP) :: Ans
    !! Zmax of the box
  END FUNCTION getZmax
END INTERFACE

!----------------------------------------------------------------------------
!                                                  isIntersectInX@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Checks if two boxes overlap in the x direction
!
! Returns `.TRUE.` if the Xmin or the Xmax of `obj2` lies in the
! closed interval from Xmin to Xmax of `obj`.
! Boxes that only touch at an end point count as overlapping.
!
! Returns:
!   Ans : LOGICAL(LGT) - `.TRUE.` if the boxes overlap in x
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/isIntersect_test_1.F90)
!
! See also: [[isIntersectInY]], [[isIntersectInZ]], [[is_intersect]]

INTERFACE isIntersectInX
  MODULE PURE FUNCTION is_intersect_in_X(obj, obj2) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj, obj2
    !! `obj` is the first bounding box, `obj2` is the second
    LOGICAL(LGT) :: Ans
    !! `.TRUE.` if the boxes overlap in x
  END FUNCTION is_intersect_in_X
END INTERFACE isIntersectInX

!----------------------------------------------------------------------------
!                                                  isIntersectInY@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Checks if two boxes overlap in the y direction
!
! Returns `.TRUE.` if the Ymin or the Ymax of `obj2` lies in the
! closed interval from Ymin to Ymax of `obj`.
! Boxes that only touch at an end point count as overlapping.
!
! Returns:
!   Ans : LOGICAL(LGT) - `.TRUE.` if the boxes overlap in y
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/isIntersect_test_1.F90)
!
! See also: [[isIntersectInX]], [[isIntersectInZ]], [[is_intersect]]

INTERFACE isIntersectInY
  MODULE PURE FUNCTION is_intersect_in_Y(obj, obj2) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj, obj2
    !! `obj` is the first bounding box, `obj2` is the second
    LOGICAL(LGT) :: Ans
    !! `.TRUE.` if the boxes overlap in y
  END FUNCTION is_intersect_in_Y
END INTERFACE isIntersectInY

!----------------------------------------------------------------------------
!                                                 isIntersectInZ@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Checks if two boxes overlap in the z direction
!
! Returns `.TRUE.` if the Zmin or the Zmax of `obj2` lies in the
! closed interval from Zmin to Zmax of `obj`.
! Boxes that only touch at an end point count as overlapping.
!
! Returns:
!   Ans : LOGICAL(LGT) - `.TRUE.` if the boxes overlap in z
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/isIntersect_test_1.F90)
!
! See also: [[isIntersectInX]], [[isIntersectInY]], [[is_intersect]]

INTERFACE isIntersectInZ
  MODULE PURE FUNCTION is_intersect_in_Z(obj, obj2) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj, obj2
    !! `obj` is the first bounding box, `obj2` is the second
    LOGICAL(LGT) :: Ans
    !! `.TRUE.` if the boxes overlap in z
  END FUNCTION is_intersect_in_Z
END INTERFACE isIntersectInZ

!----------------------------------------------------------------------------
!                                                    isIntersect@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-12
! summary: Checks if two bounding boxes intersect
!
! Returns `.TRUE.` if the boxes overlap in the x, y and z directions.
! Use it as the operator `.isIntersect.` or the generic `isIntersect`.
!
! The function always checks all three directions, whatever the value
! of `nsd`. For 2D boxes the z limits are both 0, so the z check passes.
!
! Returns:
!   Ans : LOGICAL(LGT) - `.TRUE.` if the boxes intersect
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/isIntersect_test_1.F90)
!
! See also: [[isIntersectInX]], [[isIntersectInY]], [[isIntersectInZ]]

INTERFACE OPERATOR(.isIntersect.)
  MODULE PURE FUNCTION is_intersect(obj, obj2) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj, obj2
    !! `obj` is the first bounding box, `obj2` is the second
    LOGICAL(LGT) :: Ans
    !! `.TRUE.` if the boxes intersect
  END FUNCTION is_intersect
END INTERFACE

INTERFACE isIntersect
  MODULE PROCEDURE is_intersect
END INTERFACE isIntersect

!----------------------------------------------------------------------------
!                                                         isEmpty@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-11
! summary: Checks if the bounding box is empty
!
! Returns `.TRUE.` if the box is empty.
!
! The box is not empty if its length is positive in at least one
! direction. Otherwise it is empty.
!
! Returns:
!   ans : LOGICAL(LGT) - `.TRUE.` if the box is empty
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/isEmpty_test_1.F90)
!
! See also: [[Intersection]]

INTERFACE isEmpty
  MODULE PURE FUNCTION bbox_isEmpty(obj) RESULT(ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box
    LOGICAL(LGT) :: ans
    !! `.TRUE.` if the box is empty
  END FUNCTION bbox_isEmpty
END INTERFACE isEmpty

!----------------------------------------------------------------------------
!                                                  getIntersection@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Returns the intersection of two bounding boxes
!
! Returns the box common to both boxes. Use it as the operator
! `.Intersection.` or the generic `Intersection`.
!
! In each direction, the lower limit is the larger of the two lower
! limits. The upper limit is the smaller of the two upper limits. The
! result has `nsd` equal to the larger `nsd` of the two boxes.
!
! @warning
! The function does not check that the boxes overlap. For disjoint boxes
! the lower limit is above the upper limit, so the length is negative.
! Use [[isIntersect]] first.
! @endwarning
!
! Returns:
!   Ans : `BoundingBox_` - intersection of `obj` and `obj2`
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Intersection_test_1.F90)
!
! See also: [[Union]], [[isIntersect]], [[isEmpty]]

INTERFACE OPERATOR(.Intersection.)
  MODULE PURE FUNCTION get_intersection(obj, obj2) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj, obj2
    !! `obj` is the first bounding box, `obj2` is the second
    TYPE(BoundingBox_) :: Ans
    !! Intersection of the two boxes
  END FUNCTION get_intersection
END INTERFACE

INTERFACE Intersection
  MODULE PROCEDURE get_intersection
END INTERFACE Intersection

!----------------------------------------------------------------------------
!                                                         getUnion@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Returns the union of two bounding boxes
!
! Returns the smallest box that contains both boxes. Use it as the
! operator `.UNION.` or the generic `Union`.
!
! In each direction, the lower limit is the smaller of the two lower
! limits. The upper limit is the larger of the two upper limits. The
! result has `nsd` equal to the larger `nsd` of the two boxes.
!
! @note
! The function always uses all three directions. A default box, with all
! limits 0, adds the origin to the union.
! @endnote
!
! Returns:
!   Ans : `BoundingBox_` - union of `obj` and `obj2`
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Union_test_1.F90)
!
! See also: [[Intersection]]

INTERFACE OPERATOR(.UNION.)
  MODULE PURE FUNCTION get_Union(obj, obj2) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj, obj2
    !! `obj` is the first bounding box, `obj2` is the second
    TYPE(BoundingBox_) :: Ans
    !! Union of the two boxes
  END FUNCTION get_Union
END INTERFACE

INTERFACE Union
  MODULE PROCEDURE get_Union
END INTERFACE Union

!----------------------------------------------------------------------------
!                                                        getCenter@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Returns the center of the bounding box
!
! Returns the center of the box as a vector of size 3. Use it as the
! operator `.Center.` or the generic `Center`.
!
! Each entry is the mean of the lower and upper limits in that
! direction. Unused directions give 0.
!
! Returns:
!   Ans : REAL(DFP) - coordinates of the center, size 3
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Center_test_1.F90)

INTERFACE OPERATOR(.Center.)
  MODULE PURE FUNCTION get_Center(obj) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box
    REAL(DFP) :: Ans(3)
    !! Coordinates of the center
  END FUNCTION get_Center
END INTERFACE

INTERFACE Center
  MODULE PROCEDURE get_Center
END INTERFACE Center

!----------------------------------------------------------------------------
!                                                         isInside@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-23
! summary: Checks if a point is inside the bounding box
!
! Returns `.TRUE.` if the point lies inside the box. Use it as the
! operator `.isInside.` or the generic `isInside`.
!
! The function compares each coordinate `Val(ii)` with the limits of
! the box in direction `ii`, for `ii = 1, SIZE(Val)`. The comparison is
! approximate, so points on the boundary are inside.
!
! @warning
! `SIZE(Val)` must be at most 3. The function does not check this.
! A larger size reads outside the data of the box.
! @endwarning
!
! Returns:
!   Ans : LOGICAL(LGT) - `.TRUE.` if the point is inside the box
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/isInside_test_1.F90)
!
! See also: [[Nptrs]]

INTERFACE OPERATOR(.isInside.)
  MODULE PURE FUNCTION is_Inside(obj, Val) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box
    REAL(DFP), INTENT(IN) :: Val(:)
    !! Coordinates of the point, `SIZE(Val)` is at most 3
    LOGICAL(LGT) :: Ans
    !! `.TRUE.` if the point is inside the box
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
! summary: Returns the indices of the points located inside the box
!
! Returns the column indices of `xij` whose points lie inside the box.
! Use it as the operator `.Nptrs.`.
!
! The indices are in increasing order. The result is an empty array if
! no point is inside.
!
! Points on the boundary count as inside. See [[isInside]].
!
! Returns:
!   Ans : INTEGER(I4B), ALLOCATABLE - indices of the points inside
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/Nptrs_test_1.F90)
!
! See also: [[isInside]]

INTERFACE OPERATOR(.Nptrs.)
  MODULE PURE FUNCTION get_nptrs(obj, xij) RESULT(Ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Coordinates of the points, `xij(1:nsd, 1:tnodes)`.
    !! Each column is one point, and `nsd` must be at most 3.
    INTEGER(I4B), ALLOCATABLE :: Ans(:)
    !! Indices of the points inside the box
  END FUNCTION get_nptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetDiameter@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-03-03
! summary: Returns the diameter of the box
!
! Returns the length of the box diagonal.
!
! The function reads `obj%l` only. The result is stale if the limits
! changed without updating `obj%l`.
!
! Returns:
!   ans : REAL(DFP) - diameter of the box
!
! Formula:
!   $$ ans = \sqrt{l_1^2 + l_2^2 + l_3^2} $$
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/GetDiameter_test_1.F90)
!
! See also: [[GetDiameterSqr]], [[GetRadius]]

INTERFACE GetDiameter
  MODULE PURE FUNCTION bbox_GetDiameter(obj) RESULT(ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box
    REAL(DFP) :: ans
    !! Diameter of the box
  END FUNCTION bbox_GetDiameter
END INTERFACE GetDiameter

!----------------------------------------------------------------------------
!                                                    GetRadius@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Returns the radius of the box
!
! Returns half of the diameter of the box.
!
! The function reads `obj%l` only. The result is stale if the limits
! changed without updating `obj%l`.
!
! Returns:
!   ans : REAL(DFP) - radius of the box
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/GetDiameter_test_1.F90)
!
! See also: [[GetDiameter]], [[GetRadiusSqr]]

INTERFACE GetRadius
  MODULE PURE FUNCTION bbox_GetRadius(obj) RESULT(ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box
    REAL(DFP) :: ans
    !! Radius of the box
  END FUNCTION bbox_GetRadius
END INTERFACE GetRadius

!----------------------------------------------------------------------------
!                                                   GetDiameterSqr@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Returns the square of the diameter of the box
!
! Returns the square of the diameter, which is the squared length of the
! box diagonal. This avoids a square root.
!
! The function reads `obj%l` only. The result is stale if the limits
! changed without updating `obj%l`.
!
! Returns:
!   ans : REAL(DFP) - square of the diameter
!
! Formula:
!   $$ ans = l_1^2 + l_2^2 + l_3^2 $$
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/GetDiameter_test_1.F90)
!
! See also: [[GetDiameter]], [[GetRadiusSqr]]

INTERFACE GetDiameterSqr
  MODULE PURE FUNCTION bbox_GetDiameterSqr(obj) RESULT(ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box
    REAL(DFP) :: ans
    !! Square of the diameter
  END FUNCTION bbox_GetDiameterSqr
END INTERFACE GetDiameterSqr

!----------------------------------------------------------------------------
!                                                   GetRadiusSqr@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Returns the square of the radius of the box
!
! Returns the square of the radius, which is half the diagonal of the
! box. This avoids a square root.
!
! The function reads `obj%l` only. The result is stale if the limits
! changed without updating `obj%l`.
!
! Returns:
!   ans : REAL(DFP) - square of the radius
!
! Formula:
!   $$ ans = \frac{1}{4} (l_1^2 + l_2^2 + l_3^2) $$
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/GetDiameter_test_1.F90)
!
! See also: [[GetRadius]], [[GetDiameterSqr]]

INTERFACE GetRadiusSqr
  MODULE PURE FUNCTION bbox_GetRadiusSqr(obj) RESULT(ans)
    TYPE(BoundingBox_), INTENT(IN) :: obj
    !! Bounding box
    REAL(DFP) :: ans
    !! Square of the radius
  END FUNCTION bbox_GetRadiusSqr
END INTERFACE GetRadiusSqr

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary: Reads one bounding box from a TOML table
!
! This routine reads the keys `xmin`, `ymin`, `zmin`, `xmax`, `ymax` and
! `zmax` directly from `table`. A missing key gives 0. The box is always
! three dimensional (`nsd = 3`).
!
! @note
! The routine ignores the argument `key`. It reads the six keys from
! `table` itself, so pass the table that holds them.
! @endnote
!
! The routine reads the six keys one after another. `stat` and `origin`
! describe the last read. The routine does not use `isFound`.
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/GetValue_test_1.F90)
!
! See also: [[GetValue]]

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_bbox_r0(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    !! TOML table that holds the six limit keys
    CHARACTER(*), INTENT(IN) :: key
    !! Ignored. The routine does not use `key`.
    TYPE(BoundingBox_), INTENT(INOUT) :: VALUE
    !! Bounding box read from the table
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    !! Origin of the TOML data, from the last read
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    !! Status of the last read
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    !! Not used by this routine
  END SUBROUTINE toml_get_bbox_r0
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary: Reads an array of bounding boxes from a TOML table
!
! This routine reads an array of bounding boxes from the array named `key`
! in `table`.
!
! 1. It deallocates `VALUE`.
! 2. It looks up the array `key` in `table`.
! 3. If the array exists, it allocates `VALUE` with one entry per array
!    element.
! 4. It reads each element, an inline table, with the scalar
!    `GetValue` for [[BoundingBox_]].
!
! If `key` is missing, `VALUE` stays unallocated.
!
! @warning
! Every element of the array must be a table. For any other element
! type, the scalar routine receives a null table and the program
! crashes.
! @endwarning
!
! Examples
!
! - [test 1](|base|BoundingBox/examples/GetValue_test_1.F90)
!
! See also: [[GetValue]]

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_bbox_r1(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    !! TOML table that holds the array
    CHARACTER(*), INTENT(IN) :: key
    !! Name of the array of bounding boxes
    TYPE(BoundingBox_), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    !! Bounding boxes read from the array
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    !! Origin of the TOML data, set by the reader
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    !! Status of the read
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    !! True if the array `key` exists in `table`
  END SUBROUTINE toml_get_bbox_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BoundingBox_Method
