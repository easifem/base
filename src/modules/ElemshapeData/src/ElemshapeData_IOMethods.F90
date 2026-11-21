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
! date: 2021-03-04
! summary: Input and output methods of ElemShapeData_
!
! Display element shape data, or encode it as markdown.

MODULE ElemshapeData_IOMethods
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: STElemShapeData_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE String_Class, ONLY: String
IMPLICIT NONE

PRIVATE

PUBLIC :: Display
PUBLIC :: ElemshapeData_MdEncode
PUBLIC :: MdEncode
PUBLIC :: ElemshapeData_ReactEncode

!----------------------------------------------------------------------------
!                                                             Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Display the content of ElemShapeData_
!
! Writes the content of `obj` to the unit `unitNo`.
! The default unit is the standard output.
!
! The output holds these items in order.
!
! 1. The message `msg`.
! 2. The sizes `nsd`, `xidim`, `nns` and `nips`.
! 3. The arrays `N`, `dNdXi`, `dNdXt`, `jacobian`, `js`, `ws`,
!    `thickness`, `coord` and `normal`.
!
! For an array that is not allocated, it writes `NOT ALLOCATED`.
! It does not write `NLaplacian`.
!
! If `obj` is an `STElemShapeData_`, the output ends with the time
! data `jt`, `wt`, `T`, `dTdTheta`, `dNTdt` and `dNTdXt`.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/Display_test_1.F90)
!
! See also: [[elemsd_display_2]]

INTERFACE Display
  MODULE SUBROUTINE elemsd_display_1(obj, msg, unitNo)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    CHARACTER(*), INTENT(IN) :: msg
    !! message written first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
    !! output unit; default is the standard output
  END SUBROUTINE elemsd_display_1
END INTERFACE Display

!----------------------------------------------------------------------------
!                                              ElemshapeData_MdEncode@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Markdown encoding of ElemShapeData_
!
! Returns the content of `obj` as markdown text.
!
! - `N`, `js`, `thickness` and `normal` each get one table.
! - `dNdXi`, `dNdXt` and `jacobian` get one table per quadrature
!   point.
!
! An array that is not allocated appears as `Not ALLOCATED`.
!
! The arrays `ws`, `coord` and `NLaplacian` are not encoded.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/MdEncode_test_1.F90)

INTERFACE MdEncode
  MODULE FUNCTION ElemshapeData_MdEncode(obj) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(String) :: ans
    !! encoded text
  END FUNCTION ElemshapeData_MdEncode
END INTERFACE MdEncode

!----------------------------------------------------------------------------
!                                           ElemshapeData_ReactEncode@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: React encoding of ElemShapeData_ (not implemented)
!
! @warning
! It is not implemented. It returns an empty `String`.
! @endwarning
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/ReactEncode_test_1.F90)

INTERFACE
  MODULE FUNCTION ElemshapeData_ReactEncode(obj) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(String) :: ans
    !! encoded text
  END FUNCTION ElemshapeData_ReactEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-04
! summary: Display the content of space-time shape data
!
! Displays every element of `obj`, one time point after the other.
! It writes each element like [[elemsd_display_1]] does, including
! the time data.
! The message of element `ii` is `msg` followed by `(ii): `.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/Display_test_2.F90)
!
! See also: [[elemsd_display_1]]

INTERFACE Display
  MODULE SUBROUTINE elemsd_display_2(obj, msg, unitNo)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data at each time point
    CHARACTER(*), INTENT(IN) :: msg
    !! message written first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
    !! output unit; default is the standard output
  END SUBROUTINE elemsd_display_2
END INTERFACE Display

END MODULE ElemshapeData_IOMethods
