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

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Input and output methods for [[IntVector_]]
!
! `Display` prints one [[IntVector_]] or an array of them.

MODULE IntVector_IOMethod
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
PRIVATE
PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Display an array of [[IntVector_]]
!
! Writes `msg` and the size of `obj`.
! Then it displays each vector of `obj`, labelled with its position,
! for example `(2)`.
!
! The output goes to `UnitNo`. If `UnitNo` is absent, the routine
! writes to the standard output.
!
! Examples
!
! - [test 1](|base|IntVector/examples/Display_test_1.F90) with an array
!
! See also: [[obj_Display2]]

INTERFACE Display
  MODULE SUBROUTINE obj_Display1(obj, msg, UnitNo, orient)
    TYPE(IntVector_), INTENT(IN) :: obj(:)
    !! vectors to display
    CHARACTER(*), INTENT(IN) :: msg
    !! title written before the vectors
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
    !! unit to write to; default is the standard output
    CHARACTER(*), OPTIONAL, INTENT(IN) :: orient
    !! layout of the entries, passed to the display of each vector
  END SUBROUTINE obj_Display1
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-25
! summary: Display one [[IntVector_]]
!
! Writes `msg`, then whether `obj%val` is allocated.
! If it is allocated, the routine also writes `tsize`, `capacity` and
! `tDimension`, followed by the array `obj%val`.
!
! The output goes to `unitno`. If `unitno` is absent, the routine
! writes to the standard output.
!
! @note
! The routine writes the whole array `obj%val`. This includes spare
! entries beyond `tsize`.
! @endnote
!
! Examples
!
! - [test 1](|base|IntVector/examples/Display_test_1.F90)
!
! See also: [[obj_Display1]]

INTERFACE Display
  MODULE SUBROUTINE obj_Display2(obj, msg, unitno, orient)
    TYPE(IntVector_), INTENT(IN) :: obj
    !! vector to display
    CHARACTER(*), INTENT(IN) :: msg
    !! title written before the vector
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    !! unit to write to; default is the standard output
    CHARACTER(*), OPTIONAL, INTENT(IN) :: orient
    !! layout of the entries of `obj%val`
  END SUBROUTINE obj_Display2
END INTERFACE Display

END MODULE IntVector_IOMethod
