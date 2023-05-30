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

MODULE RealVector_ComparisonMethods
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
PUBLIC :: OPERATOR(.EQ.)

!----------------------------------------------------------------------------
!                                                                        EQ
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION realvec_isEqual(obj, obj2) RESULT(Ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    CLASS(RealVector_), INTENT(IN) :: obj2
    LOGICAL(LGT) :: ans
  END FUNCTION realvec_isEqual
END INTERFACE

INTERFACE OPERATOR(.EQ.)
  MODULE PROCEDURE realvec_isEqual
END INTERFACE OPERATOR(.EQ.)

END MODULE RealVector_ComparisonMethods
