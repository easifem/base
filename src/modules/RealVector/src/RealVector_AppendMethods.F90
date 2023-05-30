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

MODULE RealVector_AppendMethods
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
PUBLIC :: Append

!----------------------------------------------------------------------------
!                                                          Append@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         28 Feb 2021
! summary:         This subroutine appends value to [[RealVector_]]

INTERFACE
  MODULE PURE SUBROUTINE realVec_Append1(obj, VALUE)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE realVec_Append1
END INTERFACE

INTERFACE Append
  MODULE PROCEDURE realVec_Append1
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                          Append@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         28 Feb 2021
! summary:         This subroutine appends value to [[RealVector_]]

INTERFACE
  MODULE PURE SUBROUTINE realVec_Append2(obj, VALUE)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE realVec_Append2
END INTERFACE

INTERFACE Append
  MODULE PROCEDURE realVec_Append2
END INTERFACE Append

!----------------------------------------------------------------------------
!                                                          Append@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         28 Feb 2021
! summary:         This subroutine appends value to [[RealVector_]]

INTERFACE
  MODULE PURE SUBROUTINE realVec_Append3(obj, anotherobj)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    CLASS(RealVector_), INTENT(IN) :: anotherobj
  END SUBROUTINE realVec_Append3
END INTERFACE

INTERFACE Append
  MODULE PROCEDURE realVec_Append3
END INTERFACE Append

END MODULE RealVector_AppendMethods
