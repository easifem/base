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

MODULE DOF_IOMethods
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: RealVector_, DOF_

IMPLICIT NONE
PRIVATE

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 June 2021
! summary:         Display content of [[dof_]]

INTERFACE
  MODULE SUBROUTINE dof_Display1(obj, msg, UnitNo)
    CLASS(DOF_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
  END SUBROUTINE dof_Display1
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE dof_Display1
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 June 2021
! summary: Display content of fortran vec with [[DOF_]] object info
!
!
!## Usage
!
!```fortran
! ! [[DOF_]]
! PROGRAM main
! USE easifemBase
! IMPLICIT NONE
! TYPE( DOF_ ) :: obj
! REAL( DFP ), ALLOCATABLE :: val( : )
! ! main
! ! #DOF/Initiate
! CALL Initiate( obj, tNodes=[10], names=["U"], spacecompo=[3],  &
!   & timecompo=[1], storageFMT = FMT_DOF )
! ! #DOF/Initiate
! CALL Initiate( Val=val, obj=obj )
! val(1:10) = 1; val(11:20)=2; val(21:)=3
! CALL Display( Val, obj, "CALL Initiate( Val=val, obj=obj ) : " )
! ! #DOF/Deallocate
! CALL Deallocate( obj )
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE dof_Display2(Vec, obj, msg, unitno)
    REAL(DFP), INTENT(IN) :: Vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE dof_Display2
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE dof_Display2
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 June 2021
! summary: Display content of fortran vec with [[DOF_]] object info

INTERFACE
  MODULE SUBROUTINE dof_Display3(Vec, obj, msg, unitno)
    CLASS(RealVector_), INTENT(IN) :: Vec
    CLASS(DOF_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE dof_Display3
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE dof_Display3
END INTERFACE Display

END MODULE DOF_IOMethods
