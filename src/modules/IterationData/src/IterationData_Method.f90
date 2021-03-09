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

MODULE IterationData_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE initiate_obj( Obj, MaxIter, IterationNumber, &
  & Tolerance, ErrorAtStart, ErrorAtEnd, TimeAtStart, TimeAtEnd, &
  & ConvergenceType, ConvergenceIn, NormType, Converged )
  TYPE( IterationData_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: MaxIter
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: IterationNumber
  REAL( DFP ), INTENT( IN ), OPTIONAL :: Tolerance
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: ErrorAtStart
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: ErrorAtEnd
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: TimeAtStart
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: TimeAtEnd
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: ConvergenceType
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: ConvergenceIn
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: NormType
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: Converged
END SUBROUTINE initiate_obj
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_obj
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                    isConverged@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION is_converged( Obj ) RESULT( Ans )
  TYPE( IterationData_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
END FUNCTION is_converged
END INTERFACE

INTERFACE isConverged
  MODULE PROCEDURE is_converged
END INTERFACE isConverged

PUBLIC :: isConverged

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE Display_obj( Obj, msg, UnitNo )
  TYPE( IterationData_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE Display_obj
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE Display_obj
END INTERFACE Display

PUBLIC :: Display

END MODULE IterationData_Method