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
MODULE PURE SUBROUTINE iterdata_Initiate( obj, MaxIter, IterationNumber, &
  & ResidualError0, ResidualError, ResidualTolerance, SolutionError0, &
  & SolutionError, SolutionTolerance, ConvergenceType, &
  & ConvergenceIn, NormType, Converged, TimeAtStart, TimeAtEnd )
  TYPE( IterationData_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: MaxIter
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: IterationNumber
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: ResidualError0
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: ResidualError
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: ResidualTolerance
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: SolutionError0
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: SolutionError
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: SolutionTolerance
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ConvergenceType
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ConvergenceIn
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: NormType
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: Converged
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: TimeAtStart
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: TimeAtEnd
END SUBROUTINE iterdata_Initiate
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE iterdata_Initiate
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                              Deallocate
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE iterdata_Deallocate( obj )
  TYPE( IterationData_ ), INTENT( INOUT ) :: obj
END SUBROUTINE iterdata_Deallocate
END INTERFACE

INTERFACE Deallocate
  MODULE PROCEDURE iterdata_Deallocate
END INTERFACE Deallocate

PUBLIC :: Deallocate

!----------------------------------------------------------------------------
!                                                    isConverged@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION iterdata_isConverged( obj ) RESULT( Ans )
  TYPE( IterationData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: Ans
END FUNCTION iterdata_isConverged
END INTERFACE

INTERFACE isConverged
  MODULE PROCEDURE iterdata_isConverged
END INTERFACE isConverged

PUBLIC :: isConverged

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE iterdata_Display( obj, msg, UnitNo )
  TYPE( IterationData_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE iterdata_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE iterdata_Display
END INTERFACE Display

PUBLIC :: Display

END MODULE IterationData_Method