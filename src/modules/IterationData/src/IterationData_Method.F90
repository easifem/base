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
MODULE PURE SUBROUTINE iterdata_Initiate( obj, maxIter, iterationNumber, &
  & residualError0, residualError, residualTolerance, solutionError0, &
  & solutionError, solutionTolerance, convergenceType, &
  & convergenceIn, normType, converged, timeAtStart, timeAtEnd )
  TYPE( IterationData_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: maxIter
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: iterationNumber
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: residualError0
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: residualError
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: residualTolerance
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: solutionError0
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: solutionError
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: solutionTolerance
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: convergenceType
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: convergenceIn
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: normType
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: converged
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: timeAtStart
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: timeAtEnd
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