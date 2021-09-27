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

SUBMODULE( IterationData_Method ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE iterdata_Initiate
  IF( PRESENT( MaxIter ) ) obj%MaxIter = MaxIter
  IF( PRESENT( IterationNumber ) ) obj%IterationNumber = IterationNumber
  IF( PRESENT( ResidualError0 ) ) obj%ResidualError0 = ResidualError0
  IF( PRESENT( ResidualError ) ) obj%ResidualError = ResidualError
  IF( PRESENT( ResidualTolerance ) ) obj%ResidualTolerance = ResidualTolerance
  IF( PRESENT( SolutionError0 ) ) obj%SolutionError0 = SolutionError0
  IF( PRESENT( SolutionError ) ) obj%SolutionError = SolutionError
  IF( PRESENT( SolutionTolerance ) ) obj%SolutionTolerance = SolutionTolerance
  IF( PRESENT( ConvergenceType ) ) obj%ConvergenceType = ConvergenceType
  IF( PRESENT( ConvergenceIn ) ) obj%ConvergenceIn = ConvergenceIn
  IF( PRESENT( NormType ) ) obj%NormType = NormType
  IF( PRESENT( Converged ) ) obj%Converged = Converged
  IF( PRESENT( TimeAtStart ) ) obj%TimeAtStart = TimeAtStart
  IF( PRESENT( TimeAtEnd ) ) obj%TimeAtEnd = TimeAtEnd
END PROCEDURE iterdata_Initiate

!----------------------------------------------------------------------------
!                                                           DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE iterdata_DeallocateData
  obj%MaxIter = 0
  obj%IterationNumber = 0
  obj%ResidualError = 0.0
  obj%ResidualError0 = 0.0
  obj%ResidualTolerance = 0.0
  obj%SolutionError = 0.0
  obj%SolutionError0 = 0.0
  obj%SolutionTolerance = 0.0
  obj%ConvergenceType = 0
  obj%ConvergenceIn = 0
  obj%NormType = 0
  obj%Converged = .FALSE.
  obj%TimeAtStart = 0.0
  obj%TimeAtEnd = 0.0
END PROCEDURE iterdata_DeallocateData
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE iterdata_isConverged
  ! SELECT CASE( obj%ConvergenceType )
  ! CASE( RelativeConvergence )
  !   IF( obj%ErrorAtEnd .LE. obj%Tolerance * obj%ErrorAtStart ) THEN
  !     Ans = .TRUE.
  !   ELSE
  !     Ans = .FALSE.
  !   END IF
  ! CASE( AbsoluteConvergence )
  !   IF( obj%ErrorAtEnd .LE. obj%Tolerance ) THEN
  !     Ans = .TRUE.
  !   ELSE
  !     Ans = .FALSE.
  !   END IF
  ! END SELECT
END PROCEDURE iterdata_isConverged

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Constructor