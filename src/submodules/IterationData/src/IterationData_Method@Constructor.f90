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

MODULE PROCEDURE initiate_obj
  IF( PRESENT( MaxIter ) ) obj%MaxIter = MaxIter
  IF( PRESENT( IterationNumber ) ) obj%IterationNumber = IterationNumber
  IF( PRESENT( ConvergenceType ) ) obj%ConvergenceType = ConvergenceType
  IF( PRESENT( ConvergenceIn ) ) obj%ConvergenceIn = ConvergenceIn
  IF( PRESENT( NormType ) ) obj%NormType = NormType
  IF( PRESENT( Tolerance ) ) obj%Tolerance = Tolerance
  IF( PRESENT( ErrorAtEnd ) ) obj%ErrorAtEnd = ErrorAtEnd
  IF( PRESENT( ErrorAtStart ) ) obj%ErrorAtStart = ErrorAtStart
  IF( PRESENT( TimeAtStart ) ) obj%TimeAtStart = TimeAtStart
  IF( PRESENT( TimeAtEnd ) ) obj%TimeAtEnd = TimeAtEnd
  IF( PRESENT( Converged ) ) obj%Converged = Converged
END PROCEDURE initiate_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE is_converged
  SELECT CASE( obj%ConvergenceType )
  CASE( RelativeConvergence )
    IF( obj%ErrorAtEnd .LE. obj%Tolerance * obj%ErrorAtStart ) THEN
      Ans = .TRUE.
    ELSE
      Ans = .FALSE.
    END IF
  CASE( AbsoluteConvergence )
    IF( obj%ErrorAtEnd .LE. obj%Tolerance ) THEN
      Ans = .TRUE.
    ELSE
      Ans = .FALSE.
    END IF
  END SELECT
END PROCEDURE is_converged

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Constructor