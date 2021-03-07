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
  IF( PRESENT( MaxIter ) ) Obj % MaxIter = MaxIter
  IF( PRESENT( IterationNumber ) ) Obj % IterationNumber = IterationNumber
  IF( PRESENT( ConvergenceType ) ) Obj % ConvergenceType = ConvergenceType
  IF( PRESENT( ConvergenceIn ) ) Obj % ConvergenceIn = ConvergenceIn
  IF( PRESENT( NormType ) ) Obj % NormType = NormType
  IF( PRESENT( Tolerance ) ) Obj % Tolerance = Tolerance
  IF( PRESENT( ErrorAtEnd ) ) Obj % ErrorAtEnd = ErrorAtEnd
  IF( PRESENT( ErrorAtStart ) ) Obj % ErrorAtStart = ErrorAtStart
  IF( PRESENT( TimeAtStart ) ) Obj % TimeAtStart = TimeAtStart
  IF( PRESENT( TimeAtEnd ) ) Obj % TimeAtEnd = TimeAtEnd
  IF( PRESENT( Converged ) ) Obj % Converged = Converged
END PROCEDURE initiate_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE is_converged
  SELECT CASE( Obj % ConvergenceType )
  CASE( RelativeConvergence )
    IF( Obj % ErrorAtEnd .LE. Obj % Tolerance * Obj % ErrorAtStart ) THEN
      Ans = .TRUE.
    ELSE
      Ans = .FALSE.
    END IF
  CASE( AbsoluteConvergence )
    IF( Obj % ErrorAtEnd .LE. Obj % Tolerance ) THEN
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