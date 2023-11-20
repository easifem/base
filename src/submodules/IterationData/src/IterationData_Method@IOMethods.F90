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
SUBMODULE(IterationData_Method) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE iterdata_Display
CALL Display(TRIM(msg), unitno)
CALL Display(obj%maxIter, 'maxIter: ', unitno)
CALL Display(obj%iterationNumber, 'iterationNumber: ', unitno)
CALL Display(obj%residualError0, 'residualError0: ', unitno)
CALL Display(obj%residualError, 'residualError: ', unitno)
CALL Display(obj%residualTolerance, 'residualTolerance: ', unitno)
CALL Display(obj%solutionError0, 'solutionError0: ', unitno)
CALL Display(obj%solutionError, 'solutionError: ', unitno)
CALL Display(obj%solutionTolerance, 'solutionTolerance: ', unitno)
CALL Display(obj%convergenceType, 'convergenceType: ', unitno)
CALL Display(obj%convergenceIn, 'convergenceIn: ', unitno)
CALL Display(obj%normType, 'normType: ', unitno)
CALL Display(obj%converged, 'converged: ', unitno)
CALL Display(obj%timeAtStart, 'timeAtStart: ', unitno)
CALL Display(obj%timeAtEnd, 'timeAtEnd: ', unitno)
END PROCEDURE iterdata_Display

END SUBMODULE IOMethods
