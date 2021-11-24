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

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION assert_eq2(n1, n2, string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER(I4B), INTENT(IN) :: n1, n2
    INTEGER(I4B) :: assert_eq2
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION assert_eq3(n1, n2, n3, string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER(I4B), INTENT(IN) :: n1, n2, n3
    INTEGER(I4B) :: assert_eq3
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION assert_eq4(n1, n2, n3, n4, string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER(I4B), INTENT(IN) :: n1, n2, n3, n4
    INTEGER(I4B) :: assert_eq4
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION assert_eqn(nn, string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: nn
    INTEGER(I4B) :: assert_eqn
  END FUNCTION
END INTERFACE

INTERFACE assert_eq
  MODULE PROCEDURE assert_eqn, assert_eq2, assert_eq3, assert_eq4
END INTERFACE

PUBLIC :: ASSERT_EQ

!----------------------------------------------------------------------------
!                                                              Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE assert_shape_2(Mat, s, msg, file, line, routine)
    REAL(DFP), INTENT(IN) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(2)
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE assert_shape_3(Mat, s, msg, file, line, routine)
    REAL(DFP), INTENT(IN) :: Mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(3)
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE assert_shape_4(Mat, s, msg, file, line, routine)
    REAL(DFP), INTENT(IN) :: Mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(4)
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE
END INTERFACE

INTERFACE ASSERT
  MODULE PROCEDURE assert_shape_2, assert_shape_3, assert_shape_4
END INTERFACE ASSERT

PUBLIC :: ASSERT
