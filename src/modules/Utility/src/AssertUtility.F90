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

MODULE AssertUtility
USE GlobalData, ONLY: I4B, DFP
IMPLICIT NONE
PRIVATE
PUBLIC :: ASSERT
PUBLIC :: ASSERT_EQ

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE ASSERT_EQ
  MODULE FUNCTION assert_eq2(n1, n2, string)
    INTEGER(I4B), INTENT(IN) :: n1, n2
    CHARACTER(*), INTENT(IN) :: string
    INTEGER(I4B) :: assert_eq2
  END FUNCTION
END INTERFACE ASSERT_EQ

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE ASSERT_EQ
  MODULE FUNCTION assert_eq3(n1, n2, n3, string)
    INTEGER(I4B), INTENT(IN) :: n1, n2, n3
    CHARACTER(*), INTENT(IN) :: string
    INTEGER(I4B) :: assert_eq3
  END FUNCTION
END INTERFACE ASSERT_EQ

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE ASSERT_EQ
  MODULE FUNCTION assert_eq4(n1, n2, n3, n4, string)
    INTEGER(I4B), INTENT(IN) :: n1, n2, n3, n4
    CHARACTER(*), INTENT(IN) :: string
    INTEGER(I4B) :: assert_eq4
  END FUNCTION
END INTERFACE ASSERT_EQ

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE ASSERT_EQ
  MODULE FUNCTION assert_eqn(nn, string)
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: nn
    CHARACTER(*), INTENT(IN) :: string
    INTEGER(I4B) :: assert_eqn
  END FUNCTION
END INTERFACE ASSERT_EQ

!----------------------------------------------------------------------------
!                                                              Assert@Assert
!----------------------------------------------------------------------------

INTERFACE ASSERT
  MODULE SUBROUTINE assert_shape_2(Mat, s, msg, file, line, routine)
    REAL(DFP), INTENT(IN) :: Mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(2)
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE
END INTERFACE ASSERT

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE ASSERT
  MODULE SUBROUTINE assert_shape_3(Mat, s, msg, file, line, routine)
    REAL(DFP), INTENT(IN) :: Mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(3)
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE
END INTERFACE ASSERT

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE ASSERT
  MODULE SUBROUTINE assert_shape_4(Mat, s, msg, file, line, routine)
    REAL(DFP), INTENT(IN) :: Mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(4)
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE
END INTERFACE ASSERT

!----------------------------------------------------------------------------
!                                                             Assert@Assert
!----------------------------------------------------------------------------

INTERFACE ASSERT
  MODULE SUBROUTINE assert_2(n1, n2, msg, file, line, routine)
    INTEGER(I4B), INTENT(IN) :: n1, n2
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE assert_2

  MODULE SUBROUTINE assert_3(n1, n2, n3, msg, file, line, routine)
    INTEGER(I4B), INTENT(IN) :: n1, n2, n3
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE assert_3

  MODULE SUBROUTINE assert_4(n1, n2, n3, n4, msg, file, line, routine)
    INTEGER(I4B), INTENT(IN) :: n1, n2, n3, n4
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE assert_4

  MODULE SUBROUTINE assert_n(nn, msg, file, line, routine)
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: nn
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE assert_n
END INTERFACE ASSERT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AssertUtility
