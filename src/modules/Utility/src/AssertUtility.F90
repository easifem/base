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
!                                                             Assert@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: If n1 and n2 are same, then return n1, else print error
!
INTERFACE ASSERT_EQ
  MODULE FUNCTION assert_eq2(n1, n2, string) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n1, n2
    CHARACTER(*), INTENT(IN) :: string
    INTEGER(I4B) :: ans
  END FUNCTION assert_eq2
END INTERFACE ASSERT_EQ

!----------------------------------------------------------------------------
!                                                             Assert@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: If n1, n2, n3 are same, then return n1, else print error
!
INTERFACE ASSERT_EQ
  MODULE FUNCTION assert_eq3(n1, n2, n3, string) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n1, n2, n3
    CHARACTER(*), INTENT(IN) :: string
    INTEGER(I4B) :: ans
  END FUNCTION assert_eq3
END INTERFACE ASSERT_EQ

!----------------------------------------------------------------------------
!                                                             Assert@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: If n1, n2, n3, n4 are same, then return n1, else print error
!
INTERFACE ASSERT_EQ
  MODULE FUNCTION assert_eq4(n1, n2, n3, n4, string) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n1, n2, n3, n4
    CHARACTER(*), INTENT(IN) :: string
    INTEGER(I4B) :: ans
  END FUNCTION assert_eq4
END INTERFACE ASSERT_EQ

!----------------------------------------------------------------------------
!                                                             Assert@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: If all nn are same, then return nn(1), else print error
!
INTERFACE ASSERT_EQ
  MODULE FUNCTION assert_eqn(nn, string) RESULT(ans)
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: nn
    CHARACTER(*), INTENT(IN) :: string
    INTEGER(I4B) :: ans
  END FUNCTION assert_eqn
END INTERFACE ASSERT_EQ

!----------------------------------------------------------------------------
!                                                              Assert@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: compare the shape of matrix.
!
! Check the shape of mat is same as s(1:2).
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE AssertUtility, ONLY: Assert
! IMPLICIT NONE
! REAL(DFP) :: mat(4, 4)
! CALL Assert(mat=mat, s=[4, 4], msg="shape of mat is not same as [4,4]", &
!             line=__LINE__, file=__FILE__, routine="main()")
! CALL Assert(mat=mat, s=[5, 5], msg="shape of mat is not same as [5,5]", &
!             line=__LINE__, file=__FILE__, routine="main()")
! END PROGRAM main
!```
INTERFACE ASSERT
  MODULE SUBROUTINE assert_shape_2(mat, s, msg, file, line, routine)
    REAL(DFP), INTENT(IN) :: mat(:, :)
    INTEGER(I4B), INTENT(IN) :: s(2)
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE assert_shape_2
END INTERFACE ASSERT

!----------------------------------------------------------------------------
!                                                             Assert@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: compare the shape of matrix.
!
! Check the shape of mat is same as s(1:3).
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE AssertUtility, ONLY: Assert
! IMPLICIT NONE
! REAL(DFP) :: mat(4, 4, 2)
! CALL Assert(mat=mat, s=[4, 4, 2], msg="shape of mat is not same as [4,4,2]", &
!             line=__LINE__, file=__FILE__, routine="main()")
! CALL Assert(mat=mat, s=[5, 5, 2], msg="shape of mat is not same as [5,5,2]", &
!             line=__LINE__, file=__FILE__, routine="main()")
! END PROGRAM main
!```
INTERFACE ASSERT
  MODULE SUBROUTINE assert_shape_3(Mat, s, msg, file, line, routine)
    REAL(DFP), INTENT(IN) :: mat(:, :, :)
    INTEGER(I4B), INTENT(IN) :: s(3)
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE assert_shape_3
END INTERFACE ASSERT

!----------------------------------------------------------------------------
!                                                             Assert@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: compare the shape of matrix.
!
! Check the shape of mat is same as s(1:4).
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE AssertUtility, ONLY: Assert
! IMPLICIT NONE
! REAL(DFP) :: mat(4, 4, 2, 2)
! CALL Assert(mat=mat, s=[4, 4, 2, 2], &
!             msg="shape of mat is not same as [4,4,2,2]", &
!             line=__LINE__, file=__FILE__, routine="main()")
! CALL Assert(mat=mat, s=[5, 5, 2, 2], &
!             msg="shape of mat is not same as [5,5,2,2]", &
!             line=__LINE__, file=__FILE__, routine="main()")
! END PROGRAM main
!```
INTERFACE ASSERT
  MODULE SUBROUTINE assert_shape_4(mat, s, msg, file, line, routine)
    REAL(DFP), INTENT(IN) :: mat(:, :, :, :)
    INTEGER(I4B), INTENT(IN) :: s(4)
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE assert_shape_4
END INTERFACE ASSERT

!----------------------------------------------------------------------------
!                                                             Assert@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: compare the n1 and n2.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE AssertUtility, ONLY: Assert
! IMPLICIT NONE
! CALL Assert(n1=1, n2=2, msg="n1 and n2 are not same", &
!             line=__LINE__, file=__FILE__, routine="main()")
! CALL Assert(n1=1, n2=1, n3=1, &
!             msg="n1, n2, n3 are not same", &
!             line=__LINE__, file=__FILE__, routine="main()")
! CALL Assert(n1=1, n2=1, n3=1, n4=1, &
!             msg="n1, n2, n3, n4 are not same", &
!             line=__LINE__, file=__FILE__, routine="main()")
! END PROGRAM main
!```
INTERFACE ASSERT
  MODULE SUBROUTINE assert_2(n1, n2, msg, file, line, routine)
    INTEGER(I4B), INTENT(IN) :: n1, n2
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE assert_2
END INTERFACE ASSERT

!----------------------------------------------------------------------------
!                                                             Assert@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: compare the n1, n2, n3.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE AssertUtility, ONLY: Assert
! IMPLICIT NONE
! CALL Assert(n1=1, n2=2, msg="n1 and n2 are not same", &
!             line=__LINE__, file=__FILE__, routine="main()")
! CALL Assert(n1=1, n2=1, n3=1, &
!             msg="n1, n2, n3 are not same", &
!             line=__LINE__, file=__FILE__, routine="main()")
! CALL Assert(n1=1, n2=1, n3=1, n4=1, &
!             msg="n1, n2, n3, n4 are not same", &
!             line=__LINE__, file=__FILE__, routine="main()")
! END PROGRAM main
!```
INTERFACE ASSERT
  MODULE SUBROUTINE assert_3(n1, n2, n3, msg, file, line, routine)
    INTEGER(I4B), INTENT(IN) :: n1, n2, n3
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE assert_3
END INTERFACE ASSERT

!----------------------------------------------------------------------------
!                                                             Assert@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: compare the n1, n2, n3, n4.
!
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE AssertUtility, ONLY: Assert
! IMPLICIT NONE
! CALL Assert(n1=1, n2=2, msg="n1 and n2 are not same", &
!             line=__LINE__, file=__FILE__, routine="main()")
! CALL Assert(n1=1, n2=1, n3=1, &
!             msg="n1, n2, n3 are not same", &
!             line=__LINE__, file=__FILE__, routine="main()")
! CALL Assert(n1=1, n2=1, n3=1, n4=1, &
!             msg="n1, n2, n3, n4 are not same", &
!             line=__LINE__, file=__FILE__, routine="main()")
! END PROGRAM main
!```
INTERFACE ASSERT

  MODULE SUBROUTINE assert_4(n1, n2, n3, n4, msg, file, line, routine)
    INTEGER(I4B), INTENT(IN) :: n1, n2, n3, n4
    INTEGER(I4B), INTENT(IN) :: line
    CHARACTER(*), INTENT(IN) :: msg, file, routine
  END SUBROUTINE assert_4
END INTERFACE ASSERT

!----------------------------------------------------------------------------
!                                                             Assert@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-17
! summary: compare the all elements of nn.
!
INTERFACE ASSERT
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
