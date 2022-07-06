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
PUBLIC :: HEAD
PUBLIC :: TAIL
PUBLIC :: SPLIT

!----------------------------------------------------------------------------
!                                                     Head@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first element of array `x`.

INTERFACE
  MODULE PURE FUNCTION head_int(x) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: x(:)
    INTEGER(I4B) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE HEAD
  MODULE PROCEDURE head_int
END INTERFACE HEAD

!----------------------------------------------------------------------------
!                                                     Head@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first element of array `x`.

INTERFACE
  MODULE PURE FUNCTION head_real(x) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: Ans
  END FUNCTION head_real
END INTERFACE

INTERFACE HEAD
  MODULE PROCEDURE head_real
END INTERFACE HEAD

!----------------------------------------------------------------------------
!                                                     Head@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first element of array `x`.

INTERFACE
  MODULE PURE FUNCTION head_char(x) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: x
    CHARACTER(LEN=1) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE HEAD
  MODULE PROCEDURE head_char
END INTERFACE HEAD

!----------------------------------------------------------------------------
!                                                     Tail@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION tail_int(x) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: x(:)
    INTEGER(I4B) :: Ans(SIZE(x) - 1)
  END FUNCTION tail_int
END INTERFACE

INTERFACE TAIL
  MODULE PROCEDURE tail_int
END INTERFACE TAIL

!----------------------------------------------------------------------------
!                                                     Tail@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION tail_real(x) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: Ans(SIZE(x) - 1)
  END FUNCTION tail_real
END INTERFACE

INTERFACE TAIL
  MODULE PROCEDURE tail_real
END INTERFACE TAIL

!----------------------------------------------------------------------------
!                                                     Tail@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION tail_char(x) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: x
    CHARACTER(LEN=LEN(x) - 1) :: Ans
  END FUNCTION tail_char
END INTERFACE

INTERFACE TAIL
  MODULE PROCEDURE tail_char
END INTERFACE TAIL

!----------------------------------------------------------------------------
!                                                    SPLIT@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first half of the array `x` if `section == 1`
!
!# Introduction
!
! Returns the first half of the array `x` if `section == 1`, the second half
! of the array `x` if `section == 2`, and an empty array otherwise. If `size
! (x) == 1`,  `split(x, 1)`  returns and empty array,  and `split(x, 2)`
! returns `x(1)`.

INTERFACE
  MODULE PURE FUNCTION split_int(x, section) RESULT(Ans)
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: x !! Input array
    INTEGER(I4B), INTENT(IN) :: section !! Array section to return
    INTEGER(I4B), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION split_int
END INTERFACE

INTERFACE SPLIT
  MODULE PROCEDURE split_int
END INTERFACE SPLIT

!----------------------------------------------------------------------------
!                                                    SPLIT@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first half of the array `x` if `section == 1`,
!
!# Introduction
!
! Returns the first half of the array `x` if `section == 1`, the second half
! of the array `x` if `section == 2`, and an empty array otherwise. If `size
! (x) == 1`,  `split(x, 1)`  returns and empty array,  and `split(x, 2)`
! returns `x(1)`.

INTERFACE
  MODULE PURE FUNCTION split_real(x, section) RESULT(Ans)
    REAL(DFP), DIMENSION(:), INTENT(IN) :: x !! Input array
    INTEGER(I4B), INTENT(IN) :: section !! Array section to return
    REAL(DFP), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION split_real
END INTERFACE

INTERFACE SPLIT
  MODULE PROCEDURE split_real
END INTERFACE SPLIT

!----------------------------------------------------------------------------
!                                                    SPLIT@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first half of the array `x` if `section == 1`,
!
!# Introduction
!
! Returns the first half of the array `x` if `section == 1`, the second half
! of the array `x` if `section == 2`, and an empty array otherwise. If `size
! (x) == 1`,  `split(x, 1)`  returns and empty array,  and `split(x, 2)`
! returns `x(1)`.

INTERFACE
  MODULE PURE FUNCTION split_char(x, section) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: x !! Input array
    INTEGER(I4B), INTENT(IN) :: section !! Array section to return
    CHARACTER(LEN=:), ALLOCATABLE :: Ans
  END FUNCTION split_char
END INTERFACE

INTERFACE SPLIT
  MODULE PROCEDURE split_char
END INTERFACE SPLIT
