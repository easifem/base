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

MODULE ReverseUtility
USE GlobalData, ONLY: I4B, DFP, LGT, REAL32, REAL64, INT8, INT16, INT32, &
                      INT64
IMPLICIT NONE

PRIVATE

PUBLIC :: Reverse

!----------------------------------------------------------------------------
!                                                             Reverse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: This function reverses the elements of an integer array

INTERFACE Reverse
  MODULE SUBROUTINE Reverse_Int8_R1(ans, n1, n2)
    INTEGER(INT8), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
  END SUBROUTINE Reverse_Int8_R1
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                             Reverse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: This function reverses the elements of an integer array

INTERFACE Reverse
  MODULE SUBROUTINE Reverse_Int16_R1(ans, n1, n2)
    INTEGER(INT16), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
  END SUBROUTINE Reverse_Int16_R1
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                             Reverse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: This function reverses the elements of an integer array

INTERFACE Reverse
  MODULE SUBROUTINE Reverse_Int32_R1(ans, n1, n2)
    INTEGER(INT32), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
  END SUBROUTINE Reverse_Int32_R1
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                             Reverse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: This function reverses the elements of an integer array

INTERFACE Reverse
  MODULE SUBROUTINE Reverse_Int64_R1(ans, n1, n2)
    INTEGER(INT64), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
  END SUBROUTINE Reverse_Int64_R1
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                             Reverse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: This function reverses the elements of a real array

INTERFACE Reverse
  MODULE SUBROUTINE Reverse_Real32_R1(ans, n1, n2)
    REAL(REAL32), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
  END SUBROUTINE Reverse_Real32_R1
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                             Reverse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: This function reverses the elements of a real array

INTERFACE Reverse
  MODULE SUBROUTINE Reverse_Real64_R1(ans, n1, n2)
    REAL(REAL64), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(IN) :: n1, n2
  END SUBROUTINE Reverse_Real64_R1
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                             Reverse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: This function reverses the elements of a integer matrix

INTERFACE Reverse
  MODULE SUBROUTINE Reverse_Int8_R2(ans, r1, r2, c1, c2, dim)
    INTEGER(INT8), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2
    !! Extent of ans(r1:r2, c1:c2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the rows
    !! dim=2, reverse the columns
  END SUBROUTINE Reverse_Int8_R2
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                             Reverse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: This function reverses the elements of a integer matrix

INTERFACE Reverse
  MODULE SUBROUTINE Reverse_Int16_R2(ans, r1, r2, c1, c2, dim)
    INTEGER(INT16), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2
    !! Extent of ans(r1:r2, c1:c2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the rows
    !! dim=2, reverse the columns
  END SUBROUTINE Reverse_Int16_R2
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                             Reverse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: This function reverses the elements of a integer matrix

INTERFACE Reverse
  MODULE SUBROUTINE Reverse_Int32_R2(ans, r1, r2, c1, c2, dim)
    INTEGER(INT32), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2
    !! Extent of ans(r1:r2, c1:c2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the rows
    !! dim=2, reverse the columns
  END SUBROUTINE Reverse_Int32_R2
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                             Reverse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: This function reverses the elements of a integer matrix

INTERFACE Reverse
  MODULE SUBROUTINE Reverse_Int64_R2(ans, r1, r2, c1, c2, dim)
    INTEGER(INT64), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2
    !! Extent of ans(r1:r2, c1:c2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the rows
    !! dim=2, reverse the columns
  END SUBROUTINE Reverse_Int64_R2
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                             Reverse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: This function reverses the elements of a real matrix

INTERFACE Reverse
  MODULE SUBROUTINE Reverse_Real32_R2(ans, r1, r2, c1, c2, dim)
    REAL(REAL32), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2
    !! Extent of ans(r1:r2, c1:c2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the rows
    !! dim=2, reverse the columns
  END SUBROUTINE Reverse_Real32_R2
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                             Reverse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: This function reverses the elements of a real matrix

INTERFACE Reverse
  MODULE SUBROUTINE Reverse_Real64_R2(ans, r1, r2, c1, c2, dim)
    REAL(REAL64), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2
    !! Extent of ans(r1:r2, c1:c2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the rows
    !! dim=2, reverse the columns
  END SUBROUTINE Reverse_Real64_R2
END INTERFACE Reverse

!----------------------------------------------------------------------------
!                                                             Reverse@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-14
! summary: This function reverses the elements of a real matrix

INTERFACE Reverse
  MODULE SUBROUTINE Reverse_Real64_R3(ans, r1, r2, c1, c2, d1, d2, dim)
    REAL(REAL64), INTENT(INOUT) :: ans(:, :, :)
    INTEGER(I4B), INTENT(IN) :: r1, r2, c1, c2, d1, d2
    !! Extent of ans(r1:r2, c1:c2, d1:d2)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim=1, reverse the dim1
    !! dim=2, reverse the dim2
    !! dim=3, reverse the dim3
  END SUBROUTINE Reverse_Real64_R3
END INTERFACE Reverse

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ReverseUtility
