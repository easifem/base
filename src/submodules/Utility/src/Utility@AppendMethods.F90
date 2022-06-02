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

SUBMODULE(Utility) AppendMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_1a
INTEGER(I4B), ALLOCATABLE :: Dummy(:)
INTEGER(I4B) :: n
#include "./Append_1.inc"
END PROCEDURE Append_1a

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_1b
REAL(DFP), ALLOCATABLE :: Dummy(:)
INTEGER(I4B) :: n
#include "./Append_1.inc"
END PROCEDURE Append_1b

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_1c
  INTEGER(I4B) :: n
  n = SIZE(A)
  CALL Reallocate( C, n+1 )
  C(1:n) = A; C(1 + n) = B
END PROCEDURE Append_1c

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_1d
  INTEGER(I4B) :: n
  n = SIZE(A)
  CALL Reallocate( C, n+1 )
  C(1:n) = A; C(1 + n) = B
END PROCEDURE Append_1d

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_2a
INTEGER(I4B), ALLOCATABLE :: Dummy(:)
INTEGER(I4B) :: n, m
#include "./Append_2.inc"
END PROCEDURE Append_2a

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_2b
REAL(DFP), ALLOCATABLE :: Dummy(:)
INTEGER(I4B) :: n, m
#include "./Append_2.inc"
END PROCEDURE Append_2b

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_2c
  INTEGER(I4B) :: n, m
  !!
  IF (SIZE(B) .NE. 0) THEN
    m = SIZE(B)
    n = SIZE(A)
    CALL Reallocate( C, n+m )
    C(1:n) = A
    C(n + 1:) = B
  ELSE
    C = A
  END IF
  !!
END PROCEDURE Append_2c

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_2d
  INTEGER(I4B) :: n, m
  !!
  IF (SIZE(B) .NE. 0) THEN
    m = SIZE(B)
    n = SIZE(A)
    CALL Reallocate( C, n+m )
    C(1:n) = A
    C(n + 1:) = B
  ELSE
    C = A
  END IF
  !!
END PROCEDURE Append_2d

!----------------------------------------------------------------------------
!                                                                 colConcat
!----------------------------------------------------------------------------

module procedure colconcat_1a
#include "./ColConcat_1.inc"
end procedure colconcat_1a

module procedure colconcat_1b
#include "./ColConcat_1.inc"
end procedure colconcat_1b

module procedure colconcat_1c
#include "./ColConcat_1.inc"
end procedure colconcat_1c

module procedure colconcat_1d
#include "./ColConcat_1.inc"
end procedure colconcat_1d

module procedure colconcat_1e
#include "./ColConcat_1.inc"
end procedure colconcat_1e

module procedure colconcat_1f
#include "./ColConcat_1.inc"
end procedure colconcat_1f

!----------------------------------------------------------------------------
!                                                                 colConcat
!----------------------------------------------------------------------------

module procedure colconcat_2a
#include "./ColConcat_2.inc"
end procedure colconcat_2a

module procedure colconcat_2b
#include "./ColConcat_2.inc"
end procedure colconcat_2b

module procedure colconcat_2c
#include "./ColConcat_2.inc"
end procedure colconcat_2c

module procedure colconcat_2d
#include "./ColConcat_2.inc"
end procedure colconcat_2d

module procedure colconcat_2e
#include "./ColConcat_2.inc"
end procedure colconcat_2e

module procedure colconcat_2f
#include "./ColConcat_2.inc"
end procedure colconcat_2f

!----------------------------------------------------------------------------
!                                                                 colConcat
!----------------------------------------------------------------------------

module procedure colconcat_3a
#include "./ColConcat_3.inc"
end procedure colconcat_3a

module procedure colconcat_3b
#include "./ColConcat_3.inc"
end procedure colconcat_3b

module procedure colconcat_3c
#include "./ColConcat_3.inc"
end procedure colconcat_3c

module procedure colconcat_3d
#include "./ColConcat_3.inc"
end procedure colconcat_3d

module procedure colconcat_3e
#include "./ColConcat_3.inc"
end procedure colconcat_3e

module procedure colconcat_3f
#include "./ColConcat_3.inc"
end procedure colconcat_3f

!----------------------------------------------------------------------------
!                                                                 colConcat
!----------------------------------------------------------------------------

module procedure colconcat_4a
#include "./ColConcat_4.inc"
end procedure colconcat_4a

module procedure colconcat_4b
#include "./ColConcat_4.inc"
end procedure colconcat_4b

module procedure colconcat_4c
#include "./ColConcat_4.inc"
end procedure colconcat_4c

module procedure colconcat_4d
#include "./ColConcat_4.inc"
end procedure colconcat_4d

module procedure colconcat_4e
#include "./ColConcat_4.inc"
end procedure colconcat_4e

module procedure colconcat_4f
#include "./ColConcat_4.inc"
end procedure colconcat_4f

!----------------------------------------------------------------------------
!                                                                 colConcat
!----------------------------------------------------------------------------

MODULE PROCEDURE Rowconcat_1a
#include "./RowConcat_1.inc"
END PROCEDURE Rowconcat_1a

MODULE PROCEDURE Rowconcat_1b
#include "./RowConcat_1.inc"
END PROCEDURE Rowconcat_1b

MODULE PROCEDURE Rowconcat_1c
#include "./RowConcat_1.inc"
END PROCEDURE Rowconcat_1c

MODULE PROCEDURE Rowconcat_1d
#include "./RowConcat_1.inc"
END PROCEDURE Rowconcat_1d

MODULE PROCEDURE Rowconcat_1e
#include "./RowConcat_1.inc"
END PROCEDURE Rowconcat_1e

MODULE PROCEDURE Rowconcat_1f
#include "./RowConcat_1.inc"
END PROCEDURE Rowconcat_1f

!----------------------------------------------------------------------------
!                                                                 RowConcat
!----------------------------------------------------------------------------

MODULE PROCEDURE Rowconcat_2a
#include "./RowConcat_2.inc"
END PROCEDURE Rowconcat_2a

MODULE PROCEDURE Rowconcat_2b
#include "./RowConcat_2.inc"
END PROCEDURE Rowconcat_2b

MODULE PROCEDURE Rowconcat_2c
#include "./RowConcat_2.inc"
END PROCEDURE Rowconcat_2c

MODULE PROCEDURE Rowconcat_2d
#include "./RowConcat_2.inc"
END PROCEDURE Rowconcat_2d

MODULE PROCEDURE Rowconcat_2e
#include "./RowConcat_2.inc"
END PROCEDURE Rowconcat_2e

MODULE PROCEDURE Rowconcat_2f
#include "./RowConcat_2.inc"
END PROCEDURE Rowconcat_2f

!----------------------------------------------------------------------------
!                                                                 RowConcat
!----------------------------------------------------------------------------

MODULE PROCEDURE Rowconcat_3a
#include "./RowConcat_3.inc"
END PROCEDURE Rowconcat_3a

MODULE PROCEDURE Rowconcat_3b
#include "./RowConcat_3.inc"
END PROCEDURE Rowconcat_3b

MODULE PROCEDURE Rowconcat_3c
#include "./RowConcat_3.inc"
END PROCEDURE Rowconcat_3c

MODULE PROCEDURE Rowconcat_3d
#include "./RowConcat_3.inc"
END PROCEDURE Rowconcat_3d

MODULE PROCEDURE Rowconcat_3e
#include "./RowConcat_3.inc"
END PROCEDURE Rowconcat_3e

MODULE PROCEDURE Rowconcat_3f
#include "./RowConcat_3.inc"
END PROCEDURE Rowconcat_3f

!----------------------------------------------------------------------------
!                                                                 RowConcat
!----------------------------------------------------------------------------

MODULE PROCEDURE Rowconcat_4a
#include "./RowConcat_4.inc"
END PROCEDURE Rowconcat_4a

MODULE PROCEDURE Rowconcat_4b
#include "./RowConcat_4.inc"
END PROCEDURE Rowconcat_4b

MODULE PROCEDURE Rowconcat_4c
#include "./RowConcat_4.inc"
END PROCEDURE Rowconcat_4c

MODULE PROCEDURE Rowconcat_4d
#include "./RowConcat_4.inc"
END PROCEDURE Rowconcat_4d

MODULE PROCEDURE Rowconcat_4e
#include "./RowConcat_4.inc"
END PROCEDURE Rowconcat_4e

MODULE PROCEDURE Rowconcat_4f
#include "./RowConcat_4.inc"
END PROCEDURE Rowconcat_4f
END SUBMODULE AppendMethods
