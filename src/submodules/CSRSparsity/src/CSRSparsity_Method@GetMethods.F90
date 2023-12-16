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

!> author: Vikas Sharma, Ph. D.
! date: 13 Jul 2021
! summary: Input output related methods

SUBMODULE(CSRSparsity_Method) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_shape
Ans = [obj%nrow, obj%ncol]
END PROCEDURE obj_shape

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_size
IF (PRESENT(Dims)) THEN
  IF (Dims .EQ. 1) THEN
    Ans = obj%nrow
  ELSE
    Ans = obj%ncol
  END IF
ELSE
  Ans = obj%nrow * obj%ncol
END IF
END PROCEDURE obj_size

!----------------------------------------------------------------------------
!                                                                     GetNNZ
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNZ
Ans = obj%nnz
END PROCEDURE obj_GetNNZ

!----------------------------------------------------------------------------
!                                                                    GetNNZ
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNZ_from_operation
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isSorted0

isSorted0 = Input(default=.FALSE., option=isSorted)

SELECT CASE (op)
CASE ("+", "-")
  nrow = SIZE(obj1, 1)
  ncol = SIZE(obj1, 2)
  IF (isSorted0) THEN
    ans = GetNNZ_Add_Subtract_sorted(nrow=nrow, ncol=ncol, ja=obj1%JA,  &
      & ia=obj1%IA, jb=obj2%JA, ib=obj2%IA)
  ELSE
    ans = GetNNZ_Add_Subtract(nrow=nrow, ncol=ncol, ja=obj1%JA,  &
      & ia=obj1%IA, jb=obj2%JA, ib=obj2%IA)
  END IF
END SELECT
END PROCEDURE obj_GetNNZ_from_operation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetNNZ_Add_Subtract
INTEGER(I4B) :: ii, jcol, kb, jpos, ka
LOGICAL(LGT) :: iw(ncol)
ans = 0

DO ii = 1, nrow
  iw = .FALSE.
  DO ka = ia(ii), ia(ii + 1) - 1
    ans = ans + 1
    jcol = ja(ka)
    iw(jcol) = .TRUE.
  END DO

  DO kb = ib(ii), ib(ii + 1) - 1
    jcol = jb(kb)
    IF (.NOT. iw(jcol)) THEN
      ans = ans + 1
      iw(jcol) = .TRUE.
    END IF
  END DO
END DO
END PROCEDURE GetNNZ_Add_Subtract

!----------------------------------------------------------------------------
!                                                                   GetNNZ
!----------------------------------------------------------------------------

MODULE PROCEDURE GetNNZ_Add_Subtract_sorted
! internal variables
INTEGER(I4B) :: len, i, ka, kb, kc, &
  & kamax, kbmax, j1, jj2
LOGICAL(LGT) :: isok

kc = 1
DO i = 1, nrow
  ka = ia(i)
  kb = ib(i)
  kamax = ia(i + 1) - 1
  kbmax = ib(i + 1) - 1

  DO
    isok = ka .LE. kamax .OR. kb .LE. kbmax
    IF (.NOT. isok) EXIT

    IF (ka .LE. kamax) THEN
      j1 = ja(ka)
    ELSE
      ! take j1 large enough  that always jj2 .lt. j1
      j1 = ncol + 1
    END IF

    IF (kb .LE. kbmax) THEN
      jj2 = jb(kb)
    ELSE
      ! similarly take jj2 large enough  that always j1 .lt. jj2
      jj2 = ncol + 1
    END IF

    IF (j1 .EQ. jj2) THEN
      ka = ka + 1
      kb = kb + 1
      kc = kc + 1
    ELSE IF (j1 .LT. jj2) THEN
      ka = ka + 1
      kc = kc + 1
    ELSE IF (j1 .GT. jj2) THEN
      kb = kb + 1
      kc = kc + 1
    END IF
  END DO
END DO

ans = kc - 1
END PROCEDURE GetNNZ_Add_Subtract_sorted

!----------------------------------------------------------------------------
!                                                                 GetNNZ
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNZ1
INTEGER(I4B) :: ii, rindx
IF (obj%isInitiated) THEN
  ans = 0
  SELECT CASE (from)
  CASE ("L", "l")
    DO ii = 1, obj%nrow
      DO rindx = obj%IA(ii), obj%IA(ii + 1) - 1
        IF (ii .GT. obj%JA(rindx)) ans = ans + 1
      END DO
    END DO

  CASE ("U", "u")
    DO ii = 1, obj%nrow
      DO rindx = obj%IA(ii), obj%IA(ii + 1) - 1
        IF (ii .LT. obj%JA(rindx)) ans = ans + 1
      END DO
    END DO

  CASE ("D", "d")
    DO ii = 1, obj%nrow
      DO rindx = obj%IA(ii), obj%IA(ii + 1) - 1
        IF (ii .EQ. obj%JA(rindx)) ans = ans + 1
      END DO
    END DO
  CASE default
    ans = obj%nnz
  END SELECT
ELSE
  ans = 0
END IF
END PROCEDURE obj_GetNNZ1

!----------------------------------------------------------------------------
!                                                                 GetNNZ
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNZ2
INTEGER(I4B) :: ii, rindx
IF (obj%isInitiated) THEN
  ans = 0

  DO ii = 1, obj%nrow
    DO rindx = obj%IA(ii), obj%IA(ii + 1) - 1
      IF (ii .LT. obj%JA(rindx)) THEN
        ! U
        ans(1) = ans(1) + 1
      ELSEIF (ii .GT. obj%JA(rindx)) THEN
        ! L
        ans(2) = ans(2) + 1
      ELSEIF (ii .EQ. obj%JA(rindx)) THEN
        ! D
        ans(3) = ans(3) + 1
      END IF
    END DO
  END DO
ELSE
  ans = 0
END IF
END PROCEDURE obj_GetNNZ2

!----------------------------------------------------------------------------
!                                                                 GetDiagonal
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDiagonal1
INTEGER(I4B) :: len0
CALL Reallocate(diag, obj%nrow, idiag, obj%nrow)
CALL GetDIA( &
  & obj%nrow,&
  & obj%ncol,&
  & 0,&
  & A,&
  & obj%JA,&
  & obj%IA,&
  & len0,&
  & diag,&
  & idiag,&
  & INPUT(option=offSet, default=0))
END PROCEDURE obj_GetDiagonal1

!----------------------------------------------------------------------------
!                                                               GetDiagonal
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDiagonal2
INTEGER(I4B) :: ii

IF (obj%isDiagStored) THEN

  CALL Reallocate(diag, obj%nrow)
  DO ii = 1, SIZE(diag)
    diag(ii) = A(obj%idiag(ii))
  END DO

ELSE
  CALL Reallocate(diag, obj%nrow)
  CALL GetDIA( &
    & obj%nrow,&
    & obj%ncol,&
    & 0,&
    & A,&
    & obj%JA,&
    & obj%IA,&
    & ii,&
    & diag,&
    & obj%idiag,&
    & INPUT(option=offSet, default=0))
  obj%isDiagStored = .TRUE.
END IF

END PROCEDURE obj_GetDiagonal2

!----------------------------------------------------------------------------
!                                                               GetColNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColNumber1
ans = obj%JA(indx)
END PROCEDURE obj_GetColNumber1

!----------------------------------------------------------------------------
!                                                               GetColIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColIndex1
ans(1) = obj%IA(irow)
ans(2) = obj%IA(irow + 1) - 1
END PROCEDURE obj_GetColIndex1

!----------------------------------------------------------------------------
!                                                              startColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_startColumn1
ans = obj%IA(irow)
END PROCEDURE obj_startColumn1

!----------------------------------------------------------------------------
!                                                              endColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_endColumn1
ans = obj%IA(irow + 1) - 1
END PROCEDURE obj_endColumn1

END SUBMODULE GetMethods
