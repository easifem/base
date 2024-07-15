LOGICAL :: isok, abool, ex, acase
INTEGER(I4B) :: s(2), ii, jj, fac

ex = .FALSE.
IF (PRESENT(isExpand)) ex = isExpand

fac = 1
IF (PRESENT(expandFactor)) fac = expandFactor

isok = ALLOCATED(mat)

acase = isok .AND. (.NOT. ex)

IF (acase) THEN

  s = SHAPE(mat)

  abool = s(1) .NE. row .OR. s(2) .NE. col

  IF (abool) THEN
    DEALLOCATE (mat)
    ALLOCATE (mat(row, col))
  END IF

  DO CONCURRENT(ii=1:row, jj=1:col)
    mat(ii, jj) = ZEROVALUE
  END DO
  RETURN

END IF

acase = isok .AND. ex

IF (acase) THEN

  s = SHAPE(mat)

  abool = (s(1) .LT. row) .OR. &
          (s(2) .LT. col)

  IF (abool) THEN
    DEALLOCATE (mat)
    ALLOCATE (mat(row * fac, col * fac))
  END IF

  DO CONCURRENT(ii=1:row, jj=1:col)
    mat(ii, jj) = ZEROVALUE
  END DO
  RETURN

END IF

ALLOCATE (mat(row * fac, col * fac))

DO CONCURRENT(ii=1:row, jj=1:col)
  mat(ii, jj) = ZEROVALUE
END DO

! IF (ALLOCATED(mat)) THEN
!   IF ((SIZE(mat, 1) .NE. row) .OR. (SIZE(Mat, 2) .NE. col)) THEN
!     DEALLOCATE (mat)
!     ALLOCATE (mat(row, col))
!   END IF
! ELSE
!   ALLOCATE (mat(row, col))
! END IF
