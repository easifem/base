LOGICAL :: isalloc, abool(3), ex, acase
INTEGER(I4B) :: s(2), ii, jj, fac

isalloc = ALLOCATED(mat)

! If not allocated, then allocate and return
IF (.NOT. isalloc) THEN
  ALLOCATE (mat(row, col))
  DO CONCURRENT(ii=1:row, jj=1:col)
    mat(ii, jj) = ZEROVALUE
  END DO
  RETURN
END IF

ex = .FALSE.
IF (PRESENT(isExpand)) ex = isExpand

! If allocated and isExpand is false, the deallocat and allocate
acase = .NOT. ex
IF (acase) THEN
  s = SHAPE(mat)

  abool(1) = s(1) .NE. row .OR. s(2) .NE. col

  IF (abool(1)) THEN
    DEALLOCATE (mat)
    ALLOCATE (mat(row, col))
  END IF

  DO CONCURRENT(ii=1:row, jj=1:col)
    mat(ii, jj) = ZEROVALUE
  END DO

  RETURN
END IF

! If allocated and isExpand is true
fac = 1
IF (PRESENT(expandFactor)) fac = expandFactor

s = SHAPE(mat)

abool(1) = s(1) .LT. row
abool(2) = s(2) .LT. col

IF (abool(1)) s(1) = row * fac
IF (abool(2)) s(2) = col * fac

IF (ANY(abool)) THEN
  DEALLOCATE (mat)
  ALLOCATE (mat(s(1), s(2)))
END IF

DO CONCURRENT(ii=1:row, jj=1:col)
  mat(ii, jj) = ZEROVALUE
END DO

