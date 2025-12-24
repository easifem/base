LOGICAL :: isalloc, abool(3), ex, acase
INTEGER(I4B) :: s(3), ii, jj, kk, fac

isalloc = ALLOCATED(mat)

! If not allocated, then allocate and return
IF (.NOT. isalloc) THEN
  ALLOCATE (mat(i1, i2, i3))
  DO CONCURRENT(ii=1:i1, jj=1:i2, kk=1:i3)
    mat(ii, jj, kk) = ZEROVALUE
  END DO
  RETURN
END IF

ex = .FALSE.
IF (PRESENT(isExpand)) ex = isExpand

! If allocated and isExpand is false, the deallocat and allocate
acase = .NOT. ex
IF (acase) THEN
  s = SHAPE(mat)

  abool(1) = s(1) .NE. i1 .OR. s(2) .NE. i2 .OR. s(3) .NE. i3

  IF (abool(1)) THEN
    DEALLOCATE (mat)
    ALLOCATE (mat(i1, i2, i3))
  END IF

  DO CONCURRENT(ii=1:i1, jj=1:i2, kk=1:i3)
    mat(ii, jj, kk) = ZEROVALUE
  END DO

  RETURN
END IF

! If allocated and isExpand is true
fac = 1
IF (PRESENT(expandFactor)) fac = expandFactor

s = SHAPE(mat)

! abool = (s(1) .LT. i1) .OR. s(2) .NE. i2 .OR. s(3) .NE. i3
abool(1) = s(1) .LT. i1
abool(2) = s(2) .LT. i2
abool(3) = s(3) .LT. i3

IF (abool(1)) s(1) = i1 * fac
IF (abool(2)) s(2) = i2 * fac
IF (abool(3)) s(3) = i3 * fac

IF (ANY(abool)) THEN
  DEALLOCATE (mat)
  ALLOCATE (mat(s(1), s(2), s(3)))
END IF

DO CONCURRENT(ii=1:i1, jj=1:i2, kk=1:i3)
  mat(ii, jj, kk) = ZEROVALUE
END DO

