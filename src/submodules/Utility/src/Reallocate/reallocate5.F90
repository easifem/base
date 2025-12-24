LOGICAL :: isok, abool
INTEGER(I4B) :: s(5), ii, jj, kk, ll, mm

isok = ALLOCATED(mat)

IF (isok) THEN

  s = SHAPE(mat)

  abool = (s(1) .NE. i1) .OR. &
          (s(2) .NE. i2) .OR. &
          s(3) .NE. i3 .OR. &
          s(4) .NE. i4 .OR. &
          s(5) .NE. i5

  IF (abool) THEN
    DEALLOCATE (mat)
    ALLOCATE (mat(i1, i2, i3, i4, i5))
  END IF

ELSE

  ALLOCATE (mat(i1, i2, i3, i4, i5))

END IF

DO CONCURRENT(ii=1:i1, jj=1:i2, kk=1:i3, ll=1:i4, mm=1:i5)
  mat(ii, jj, kk, ll, mm) = ZEROVALUE
END DO
