LOGICAL :: isok, abool
INTEGER(I4B) :: s(7), ii, jj, kk, ll, mm, nn, oo

isok = ALLOCATED(mat)

IF (isok) THEN

  s = SHAPE(mat)

  abool = (s(1) .NE. i1) .OR. &
          (s(2) .NE. i2) .OR. &
          (s(3) .NE. i3) .OR. &
          (s(4) .NE. i4) .OR. &
          (s(5) .NE. i5) .OR. &
          (s(6) .NE. i6) .OR. &
          (s(7) .NE. i7)

  IF (abool) THEN
    DEALLOCATE (mat)
    ALLOCATE (mat(i1, i2, i3, i4, i5, i6, i7))
  END IF

ELSE

  ALLOCATE (mat(i1, i2, i3, i4, i5, i6, i7))

END IF

DO CONCURRENT(ii=1:i1, jj=1:i2, kk=1:i3, ll=1:i4, mm=1:i5, nn=1:i6, oo=1:i7)
  mat(ii, jj, kk, ll, mm, nn, oo) = ZEROVALUE
END DO
