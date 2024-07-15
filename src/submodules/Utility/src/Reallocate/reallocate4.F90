LOGICAL :: isok, abool
INTEGER(I4B) :: s(4), ii, jj, kk, ll

isok = ALLOCATED(mat)

IF (isok) THEN

  s = SHAPE(mat)

  abool = s(1) .NE. i1 .OR. s(2) .NE. i2 .OR. s(3) .NE. i3 .OR. s(4) .NE. i4

  IF (abool) THEN
    DEALLOCATE (mat)
    ALLOCATE (mat(i1, i2, i3, i4))
  END IF

ELSE

  ALLOCATE (mat(i1, i2, i3, i4))

END IF

DO CONCURRENT(ii=1:i1, jj=1:i2, kk=1:i3, ll=1:i4)
  mat(ii, jj, kk, ll) = ZEROVALUE
END DO
