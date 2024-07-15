LOGICAL :: isok, abool
INTEGER(I4B) :: s(3), ii, jj, kk

isok = ALLOCATED(mat)

IF (isok) THEN

  s = SHAPE(mat)

  abool = s(1) .NE. i1 .OR. s(2) .NE. i2 .OR. s(3) .NE. i3

  IF (abool) THEN
    DEALLOCATE (mat)
    ALLOCATE (mat(i1, i2, i3))
  END IF

ELSE

  ALLOCATE (mat(i1, i2, i3))

END IF

DO CONCURRENT(ii=1:i1, jj=1:i2, kk=1:i3)
  mat(ii, jj, kk) = ZEROVALUE
END DO
