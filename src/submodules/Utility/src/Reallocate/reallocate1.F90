LOGICAL :: isok, abool, ex, acase
INTEGER(I4B) :: ii, fac

ex = .FALSE.
IF (PRESENT(isExpand)) ex = isExpand

fac = 1
IF (PRESENT(expandFactor)) fac = expandFactor

isok = ALLOCATED(mat)

acase = isok .AND. (.NOT. ex)
IF (acase) THEN
  abool = SIZE(mat) .NE. row

  IF (abool) THEN
    DEALLOCATE (mat)
    ALLOCATE (mat(row))
  END IF

  ! CALL setzeros
  DO CONCURRENT(ii=1:row)
    mat(ii) = ZEROVALUE
  END DO
  RETURN
END IF

acase = isok .AND. ex
IF (acase) THEN

  abool = SIZE(mat) .LT. row
  IF (abool) THEN
    DEALLOCATE (mat)
    ALLOCATE (mat(row * fac))
    ! a new allocation can be larger than the request: zero all of it
    mat = ZEROVALUE
    RETURN
  END IF

  DO CONCURRENT(ii=1:row)
    mat(ii) = ZEROVALUE
  END DO
  RETURN
END IF

! not allocated: allocate exactly row entries, as for rank 2 and 3
ALLOCATE (mat(row))
DO CONCURRENT(ii=1:row)
  mat(ii) = ZEROVALUE
END DO
! CALL setzeros
