LOGICAL(LGT) :: isok, abool
INTEGER(I4B) :: ii

isok = ALLOCATED(A)

IF (isok) THEN

  abool = SIZE(A) .NE. nA

  IF (abool) THEN
    DEALLOCATE (A)
    ALLOCATE (A(nA))
  END IF

ELSE

  ALLOCATE (A(nA))

END IF

DO CONCURRENT(ii=1:nA)
  A(ii) = 0.0
END DO

isok = ALLOCATED(IA)

IF (isok) THEN

  abool = SIZE(IA) .NE. nIA

  IF (abool) THEN
    DEALLOCATE (IA)
    ALLOCATE (IA(nIA))
  END IF

ELSE
  ALLOCATE (IA(nIA))
END IF

DO CONCURRENT(ii=1:nIA)
  IA(ii) = 0
END DO
