

! Each vector follows the rule of the rank 2 and rank 3 overloads:
! - not allocated: allocate exactly n entries
! - isExpand false or absent: reallocate if the size is not n
! - isExpand true: keep the vector if it has at least n entries,
!   else allocate n * expandFactor entries
! Only the first n entries are set to zero.

LOGICAL(LGT) :: abool, ex
INTEGER(I4B) :: ii, tsize, fac

ex = .FALSE.
IF (PRESENT(isExpand)) ex = isExpand

fac = 1
IF (PRESENT(expandFactor)) fac = expandFactor

IF (ALLOCATED(A)) THEN
  IF (ex) THEN
    abool = SIZE(A) .LT. nA
    tsize = nA * fac
  ELSE
    abool = SIZE(A) .NE. nA
    tsize = nA
  END IF
  IF (abool) THEN
    DEALLOCATE (A)
    ALLOCATE (A(tsize))
  END IF
ELSE
  ALLOCATE (A(nA))
END IF

DO CONCURRENT(ii=1:nA)
  A(ii) = 0.0
END DO

IF (ALLOCATED(IA)) THEN
  IF (ex) THEN
    abool = SIZE(IA) .LT. nIA
    tsize = nIA * fac
  ELSE
    abool = SIZE(IA) .NE. nIA
    tsize = nIA
  END IF
  IF (abool) THEN
    DEALLOCATE (IA)
    ALLOCATE (IA(tsize))
  END IF
ELSE
  ALLOCATE (IA(nIA))
END IF

DO CONCURRENT(ii=1:nIA)
  IA(ii) = 0
END DO
