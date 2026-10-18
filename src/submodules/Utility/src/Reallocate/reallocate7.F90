

! Rule (as for rank 2 and 3):
! - not allocated: allocate exactly the requested shape
! - isExpand false or absent: reallocate if the shape differs
! - isExpand true: keep each extent that is large enough, an extent
!   that is too small becomes request * expandFactor
! Only the requested leading block is set to zero.

LOGICAL(LGT) :: abool(7), ex
INTEGER(I4B) :: s(7), req(7), ii, jj, kk, ll, mm, nn, oo, fac

IF (.NOT. ALLOCATED(mat)) THEN
  ALLOCATE (mat(i1, i2, i3, i4, i5, i6, i7))
  DO CONCURRENT(ii=1:i1, jj=1:i2, kk=1:i3, ll=1:i4, mm=1:i5, nn=1:i6, oo=1:i7)
    mat(ii, jj, kk, ll, mm, nn, oo) = ZEROVALUE
  END DO
  RETURN
END IF

ex = .FALSE.
IF (PRESENT(isExpand)) ex = isExpand

fac = 1
IF (PRESENT(expandFactor)) fac = expandFactor

req = [i1, i2, i3, i4, i5, i6, i7]
s = SHAPE(mat)

IF (ex) THEN
  abool = s .LT. req
  WHERE (abool) s = req * fac
ELSE
  abool = s .NE. req
  s = req
END IF

IF (ANY(abool)) THEN
  DEALLOCATE (mat)
  ALLOCATE (mat(s(1), s(2), s(3), s(4), s(5), s(6), s(7)))
END IF

DO CONCURRENT(ii=1:i1, jj=1:i2, kk=1:i3, ll=1:i4, mm=1:i5, nn=1:i6, oo=1:i7)
  mat(ii, jj, kk, ll, mm, nn, oo) = ZEROVALUE
END DO
