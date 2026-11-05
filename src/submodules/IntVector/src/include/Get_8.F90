! make sure _KIND_ is defined
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: ii
tsize = SIZE(indx)

! ans can hold old values, skipped entries must be zero as in Get8
ans(1:tsize) = 0

loop1: DO ii = 1, tsize
#ifdef DEBUG_VER
  isok = indx(ii) .LE. obj%tsize
  IF (.NOT. isok) CYCLE loop1
#endif
  ans(ii) = INT(obj%val(indx(ii)), _KIND_)
END DO loop1
