! make sure _KIND_ is defined
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: ii, jj
tsize = MAX((iend - istart + stride) / stride, 0)

! ans can hold old values, skipped entries must be zero as in Get9
ans(1:tsize) = 0

jj = 0
loop1: DO ii = istart, iend, stride
  jj = jj + 1
#ifdef DEBUG_VER
  isok = ii .LE. obj%tsize
  IF (.NOT. isok) CYCLE loop1
#endif
  ans(jj) = INT(obj%val(ii), _KIND_)
END DO loop1
