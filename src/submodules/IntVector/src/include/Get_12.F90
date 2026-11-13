! make sure _KIND_ is defined
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: ii, jj, kk
tsize = MAX((iend - istart + stride) / stride, 0) * SIZE(obj)

! ans can hold old values, skipped entries must be zero as in Get12
ans(1:tsize) = 0

kk = 0
loop1: DO ii = 1, SIZE(obj)
  loop2: DO jj = istart, iend, stride
    kk = kk + 1
#ifdef DEBUG_VER
    isok = jj .LE. obj(ii)%tsize
    IF (.NOT. isok) CYCLE loop2
#endif
    ans(kk) = INT(obj(ii)%val(jj), _KIND_)
  END DO loop2
END DO loop1
