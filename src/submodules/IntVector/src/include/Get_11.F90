! make sure _KIND_ is defined
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: ii, jj, kk
tsize = SIZE(obj) * SIZE(indx)

! ans can hold old values, skipped entries must be zero as in Get11
ans(1:tsize) = 0

kk = 0
loop1: DO ii = 1, SIZE(obj)
  loop2: DO jj = 1, SIZE(indx)
    kk = kk + 1
#ifdef DEBUG_VER
    isok = indx(jj) .LE. obj(ii)%tsize
    IF (.NOT. isok) CYCLE loop2
#endif
    ans(kk) = INT(obj(ii)%val(indx(jj)), _KIND_)
  END DO loop2
END DO loop1
