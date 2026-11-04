! make sure _KIND_ is defined
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: ii, jj, tsize

tsize = MAX((iend - istart + stride) / stride, 0)
CALL Reallocate(val, tsize * SIZE(obj))

tsize = 0
loop1: DO ii = 1, SIZE(obj)
  loop2: DO jj = istart, iend, stride
    tsize = tsize + 1
#ifdef DEBUG_VER
    isok = jj .LE. obj(ii)%tsize
    IF (.NOT. isok) CYCLE loop2
#endif
    val(tsize) = INT(obj(ii)%val(jj), _KIND_)
  END DO loop2
END DO loop1