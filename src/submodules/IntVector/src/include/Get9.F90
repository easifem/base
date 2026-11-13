! make sure _KIND_ is defined
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: ii, tsize
tsize = MAX((iend - istart + stride) / stride, 0)
CALL Reallocate(val, tsize)
tsize = 0
loop1: DO ii = istart, iend, stride
  tsize = tsize + 1
#ifdef DEBUG_VER
  isok = ii .LE. obj%tsize
  IF (.NOT. isok) CYCLE loop1
#endif
  val(tsize) = INT(obj%val(ii), _KIND_)
END DO loop1
