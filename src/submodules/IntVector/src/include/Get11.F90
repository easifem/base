! make sure _KIND_ is defined
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: ii, jj, tsize

CALL Reallocate(val, SIZE(obj) * SIZE(indx))

tsize = 0
loop1: DO ii = 1, SIZE(obj)
  loop2: DO jj = 1, SIZE(indx)
    tsize = tsize + 1
#ifdef DEBUG_VER
    isok = indx(jj) .LE. obj(ii)%tsize
    IF (.NOT. isok) CYCLE loop2
#endif
    val(tsize) = INT(obj(ii)%val(indx(jj)), _KIND_)
  END DO loop2
END DO loop1