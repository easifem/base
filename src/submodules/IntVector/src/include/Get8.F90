! make sure _KIND_ is defined
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: ii, tsize
tsize = SIZE(indx)
CALL Reallocate(val, tsize)
loop1: DO ii = 1, tsize
#ifdef DEBUG_VER
  isok = indx(ii) .LE. obj%tsize
  IF (.NOT. isok) CYCLE loop1
#endif
  val(ii) = INT(obj%val(indx(ii)), _KIND_)
END DO loop1
