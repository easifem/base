! Make sure _KIND_ is defined
INTEGER(I4B) :: ii, tsize, jj

tsize = 0
DO ii = 1, SIZE(obj)
  tsize = tsize + obj(ii)%tsize
END DO

CALL Reallocate(val, tsize)

tsize = 0
DO ii = 1, SIZE(obj)
  DO jj = 1, obj(ii)%tsize
    tsize = tsize + 1
    val(tsize) = INT(obj(ii)%val(jj), _KIND_)
  END DO
END DO