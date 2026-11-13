! make sure _KIND_ is defined
INTEGER(I4B) :: ii, jj

tsize = 0
DO ii = 1, SIZE(obj)
  DO jj = 1, obj(ii)%tsize
    tsize = tsize + 1
    ans(tsize) = INT(obj(ii)%val(jj), _KIND_)
  END DO
END DO
