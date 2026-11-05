! make sure _KIND_ is defined
INTEGER(I4B) :: ii
tsize = obj%tsize
DO ii = 1, tsize
  ans(ii) = INT(obj%val(ii), kind=_KIND_)
END DO
