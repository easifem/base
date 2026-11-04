! make sure _KIND_ is defined
INTEGER(I4B) :: ii, tsize
tsize = obj%tsize
CALL Reallocate(val, tsize)
DO ii = 1, tsize
  val(ii) = INT(obj%val(ii), kind=_KIND_)
END DO
