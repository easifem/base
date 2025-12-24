INTEGER(I4B) :: tsize, ii

tsize = SIZE(val)
CALL Util_Reallocate(obj%val, tsize)
DO ii = 1, tsize
  obj%val(ii) = INT(val(ii), kind=I4B)
END DO
CALL SetTotalDimension(obj, 1_I4B)
