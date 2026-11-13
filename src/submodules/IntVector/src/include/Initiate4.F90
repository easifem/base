INTEGER(I4B) :: ii

obj%tsize = SIZE(val)
obj%capacity = obj%tsize
CALL Util_Reallocate(obj%val, obj%tsize)
DO ii = 1, obj%tsize
  obj%val(ii) = INT(val(ii), kind=I4B)
END DO
CALL SetTotalDimension(obj, 1_I4B)
