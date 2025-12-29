PURE SUBROUTINE space_space(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = MIN(obj1%len, obj2%len)
  ans%s(1) = MIN(obj1%s(1), obj2%s(1))
  ans%val(1:ans%len) = obj1%val(1:ans%len) _OP_ obj2%val(1:ans%len)
END SUBROUTINE space_space
