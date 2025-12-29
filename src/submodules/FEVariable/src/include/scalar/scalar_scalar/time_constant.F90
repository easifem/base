PURE SUBROUTINE time_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj1%len
  ans%s(1) = obj1%s(1)
  ans%val(1:ans%len) = obj1%val(1:ans%len) _OP_ obj2%val(1)
END SUBROUTINE time_constant
