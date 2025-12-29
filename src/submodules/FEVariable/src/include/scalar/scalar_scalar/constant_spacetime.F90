PURE SUBROUTINE constant_spacetime(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = obj2%len
  ans%s(1:2) = obj2%s(1:2)
  ans%val(1:ans%len) = obj1%val(1) _OP_ obj2%val(1:ans%len)
END SUBROUTINE constant_spacetime

