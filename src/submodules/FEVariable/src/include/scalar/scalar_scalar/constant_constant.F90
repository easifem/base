PURE SUBROUTINE constant_constant(obj1, obj2, ans)
  TYPE(FEVariable_), INTENT(IN) :: obj1, obj2
  TYPE(FEVariable_), INTENT(INOUT) :: ans

  ans%len = 1
  ans%s(1) = 1
  ans%val(1) = obj1%val(1) _OP_ obj2%val(1)
END SUBROUTINE constant_constant

