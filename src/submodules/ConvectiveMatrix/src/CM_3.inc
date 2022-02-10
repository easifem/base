
!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE CM_3(ans, test, trial, term1, term2, c, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  !! test functions
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  !! trial functions
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x, del_y, del_z
  TYPE(FEVariable_), INTENT(IN) :: c
  !! A scalar finite element variable
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips
  REAL(DFP), ALLOCATABLE :: realval(:)
  !!
  !! main
  !!
  CALL Reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  !!
  CALL GetInterpolation(obj=trial, val=c, interpol=realval)
  !!
  realval = trial%js * trial%ws * trial%thickness * realval
  !!
  DO ips = 1, SIZE(realval)
    ans = ans + outerprod(a=test%N(:, ips), &
      & b=trial%dNdXt(:, term2, ips)) * realval(ips)
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !! cleanup
  DEALLOCATE (realval)
END SUBROUTINE CM_3
