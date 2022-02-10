
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE CM_2(ans, test, trial, c, term1, term2, opt)
  !! Intent of dummy variable
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  TYPE(FEVariable_), INTENT(IN) :: c
  !! Vector variable denoting the convective variable
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_x, del_y, del_z, del_x_all
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips
  REAL(DFP), ALLOCATABLE :: p(:, :), realVal(:)
  !!
  !! main
  !!
  CALL Reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  !!
  realval = trial%js * trial%ws * trial%thickness
  !!
  !! projection on test
  !!
  CALL GetProjectionOfdNdXt(obj=test, cdNdXt=p, val=c)
  !!
  DO ips = 1, SIZE(realval)
    ans = ans + outerprod(a=p(:, ips), &
      & b=trial%N(:, ips)) * realval(ips)
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !! cleanup
  DEALLOCATE (realval, p)
END SUBROUTINE CM_2
