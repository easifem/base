!----------------------------------------------------------------------------
!                                                           AssertError1
!----------------------------------------------------------------------------

SUBROUTINE AssertError1(a, myName, msg)
  LOGICAL, INTENT(IN) :: a
  CHARACTER(*), INTENT(IN) :: myName
  CHARACTER(*), INTENT(IN) :: msg

  IF (.NOT. a) THEN
    CALL Errormsg(msg=msg, file=__FILE__, routine=myName, &
                  line=__LINE__, unitno=stderr)
    RETURN
  END IF

END SUBROUTINE AssertError1

