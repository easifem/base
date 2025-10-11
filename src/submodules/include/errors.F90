!----------------------------------------------------------------------------
!                                                           AssertError1
!----------------------------------------------------------------------------

SUBROUTINE AssertError1(a, myName, modName, lineNo, msg)
  USE GlobalData, ONLY: I4B, stderr
  USE ErrorHandling, ONLY: Errormsg
  LOGICAL, INTENT(IN) :: a
  CHARACTER(*), INTENT(IN) :: myName, modName, msg
  INTEGER(I4B), INTENT(IN) :: lineNo

  IF (.NOT. a) THEN
    CALL Errormsg(msg=msg, file=modName, routine=myName, &
                  line=lineNo, unitno=stderr)
    STOP
  END IF

END SUBROUTINE AssertError1

