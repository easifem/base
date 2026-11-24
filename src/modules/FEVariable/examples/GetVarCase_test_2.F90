!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Variable case of all pairs of variable types
!
! GetVarCase(vartype1, vartype2) = 10 * a + b, where a and b are
! 0 (constant), 1 (space), 2 (time) or 3 (spaceTime).

PROGRAM main
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE FEVariable_Method, ONLY: GetVarCase
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: all 16 pairs
BLOCK
  INTEGER(I4B) :: vt(4), ii, jj
  LOGICAL :: isok

  vt = [fevaropt%constant, fevaropt%space, fevaropt%time, &
        fevaropt%spacetime]

  isok = .TRUE.
  DO ii = 1, 4
    DO jj = 1, 4
      isok = isok .AND. &
             (GetVarCase(vt(ii), vt(jj)) .EQ. 10 * (ii - 1) + (jj - 1))
    END DO
  END DO
  CALL OK(isok, "all pairs: 10 * a + b")
END BLOCK

! Test 2: the order of the arguments matters
BLOCK
  CALL OK(GetVarCase(fevaropt%space, fevaropt%spacetime) .EQ. 13, "13")
  CALL OK(GetVarCase(fevaropt%spacetime, fevaropt%space) .EQ. 31, "31")
END BLOCK

END PROGRAM main
