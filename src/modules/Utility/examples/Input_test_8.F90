!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Input for logical scalars

PROGRAM main
USE GlobalData, ONLY: LGT
USE InputUtility, ONLY: Input
USE Test_Method, ONLY: OK
IMPLICIT NONE

LOGICAL(LGT) :: t, f

t = .TRUE.
f = .FALSE.

! option is present
CALL OK(.NOT. Input(t, f), "option is present (false)")
CALL OK(Input(f, t), "option is present (true)")
CALL OK(Input(default=f, option=t), "keyword arguments")

! option is absent
CALL OK(Input(t), "option is absent (true)")
CALL OK(.NOT. Input(f), "option is absent (false)")
CALL OK(Input(default=t), "keyword default")

END PROGRAM main
