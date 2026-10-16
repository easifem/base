PROGRAM main
USE GlobalData, ONLY: DFP, I4B, LGT
USE Display_Method, ONLY: Display
USE AppendUtility, ONLY: Expand
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: aintvec(:)
INTEGER(I4B) :: n, val

! First CALL
! We set n = 0, as aintvec is not allocated.

val = 1
n = 0
CALL Expand(vec=aintvec, n=n, chunk_size=10_I4B, val=val, finished=.FALSE.)

CALL Display(aintvec, "test1: aintvec = ", full=.TRUE.)
CALL Display(n, "n = ")
CALL Display(SIZE(aintvec), "test1: size of aintvec = ")

val = 2
CALL Expand(vec=aintvec, n=n, chunk_size=10_I4B, val=val, finished=.TRUE.)

CALL Display(aintvec, "test2: aintvec= ", full=.TRUE.)
CALL Display(n, "n = ")
CALL Display(SIZE(aintvec), "test2: size of aintvec = ")
END PROGRAM main
