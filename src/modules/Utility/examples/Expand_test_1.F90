PROGRAM main
USE AppendUtility, ONLY: Expand
USE Display_Method, ONLY: Display
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: aintvec(:)
INTEGER(I4B) :: n, val

! First CALL
! We set n = 0, as aintvec is not allocated.
val = 1
n = 0
CALL Expand(vec=aintvec, n=n, chunk_size=10_I4B, val=val, finished=.FALSE.)
CALL OK(aintvec(1) .EQ. val, "Expanding unallocated vec")
CALL OK(n .EQ. 1, "n for unallocated vec")
CALL OK(SIZE(aintvec) .EQ. 10, "size of unallocated vec")

val = 2
CALL Expand(vec=aintvec, n=n, chunk_size=10_I4B, val=val, finished=.FALSE.)
CALL OK(ALL(aintvec(1:2) .EQ. [1, 2]), "Expanding allocated vec")
CALL OK(n .EQ. 2, "n for allocated vec")
CALL OK(SIZE(aintvec) .EQ. 10_I4B, "size of allocated vec")

val = 3
CALL Expand(vec=aintvec, n=n, chunk_size=10_I4B, val=val, finished=.TRUE.)
CALL OK(ALL(aintvec(1:3) .EQ. [1, 2, 3]), "Expanding allocated vec")
CALL OK(n .EQ. 3, "n for allocated vec")
CALL OK(SIZE(aintvec) .EQ. 3, "size of allocated vec")
END PROGRAM main
