PROGRAM main
USE GlobalData, ONLY: DFP
USE BinomUtility, ONLY: Binom
USE Display_Method, ONLY: Display
IMPLICIT NONE

! Here we are calculating ${}^{4}C_{m}$, for $m=0,1,2,3,4$
CALL Display(Binom(4, 0, kind=1.0_DFP), "(4, 0) = ")
CALL Display(Binom(4, 1, kind=1.0_DFP), "(4, 1) = ")
CALL Display(Binom(4, 2, kind=1.0_DFP), "(4, 2) = ")
CALL Display(Binom(4, 3, kind=1.0_DFP), "(4, 3) = ")
CALL Display(Binom(4, 4, kind=1.0_DFP), "(4, 4) = ")

! ```txt title="Results"
! (4, 0) = 1.00000
! (4, 1) = 4.00000
! (4, 2) = 6.00000
! (4, 3) = 4.00000
! (4, 4) = 1.00000
! ```
END PROGRAM main
