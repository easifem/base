!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: VoigtSize of an unknown Voigt form is 6

PROGRAM main
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: VoigtSize
IMPLICIT NONE

! Test 1: unknown forms fall back to the 3D size
BLOCK
  CALL OK(VoigtSize(99) .EQ. 6, "unknown 99: 6")
  CALL OK(VoigtSize(-1) .EQ. 6, "unknown -1: 6")
END BLOCK

END PROGRAM main
