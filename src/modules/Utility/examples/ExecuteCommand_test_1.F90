!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ExecuteCommand runs a shell command (macOS and Linux)

PROGRAM main
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: ExecuteCommand
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: exitStat

! a command that succeeds
CALL ExecuteCommand("true", "")
CALL OK(.TRUE., "ExecuteCommand('true') returns")

! a command that writes to the terminal
CALL ExecuteCommand("echo ExecuteCommand works", "")
CALL OK(.TRUE., "ExecuteCommand('echo ...') returns")

! with exitStat, a failing command returns its exit status; without
! exitStat, a failing command stops the program
CALL ExecuteCommand("true", "", exitStat)
CALL OK(exitStat .EQ. 0_I4B, "exit status of true is 0")
CALL ExecuteCommand("false", "", exitStat)
CALL OK(exitStat .NE. 0_I4B, "exit status of false is not 0")

END PROGRAM main
