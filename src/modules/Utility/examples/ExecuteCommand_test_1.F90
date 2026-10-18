!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ExecuteCommand runs a shell command (macOS and Linux)

PROGRAM main
USE MiscUtility, ONLY: ExecuteCommand
USE Test_Method, ONLY: OK
IMPLICIT NONE

! a command that succeeds
CALL ExecuteCommand("true", "")
CALL OK(.TRUE., "ExecuteCommand('true') returns")

! a command that writes to the terminal
CALL ExecuteCommand("echo ExecuteCommand works", "")
CALL OK(.TRUE., "ExecuteCommand('echo ...') returns")

! the exit status of the command is not checked
CALL ExecuteCommand("false", "")
CALL OK(.TRUE., "ExecuteCommand('false') also returns")

END PROGRAM main
