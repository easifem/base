!> author: OpenAI
! date: 2026-11-21
! summary: Debug rejection of unsupported space-time gradient object types

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: GetLocalGradient
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE, EXTENDS(STElemShapeData_) :: ExtendedST
END TYPE ExtendedST
CLASS(ElemShapeData_), ALLOCATABLE :: obj
REAL(DFP), ALLOCATABLE :: lg2(:, :), lg3(:, :, :), lg4(:, :, :, :)
REAL(DFP) :: v2(1, 1), v3(1, 1, 1), v4(1, 1, 1, 1)
INTEGER :: mode, status, commandStatus, unit, ios, tick
CHARACTER(1024) :: executable, command, line, message
CHARACTER(160) :: logfile
CHARACTER(16) :: argument, stamp
LOGICAL :: found

IF (COMMAND_ARGUMENT_COUNT() .EQ. 0) THEN
  CALL GET_COMMAND_ARGUMENT(0, executable)
  CALL SYSTEM_CLOCK(tick)
  WRITE (stamp, '(I0)') tick
  ! Test 1: scalar, vector, and matrix raw space-time inputs
  DO mode = 1, 6
    WRITE (argument, '(I0)') mode
    logfile = '/tmp/easifem-525-obj-'//TRIM(stamp)//'-'//TRIM(argument)
    command = '"'//TRIM(executable)//'" '//TRIM(argument)// &
              ' > "'//TRIM(logfile)//'" 2>&1'
    CALL EXECUTE_COMMAND_LINE(TRIM(command), exitstat=status, &
                              cmdstat=commandStatus, cmdmsg=message)
    found = .FALSE.
    OPEN (NEWUNIT=unit, file=TRIM(logfile), status='old', &
          action='read', iostat=ios, iomsg=message)
    IF (ios .EQ. 0) THEN
      scanLog: DO
        READ (unit, '(A)', iostat=ios, iomsg=message) line
        IF (ios .NE. 0) EXIT scanLog
        IF (INDEX(line, 'GetLocalGradient: space-time object required') &
            .GT. 0) found = .TRUE.
      END DO scanLog
      CLOSE (unit)
    END IF
    CALL OK(commandStatus .EQ. 0 .AND. status .NE. 0 .AND. found, &
            'object case '//TRIM(argument)//': intended debug rejection')
  END DO
ELSE
  CALL GET_COMMAND_ARGUMENT(1, argument)
  READ (argument, *) mode
  IF (mode .LE. 3) THEN
    ALLOCATE (ElemShapeData_ :: obj)
  ELSE
    ALLOCATE (ExtendedST :: obj)
  END IF
  v2 = 1.0_DFP
  v3 = 1.0_DFP
  v4 = 1.0_DFP
  SELECT CASE (MOD(mode - 1, 3))
  CASE (0)
    CALL GetLocalGradient(obj=obj, lg=lg2, val=v2)
  CASE (1)
    CALL GetLocalGradient(obj=obj, lg=lg3, val=v3)
  CASE (2)
    CALL GetLocalGradient(obj=obj, lg=lg4, val=v4)
  CASE DEFAULT
    ERROR STOP 'invalid test mode'
  END SELECT
END IF
END PROGRAM main
