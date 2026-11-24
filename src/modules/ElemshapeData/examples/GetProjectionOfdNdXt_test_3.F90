!> author: OpenAI
! date: 2026-11-21
! summary: Debug rejection of time-dependent spatial projection velocities

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetProjectionOfdNdXt_
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(ElemShapeData_) :: obj
TYPE(FEVariable_) :: c
REAL(DFP) :: ans(1, 1)
INTEGER(I4B) :: nrow, ncol
INTEGER :: mode, status, commandStatus, unit, ios, tick
CHARACTER(1024) :: executable, command, line, message
CHARACTER(160) :: logfile
CHARACTER(16) :: argument, stamp
LOGICAL :: found

IF (COMMAND_ARGUMENT_COUNT() .EQ. 0) THEN
  CALL GET_COMMAND_ARGUMENT(0, executable)
  CALL SYSTEM_CLOCK(tick)
  WRITE (stamp, '(I0)') tick
  ! Test 1: reject both time-only and space-time velocities
  DO mode = 1, 2
    WRITE (argument, '(I0)') mode
    logfile = '/tmp/easifem-529-N-'//TRIM(stamp)//'-'//TRIM(argument)
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
        IF (INDEX(line, 'NdXt projection: time-dependent c unsupported') &
            .GT. 0) found = .TRUE.
      END DO scanLog
      CLOSE (unit)
    END IF
    CALL OK(commandStatus .EQ. 0 .AND. status .NE. 0 .AND. found, &
            'NdXt case '//TRIM(argument)//': intended debug rejection')
  END DO
ELSE
  CALL GET_COMMAND_ARGUMENT(1, argument)
  READ (argument, *) mode
  obj%nns = 1
  obj%nips = 1
  obj%nsd = 1
  obj%N = RESHAPE([1.0_DFP], [1, 1])
  obj%dNdXt = RESHAPE([1.0_DFP], [1, 1, 1])
  IF (mode .EQ. 1) THEN
    c = NodalVariable([2.0_DFP, 2.0_DFP], Vector, Time, [1, 2])
  ELSE
    c = NodalVariable([2.0_DFP, 2.0_DFP], Vector, SpaceTime, [1, 1, 2])
  END IF
  CALL GetProjectionOfdNdXt_(obj=obj, c=c, crank=Vector, ans=ans, &
                             nrow=nrow, ncol=ncol)
END IF
END PROGRAM main
