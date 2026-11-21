!> author: OpenAI
! date: 2026-11-21
! summary: Debug rejection of unsupported local-gradient FEVariables

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetLocalGradient
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(ElemShapeData_) :: obj
TYPE(FEVariable_) :: val
REAL(DFP), ALLOCATABLE :: lg2(:, :), lg3(:, :, :), lg4(:, :, :, :)
INTEGER :: mode, status, commandStatus, unit, ios, tick
CHARACTER(1024) :: executable, command, line, message
CHARACTER(160) :: logfile
CHARACTER(16) :: argument, stamp
LOGICAL :: found

IF (COMMAND_ARGUMENT_COUNT() .EQ. 0) THEN
  CALL GET_COMMAND_ARGUMENT(0, executable)
  CALL SYSTEM_CLOCK(tick)
  WRITE (stamp, '(I0)') tick
  ! Test 1: time-only, incompatible space-time, and quadrature inputs
  DO mode = 1, 9
    WRITE (argument, '(I0)') mode
    logfile = '/tmp/easifem-525-val-'//TRIM(stamp)//'-'//TRIM(argument)
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
        IF (INDEX(line, 'GetLocalGradient: unsupported FEVariable') &
            .GT. 0) found = .TRUE.
      END DO scanLog
      CLOSE (unit)
    END IF
    CALL OK(commandStatus .EQ. 0 .AND. status .NE. 0 .AND. found, &
            'FEVariable case '//TRIM(argument)//': intended debug rejection')
  END DO
ELSE
  CALL GET_COMMAND_ARGUMENT(1, argument)
  READ (argument, *) mode
  obj%nns = 1
  obj%nips = 1
  obj%xidim = 1
  obj%dNdXi = RESHAPE([1.0_DFP], [1, 1, 1])
  SELECT CASE (mode)
  CASE (1)
    val = NodalVariable([1.0_DFP], Scalar, Time)
  CASE (2)
    val = NodalVariable([1.0_DFP], Vector, Time, [1, 1])
  CASE (3)
    val = NodalVariable([1.0_DFP], Matrix, Time, [1, 1, 1])
  CASE (4)
    val = NodalVariable([1.0_DFP], Scalar, SpaceTime, [1, 1])
  CASE (5)
    val = NodalVariable([1.0_DFP], Vector, SpaceTime, [1, 1, 1])
  CASE (6)
    val = NodalVariable([1.0_DFP], Matrix, SpaceTime, [1, 1, 1, 1])
  CASE (7)
    val = QuadratureVariable([1.0_DFP], Scalar, Space)
  CASE (8)
    val = QuadratureVariable(RESHAPE([1.0_DFP], [1, 1]), Vector, Space)
  CASE (9)
    val = QuadratureVariable(RESHAPE([1.0_DFP], [1, 1, 1]), Matrix, Space)
  CASE DEFAULT
    ERROR STOP 'invalid test mode'
  END SELECT
  SELECT CASE (MOD(mode - 1, 3))
  CASE (0)
    CALL GetLocalGradient(obj=obj, lg=lg2, val=val)
  CASE (1)
    CALL GetLocalGradient(obj=obj, lg=lg3, val=val)
  CASE (2)
    CALL GetLocalGradient(obj=obj, lg=lg4, val=val)
  CASE DEFAULT
    ERROR STOP 'invalid test rank'
  END SELECT
END IF
END PROGRAM main
