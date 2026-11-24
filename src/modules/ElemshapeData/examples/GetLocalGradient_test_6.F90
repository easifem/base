!> author: OpenAI
! date: 2026-11-21
! summary: Debug rejection of matrix gradients returned as FEVariables

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: GetLocalGradient
USE ElemShapeData_Method, ONLY: LocalGradient
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: expectedError = &
                           'GetLocalGradient: matrix FEVariable result unsupported'
TYPE(ElemShapeData_) :: obj
TYPE(STElemShapeData_) :: st(1)
TYPE(FEVariable_) :: val, lg
REAL(DFP) :: values(2, 2, 1)
INTEGER :: mode, status, commandStatus, unit, ios, tick
CHARACTER(1024) :: executable, command, line, message
CHARACTER(160) :: logfile
CHARACTER(16) :: argument, stamp
LOGICAL :: found

IF (COMMAND_ARGUMENT_COUNT() .EQ. 0) THEN
  CALL GET_COMMAND_ARGUMENT(0, executable)
  CALL SYSTEM_CLOCK(tick)
  WRITE (stamp, '(I0)') tick
  ! Test 1: subroutine and function paths in space and space-time
  DO mode = 1, 4
    WRITE (argument, '(I0)') mode
    logfile = '/tmp/easifem-524-'//TRIM(stamp)//'-'//TRIM(argument)
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
        IF (INDEX(line, expectedError) .GT. 0) found = .TRUE.
      END DO scanLog
      CLOSE (unit)
    END IF
    CALL OK(commandStatus .EQ. 0 .AND. status .NE. 0 .AND. found, &
            'matrix result case '//TRIM(argument)//': debug rejection')
  END DO
ELSE
  CALL GET_COMMAND_ARGUMENT(1, argument)
  READ (argument, *) mode
  obj%nns = 1
  obj%nips = 1
  obj%xidim = 1
  obj%dNdXi = RESHAPE([1.0_DFP], [1, 1, 1])
  st(1)%nns = 1
  st(1)%nips = 1
  st(1)%xidim = 1
  st(1)%nnt = 1
  st(1)%dNdXi = obj%dNdXi
  st(1)%T = [1.0_DFP]
  values = 2.0_DFP
  val = NodalVariable(values, Matrix, Space)
  lg = NodalVariable(7.0_DFP, Scalar, Constant)
  SELECT CASE (mode)
  CASE (1)
    CALL GetLocalGradient(obj=obj, lg=lg, val=val)
  CASE (2)
    CALL GetLocalGradient(obj=st, lg=lg, val=val)
  CASE (3)
    lg = LocalGradient(obj=obj, val=val)
  CASE (4)
    lg = LocalGradient(obj=st, val=val)
  CASE DEFAULT
    ERROR STOP 'invalid test mode'
  END SELECT
END IF
END PROGRAM main
