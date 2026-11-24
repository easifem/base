!> author: OpenAI
! date: 2026-11-21
! summary: Debug rejection of time-only projection velocities

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetProjectionOfdNTdXt_
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(STElemShapeData_) :: obj(1)
TYPE(FEVariable_) :: c
REAL(DFP) :: single(1, 2, 1), batch(1, 2, 1, 1), point(1, 2)
INTEGER(I4B) :: d1, d2, d3, d4
INTEGER :: mode, status, commandStatus, unit, ios, tick
CHARACTER(1024) :: executable, command, line, message
CHARACTER(160) :: logfile
CHARACTER(16) :: argument, stamp
LOGICAL :: found

IF (COMMAND_ARGUMENT_COUNT() .EQ. 0) THEN
  CALL GET_COMMAND_ARGUMENT(0, executable)
  CALL SYSTEM_CLOCK(tick)
  WRITE (stamp, '(I0)') tick
  ! Test 1: each nonallocating overload must reject the time-only input
  DO mode = 1, 3
    WRITE (argument, '(I0)') mode
    logfile = '/tmp/easifem-529-NT-'//TRIM(stamp)//'-'//TRIM(argument)
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
        IF (INDEX(line, 'NTdXt projection: time-only c unsupported') &
            .GT. 0) found = .TRUE.
      END DO scanLog
      CLOSE (unit)
    END IF
    CALL OK(commandStatus .EQ. 0 .AND. status .NE. 0 .AND. found, &
            'NTdXt overload '//TRIM(argument)//': intended debug rejection')
  END DO
ELSE
  CALL GET_COMMAND_ARGUMENT(1, argument)
  READ (argument, *) mode
  obj(1)%nns = 1
  obj(1)%nnt = 2
  obj(1)%nips = 1
  obj(1)%nsd = 1
  obj(1)%N = RESHAPE([1.0_DFP], [1, 1])
  obj(1)%T = [0.5_DFP, 0.5_DFP]
  ALLOCATE (obj(1)%dNTdXt(1, 2, 1, 1))
  obj(1)%dNTdXt = 1.0_DFP
  c = NodalVariable([2.0_DFP, 2.0_DFP], Vector, Time, [1, 2])
  SELECT CASE (mode)
  CASE (1)
    CALL GetProjectionOfdNTdXt_(obj=obj(1), c=c, crank=Vector, &
                                ans=single, dim1=d1, dim2=d2, dim3=d3)
  CASE (2)
    CALL GetProjectionOfdNTdXt_(obj=obj, c=c, crank=Vector, ans=batch, &
                                dim1=d1, dim2=d2, dim3=d3, dim4=d4)
  CASE (3)
    CALL GetProjectionOfdNTdXt_(obj=obj, c=c, crank=Vector, ans=point, &
                                nrow=d1, ncol=d2, ips=1, ipt=1)
  CASE DEFAULT
    ERROR STOP 'invalid test mode'
  END SELECT
END IF
END PROGRAM main
