!> author: OpenAI
! date: 2026-11-22
! summary: Debug error for an element type without Lagrange nodes

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: expectedError = 'LagrangeDOF returned zero DOF'
REAL(DFP), PARAMETER :: coord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], [1, 2])
TYPE(ElemShapeData_) :: obj
TYPE(QuadraturePoint_) :: quad
INTEGER :: status, commandStatus, unit, ios, tick
CHARACTER(1024) :: executable, command, line, message
CHARACTER(160) :: logfile
CHARACTER(16) :: stamp
LOGICAL :: found, continued

IF (COMMAND_ARGUMENT_COUNT() .EQ. 0) THEN
  CALL GET_COMMAND_ARGUMENT(0, executable)
  CALL SYSTEM_CLOCK(tick)
  WRITE (stamp, '(I0)') tick
  logfile = '/tmp/easifem-519-'//TRIM(stamp)
  ! Test 1: a debug build reports the error and stops (STOP exits with 0)
  command = '"'//TRIM(executable)//'" run > "'//TRIM(logfile)//'" 2>&1'
  CALL EXECUTE_COMMAND_LINE(TRIM(command), exitstat=status, &
                            cmdstat=commandStatus, cmdmsg=message)
  found = .FALSE.
  continued = .FALSE.
  OPEN (newunit=unit, file=TRIM(logfile), status='old', action='read', &
        iostat=ios, iomsg=message)
  IF (ios .EQ. 0) THEN
    scanLog: DO
      READ (unit, '(A)', iostat=ios, iomsg=message) line
      IF (ios .NE. 0) EXIT scanLog
      IF (INDEX(line, expectedError) .GT. 0) found = .TRUE.
      IF (INDEX(line, 'REACHED AFTER CALL') .GT. 0) continued = .TRUE.
    END DO scanLog
    CLOSE (unit)
  END IF
  CALL OK(commandStatus .EQ. 0 .AND. found .AND. .NOT. continued, &
          "zero DOF: debug error message, then the program stops")
ELSE
  CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                   nips=[2], quadratureType=qpopt%GaussLegendre)
  CALL LagrangeElemShapeData(obj=obj, quad=quad, nsd=1, xidim=1, &
                             elemType=0, refelemCoord=coord, &
                             domainName="BIUNIT", order=1)
  WRITE (*, '(A)') 'REACHED AFTER CALL'
END IF
END PROGRAM main
