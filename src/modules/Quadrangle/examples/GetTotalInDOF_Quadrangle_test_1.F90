!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing GetTotalInDOF_Quadrangle and LagrangeInDOF_Quadrangle

PROGRAM main
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: GetTotalInDOF_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: LagrangeInDOF_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: a linear element has no interior DOF
BLOCK
  CALL OK(GetTotalInDOF_Quadrangle(order=1_I4B, baseContinuity="H1", &
          baseInterpolation="LAGR") .EQ. 0, "order=1: no interior DOF")
  CALL OK(LagrangeInDOF_Quadrangle(order=1_I4B) .EQ. 0, &
          "LagrangeInDOF order=1: no interior DOF")
END BLOCK

! Test 2: cubic element, (p+1)^2 total and (p-1)^2 interior DOF
BLOCK
  CALL OK(GetTotalDOF_Quadrangle(order=3_I4B, baseContinuity="H1", &
          baseInterpolation="LAGR") .EQ. 16, "order=3: 16 DOF")
  CALL OK(GetTotalInDOF_Quadrangle(order=3_I4B, baseContinuity="H1", &
          baseInterpolation="LAGR") .EQ. 4, "order=3: 4 interior DOF")
END BLOCK

! Test 3: anisotropic orders p=2, q=3
BLOCK
  CALL OK(GetTotalInDOF_Quadrangle(p=2_I4B, q=3_I4B, baseContinuity="H1", &
          baseInterpolation="LAGR") .EQ. 2, "p=2, q=3: 2 interior DOF")
  CALL OK(LagrangeInDOF_Quadrangle(p=2_I4B, q=3_I4B) .EQ. 2, &
          "LagrangeInDOF p=2, q=3: 2 interior DOF")
  CALL OK(LagrangeDOF_Quadrangle(p=2_I4B, q=3_I4B) .EQ. 12, &
          "LagrangeDOF p=2, q=3: 12 DOF")
END BLOCK

END PROGRAM main
