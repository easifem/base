!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Convert a stress between the Cauchy, Kirchhoff, PK1, PK2
!   and Mandel measures
!
! The deformation gradient is upper triangular with det(F) = 2.

PROGRAM main
USE BaseType, ONLY: DeformationGradient_
USE BaseType, ONLY: Stress_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StressMeasureKirchhoff
USE GlobalData, ONLY: StressMeasureMandel
USE GlobalData, ONLY: StressMeasurePK1
USE GlobalData, ONLY: StressMeasurePK2
USE Rank2Tensor_Method, ONLY: CauchyStress
USE Rank2Tensor_Method, ONLY: ConvertStress
USE Rank2Tensor_Method, ONLY: DeformationGradient
USE Rank2Tensor_Method, ONLY: KirchhoffStress
USE Rank2Tensor_Method, ONLY: MandelStress
USE Rank2Tensor_Method, ONLY: PK1Stress
USE Rank2Tensor_Method, ONLY: PK2Stress
USE Rank2Tensor_Method, ONLY: Stress
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
                        [10.0_DFP, 2.0_DFP, 3.0_DFP, &
                         2.0_DFP, 20.0_DFP, 4.0_DFP, &
                         3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])
REAL(DFP), PARAMETER :: f0(3, 3) = RESHAPE( &
                        [2.0_DFP, 0.0_DFP, 0.0_DFP, &
                         0.5_DFP, 1.0_DFP, 0.0_DFP, &
                         0.0_DFP, 0.2_DFP, 1.0_DFP], [3, 3])

! Test 1: Kirchhoff stress tau = J sigma
BLOCK
  TYPE(Stress_) :: tau

  tau = KirchhoffStress(Stress(s0, StressMeasureCauchy), &
                        DeformationGradient(f0))
  CALL OK(MAXVAL(ABS(tau%T - 2.0_DFP * s0)) .LT. tol, "tau = J sigma")
  CALL OK(tau%stressType .EQ. StressMeasureKirchhoff, "tau: stressType")
  CALL OK(tau%isSym, "tau: isSym is true")
END BLOCK

! Test 2: first Piola-Kirchhoff stress P = F S
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(Stress_) :: sig, P, S

  F = DeformationGradient(f0)
  sig = Stress(s0, StressMeasureCauchy)
  P = PK1Stress(sig, F)
  S = PK2Stress(sig, F)
  CALL OK(MAXVAL(ABS(P%T - MATMUL(f0, S%T))) .LT. tol, "P = F S")
  CALL OK(P%stressType .EQ. StressMeasurePK1, "P: stressType")
  CALL OK(.NOT. P%isSym, "P: isSym is false")
  CALL OK(S%stressType .EQ. StressMeasurePK2, "S: stressType")
  CALL OK(S%isSym, "S: isSym is true")
END BLOCK

! Test 3: Mandel stress M = C S = F^T P
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(Stress_) :: sig, P, S, M

  F = DeformationGradient(f0)
  sig = Stress(s0, StressMeasureCauchy)
  P = PK1Stress(sig, F)
  S = PK2Stress(sig, F)
  M = MandelStress(sig, F)
  CALL OK(MAXVAL(ABS(M%T - MATMUL(MATMUL(TRANSPOSE(f0), f0), S%T))) &
          .LT. tol, "M = C S")
  CALL OK(MAXVAL(ABS(M%T - MATMUL(TRANSPOSE(f0), P%T))) .LT. tol, &
          "M = F^T P")
  CALL OK(M%stressType .EQ. StressMeasureMandel, "M: stressType")
  CALL OK(.NOT. M%isSym, "M: isSym is false")
END BLOCK

! Test 4: Cauchy stress back from PK2
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(Stress_) :: S, sig

  F = DeformationGradient(f0)
  S = PK2Stress(Stress(s0, StressMeasureCauchy), F)
  sig = CauchyStress(S, F)
  CALL OK(MAXVAL(ABS(sig%T - s0)) .LT. tol, "S -> sigma: components")
  CALL OK(sig%stressType .EQ. StressMeasureCauchy, &
          "S -> sigma: stressType")
  CALL OK(sig%isSym, "S -> sigma: isSym is true")
END BLOCK

! Test 5: with F = I all measures are equal
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(Stress_) :: sig, X
  INTEGER(I4B) :: a
  LOGICAL(LGT) :: isok

  F = DeformationGradient(Eye3)
  sig = Stress(s0, StressMeasureCauchy)
  isok = .TRUE.
  DO a = StressMeasureCauchy, StressMeasureMandel
    X = ConvertStress(sig, F, a)
    isok = isok .AND. MAXVAL(ABS(X%T - s0)) .LT. tol
    isok = isok .AND. X%stressType .EQ. a
  END DO
  CALL OK(isok, "F = I: all measures equal sigma")
END BLOCK

! Test 6: all 5 x 5 round trips A -> B -> A
BLOCK
  TYPE(DeformationGradient_) :: F
  TYPE(Stress_) :: sig, X, Y, Z
  INTEGER(I4B) :: a, b
  LOGICAL(LGT) :: isok

  F = DeformationGradient(f0)
  sig = Stress(s0, StressMeasureCauchy)
  isok = .TRUE.
  DO a = StressMeasureCauchy, StressMeasureMandel
    X = ConvertStress(sig, F, a)
    DO b = StressMeasureCauchy, StressMeasureMandel
      Y = ConvertStress(X, F, b)
      Z = ConvertStress(Y, F, a)
      isok = isok .AND. MAXVAL(ABS(Z%T - X%T)) .LT. tol
      isok = isok .AND. Y%stressType .EQ. b
    END DO
  END DO
  CALL OK(isok, "all round trips")
END BLOCK

END PROGRAM main
