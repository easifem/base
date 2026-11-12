!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: MaterialJacobian constructor from a Voigt matrix or a rank-4
!   array

PROGRAM main
USE BaseType, ONLY: MaterialJacobian_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: StrainMeasureGreen
USE GlobalData, ONLY: StrainMeasureSmall
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StressMeasurePK2
USE GlobalData, ONLY: StressRateNone
USE MaterialJacobian_Method, ONLY: ASSIGNMENT(=)
USE MaterialJacobian_Method, ONLY: Display
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE MaterialJacobian_Method, ONLY: Rank4SymIdentity
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: from a 6x6 Voigt matrix with the default tags
BLOCK
  TYPE(MaterialJacobian_) :: obj
  REAL(DFP) :: C(6, 6)

  obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
  CALL Display(obj, "isotropic tangent")
  C = obj
  CALL OK(MAXVAL(ABS(C - IsotropicTangent(2.0_DFP, 3.0_DFP))) .LT. tol, &
          "matrix: C = obj")
  CALL OK(obj%stressType .EQ. StressMeasureCauchy, "default stressType")
  CALL OK(obj%strainType .EQ. StrainMeasureSmall, "default strainType")
  CALL OK(obj%rateType .EQ. StressRateNone, "default rateType")
END BLOCK

! Test 2: from a 6x6 Voigt matrix with tags
BLOCK
  TYPE(MaterialJacobian_) :: obj

  obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP), &
                         stressType=StressMeasurePK2, &
                         strainType=StrainMeasureGreen)
  CALL OK(obj%stressType .EQ. StressMeasurePK2, "tags: stressType")
  CALL OK(obj%strainType .EQ. StrainMeasureGreen, "tags: strainType")
END BLOCK

! Test 3: from a rank-4 array, symmetric identity from Kronecker deltas
BLOCK
  TYPE(MaterialJacobian_) :: obj
  REAL(DFP) :: S4(3, 3, 3, 3)
  INTEGER(I4B) :: i, j, k, l

  DO l = 1, 3
    DO k = 1, 3
      DO j = 1, 3
        DO i = 1, 3
          S4(i, j, k, l) = 0.5_DFP * (Eye3(i, k) * Eye3(j, l) &
                                      + Eye3(i, l) * Eye3(j, k))
        END DO
      END DO
    END DO
  END DO
  obj = MaterialJacobian(S4)
  CALL OK(MAXVAL(ABS(obj%C - Rank4SymIdentity())) .LT. tol, &
          "rank-4: Isym from deltas")
END BLOCK

END PROGRAM main
