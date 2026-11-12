!> author: Shion Shimizu
! date: 2026-11-02
! summary: ReduceVoigtTangent_ equals ReduceVoigtTangent in every form
!
! The tangent is made non-symmetric on purpose.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStress
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: ReduceVoigtTangent
USE MaterialJacobian_Method, ONLY: ReduceVoigtTangent_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
INTEGER(I4B), PARAMETER :: forms(4) = [VoigtForm3D, &
                                       VoigtFormPlaneStrain, &
                                       VoigtFormPlaneStress, &
                                       VoigtFormAxisymmetric]
CHARACTER(*), PARAMETER :: names(4) = ["3D          ", &
                                       "plane strain", &
                                       "plane stress", &
                                       "axisymmetric"]
REAL(DFP) :: C(6, 6)

C = IsotropicTangent(lambda=1.5_DFP, mu=0.5_DFP)
C(1, 4) = 0.1_DFP
C(5, 2) = 0.2_DFP

! Test 1: size and values in each Voigt form
BLOCK
  REAL(DFP) :: Cr(6, 6)
  REAL(DFP), ALLOCATABLE :: Cref(:, :)
  INTEGER(I4B) :: ii, nrow, ncol

  DO ii = 1, 4
    Cref = ReduceVoigtTangent(C, forms(ii))
    CALL ReduceVoigtTangent_(C, forms(ii), Cr, nrow, ncol)
    CALL OK(nrow .EQ. SIZE(Cref, 1) .AND. ncol .EQ. SIZE(Cref, 2), &
            TRIM(names(ii))//": nrow and ncol")
    CALL OK(MAXVAL(ABS(Cr(1:nrow, 1:ncol) - Cref)) .LT. tol, &
            TRIM(names(ii))//": values")
  END DO
END BLOCK

END PROGRAM main
