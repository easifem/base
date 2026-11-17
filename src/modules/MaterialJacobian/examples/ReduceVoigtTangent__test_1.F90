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
USE GlobalData, ONLY: VoigtFormPlaneStrainFull
USE GlobalData, ONLY: VoigtFormPlaneStress
USE GlobalData, ONLY: VoigtFormPlaneStressFull
USE GlobalData, ONLY: VoigtFormUniaxialStrain
USE GlobalData, ONLY: VoigtFormUniaxialStrainFull
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: ReduceVoigtTangent
USE MaterialJacobian_Method, ONLY: ReduceVoigtTangent_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
INTEGER(I4B), PARAMETER :: forms(8) = [VoigtForm3D, &
                                       VoigtFormPlaneStrain, &
                                       VoigtFormPlaneStress, &
                                       VoigtFormAxisymmetric, &
                                       VoigtFormUniaxialStrain, &
                                       VoigtFormPlaneStrainFull, &
                                       VoigtFormUniaxialStrainFull, &
                                       VoigtFormPlaneStressFull]
CHARACTER(*), PARAMETER :: names(8) = ["3D                  ", &
                                       "plane strain        ", &
                                       "plane stress        ", &
                                       "axisymmetric        ", &
                                       "uniaxial strain     ", &
                                       "plane strain full   ", &
                                       "uniaxial strain full", &
                                       "plane stress full   "]
REAL(DFP) :: C(6, 6)

C = IsotropicTangent(lambda=1.5_DFP, mu=0.5_DFP)
C(1, 4) = 0.1_DFP
C(5, 2) = 0.2_DFP

! Test 1: size and values in each Voigt form
BLOCK
  REAL(DFP) :: Cr(6, 6)
  REAL(DFP), ALLOCATABLE :: Cref(:, :)
  INTEGER(I4B) :: ii, nrow, ncol

  DO ii = 1, SIZE(forms)
    Cref = ReduceVoigtTangent(C, forms(ii))
    CALL ReduceVoigtTangent_(C, forms(ii), Cr, nrow, ncol)
    CALL OK(nrow .EQ. SIZE(Cref, 1) .AND. ncol .EQ. SIZE(Cref, 2), &
            TRIM(names(ii))//": nrow and ncol")
    CALL OK(MAXVAL(ABS(Cr(1:nrow, 1:ncol) - Cref)) .LT. tol, &
            TRIM(names(ii))//": values")
  END DO
END BLOCK

END PROGRAM main
