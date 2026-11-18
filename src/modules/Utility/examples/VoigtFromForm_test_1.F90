!> author: Shion Shimizu
! date: 2026-11-17
! summary: VoigtFromForm(vec, voigtForm, ans)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStrainFull
USE GlobalData, ONLY: VoigtFormUniaxialStrain
USE GlobalData, ONLY: VoigtFormUniaxialStrainFull
USE Test_Method, ONLY: OK
USE VoigtUtility, ONLY: GetVoigtVector_
USE VoigtUtility, ONLY: VoigtFromForm
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP

! Test 1: plane strain full [11, 22, 12, 33] to 3D
BLOCK
  REAL(DFP) :: ans(6), expected(6)

  CALL VoigtFromForm(vec=[1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP], &
                     voigtForm=VoigtFormPlaneStrainFull, ans=ans)
  expected = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 0.0_DFP, 0.0_DFP]
  CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "plane strain full")
END BLOCK

! Test 2: plane strain [11, 22, 12], the 33 component is zero
BLOCK
  REAL(DFP) :: ans(6), expected(6)

  CALL VoigtFromForm(vec=[1.0_DFP, 2.0_DFP, 4.0_DFP], &
                     voigtForm=VoigtFormPlaneStrain, ans=ans)
  expected = [1.0_DFP, 2.0_DFP, 0.0_DFP, 4.0_DFP, 0.0_DFP, 0.0_DFP]
  CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "plane strain")
END BLOCK

! Test 3: uniaxial strain [11] and uniaxial strain full [11, 22, 33]
BLOCK
  REAL(DFP) :: ans(6), expected(6)

  CALL VoigtFromForm(vec=[1.0_DFP], voigtForm=VoigtFormUniaxialStrain, &
                     ans=ans)
  expected = [1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
  CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "uniaxial strain")
  CALL VoigtFromForm(vec=[1.0_DFP, 2.0_DFP, 3.0_DFP], &
                     voigtForm=VoigtFormUniaxialStrainFull, ans=ans)
  expected = [1.0_DFP, 2.0_DFP, 3.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
  CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "uniaxial strain full")
END BLOCK

! Test 4: round trip through GetVoigtVector_ in 3D
BLOCK
  REAL(DFP) :: v(6), vec(6), ans(6)
  INTEGER(I4B) :: tsize

  v = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  CALL GetVoigtVector_(v=v, voigtForm=VoigtForm3D, ans=vec, tsize=tsize)
  CALL VoigtFromForm(vec=vec(1:tsize), voigtForm=VoigtForm3D, ans=ans)
  CALL OK(MAXVAL(ABS(ans - v)) .LT. tol, "3D: round trip")
END BLOCK

END PROGRAM main
