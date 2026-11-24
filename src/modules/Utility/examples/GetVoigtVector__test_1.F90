!> author: Shion Shimizu
! date: 2026-11-17
! summary: GetVoigtVector_(v, voigtForm, ans, tsize) of a 3D Voigt array

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
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
REAL(DFP), PARAMETER :: v(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, &
                                4.0_DFP, 5.0_DFP, 6.0_DFP]

! Test 1: 3D form writes all 6 entries
BLOCK
  REAL(DFP) :: ans(6)
  INTEGER(I4B) :: tsize

  CALL GetVoigtVector_(v=v, voigtForm=VoigtForm3D, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 6, "3D: tsize")
  CALL OK(MAXVAL(ABS(ans - v)) .LT. tol, "3D: values")
END BLOCK

! Test 2: plane strain writes [11, 22, 12], the rest is untouched
BLOCK
  REAL(DFP) :: ans(6)
  INTEGER(I4B) :: tsize

  ans = -1.0_DFP
  CALL GetVoigtVector_(v=v, voigtForm=VoigtFormPlaneStrain, ans=ans, &
                       tsize=tsize)
  CALL OK(tsize .EQ. 3, "plane strain: tsize")
  CALL OK(MAXVAL(ABS(ans(1:3) - [1.0_DFP, 2.0_DFP, 4.0_DFP])) .LT. tol, &
          "plane strain: values")
  CALL OK(ALL(ans(4:6) .EQ. -1.0_DFP), "plane strain: rest untouched")
END BLOCK

! Test 3: plane strain full keeps the 33 component, [11, 22, 12, 33]
BLOCK
  REAL(DFP) :: ans(4)
  INTEGER(I4B) :: tsize

  CALL GetVoigtVector_(v=v, voigtForm=VoigtFormPlaneStrainFull, ans=ans, &
                       tsize=tsize)
  CALL OK(tsize .EQ. 4, "plane strain full: tsize")
  CALL OK(MAXVAL(ABS(ans - [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP])) &
          .LT. tol, "plane strain full: values")
END BLOCK

! Test 4: uniaxial strain [11] and uniaxial strain full [11, 22, 33]
BLOCK
  REAL(DFP) :: ans(3)
  INTEGER(I4B) :: tsize

  CALL GetVoigtVector_(v=v, voigtForm=VoigtFormUniaxialStrain, ans=ans, &
                       tsize=tsize)
  CALL OK(tsize .EQ. 1, "uniaxial strain: tsize")
  CALL OK(ABS(ans(1) - 1.0_DFP) .LT. tol, "uniaxial strain: values")
  CALL GetVoigtVector_(v=v, voigtForm=VoigtFormUniaxialStrainFull, &
                       ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 3, "uniaxial strain full: tsize")
  CALL OK(MAXVAL(ABS(ans - [1.0_DFP, 2.0_DFP, 3.0_DFP])) .LT. tol, &
          "uniaxial strain full: values")
END BLOCK

END PROGRAM main
