!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceQuadrangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE BaseType, ONLY: ReferenceQuadrangle_
USE BaseType, ONLY: ReferenceElement_
USE ReferenceQuadrangle_Method, ONLY: HighOrderElement_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(ReferenceQuadrangle_) :: ref
TYPE(ReferenceElement_) :: high

ref = ReferenceQuadrangle(nsd=2_I4B,domainName="UNIT")
CALL HighOrderElement_Quadrangle(refelem=ref,order=2_I4B, &
  obj=high,ipType=Equidistance)
CALL OK(SIZE(high%xij,2) == 9, "Quadratic node count")
CALL OK(high%order == 2, "Stored interpolation order")
END PROGRAM main
