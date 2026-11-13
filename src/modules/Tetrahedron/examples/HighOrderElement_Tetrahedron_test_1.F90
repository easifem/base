!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceTetrahedron_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE BaseType, ONLY: ReferenceTetrahedron_
USE BaseType, ONLY: ReferenceElement_
USE ReferenceTetrahedron_Method, ONLY: HighOrderElement_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(ReferenceTetrahedron_) :: ref
TYPE(ReferenceElement_) :: high

ref = ReferenceTetrahedron(nsd=3_I4B,domainName="UNIT")
CALL HighOrderElement_Tetrahedron(refelem=ref,order=2_I4B, &
  obj=high,ipType=Equidistance)
CALL OK(SIZE(high%xij,2) == 10, "Quadratic node count")
CALL OK(high%order == 2, "Stored interpolation order")
END PROGRAM main
