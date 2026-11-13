!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceTetrahedron_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: ReferenceTetrahedron_
USE ReferenceTetrahedron_Method, ONLY: Initiate
USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron_Pointer
USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(ReferenceTetrahedron_) :: a,b
CLASS(ReferenceTetrahedron_), POINTER :: p
REAL(DFP) :: x(3,4)

CALL Initiate(obj=a,nsd=3_I4B,domainName="UNIT")
b = ReferenceTetrahedron(nsd=3_I4B,domainName="UNIT")
p => ReferenceTetrahedron_Pointer(nsd=3_I4B,domainName="UNIT")
x = RefCoord_Tetrahedron("UNIT")
CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
CALL OK(ASSOCIATED(p), "Pointer constructor allocates a target")
CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Reference coordinate rows")
DEALLOCATE(p)
END PROGRAM main
