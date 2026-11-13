!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceQuadrangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: ReferenceQuadrangle_
USE ReferenceQuadrangle_Method, ONLY: Initiate
USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle_Pointer
USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: RefQuadrangleCoord
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(ReferenceQuadrangle_) :: a,b
CLASS(ReferenceQuadrangle_), POINTER :: p
REAL(DFP) :: x(2,4)

CALL Initiate(obj=a,nsd=2_I4B,domainName="UNIT")
b = ReferenceQuadrangle(nsd=2_I4B,domainName="UNIT")
p => ReferenceQuadrangle_Pointer(nsd=2_I4B,domainName="UNIT")
x = RefCoord_Quadrangle("UNIT")
CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
CALL OK(ASSOCIATED(p), "Pointer constructor allocates a target")
CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Reference coordinate rows")
DEALLOCATE(p)
CALL OK(MAXVAL(ABS(x-RefQuadrangleCoord("UNIT"))) &
  < 1.0E-12_DFP, "Coordinate aliases agree")
END PROGRAM main
