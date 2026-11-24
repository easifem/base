!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing element name, type, order and node count lookups

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Tetrahedron4
USE GlobalData, ONLY: Tetrahedron20
USE ReferenceTetrahedron_Method, ONLY: ElementName_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: ElementOrder_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: ElementType_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: TotalEntities_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: TotalNodesInElement_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: "Tetrahedron" is an alias of Tetrahedron4
BLOCK
  CALL OK(ElementType_Tetrahedron("Tetrahedron") .EQ. Tetrahedron4, &
          "Tetrahedron alias")
END BLOCK

! Test 2: cubic tetrahedron
BLOCK
  INTEGER(I4B) :: elemType
  elemType = ElementType_Tetrahedron("Tetrahedron20")
  CALL OK(elemType .EQ. Tetrahedron20, "Tetrahedron20 type")
  CALL OK(ElementName_Tetrahedron(elemType) .EQ. "Tetrahedron20", &
          "Tetrahedron20 name")
  CALL OK(ElementOrder_Tetrahedron(elemType) .EQ. 3, "Tetrahedron20 order")
  CALL OK(TotalNodesInElement_Tetrahedron(elemType) .EQ. 20, &
          "Tetrahedron20 nodes")
  CALL OK(ALL(TotalEntities_Tetrahedron(elemType) .EQ. [20, 6, 4, 1]), &
          "Tetrahedron20 entity counts")
END BLOCK

! Test 3: unknown names and types
BLOCK
  CALL OK(ElementType_Tetrahedron("tetrahedron4") .EQ. 0, &
          "lookup is case sensitive")
  CALL OK(ElementName_Tetrahedron(-1_I4B) .EQ. "NONE", "unknown type name")
  CALL OK(ElementOrder_Tetrahedron(-1_I4B) .EQ. 0, "unknown type order")
  CALL OK(TotalNodesInElement_Tetrahedron(-1_I4B) .EQ. 0, &
          "unknown type nodes")
END BLOCK

END PROGRAM main
