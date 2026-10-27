!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Total nodes from topology name and order
!
! Description:
!   Checks GetTotalNodes(topoName, order) for every topology, compares
!   the linear case with GetTotalNodes(elemType), and checks that an
!   unknown topology gives 0.

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Triangle3
USE GlobalData, ONLY: Hexahedron8
USE ReferenceElement_Method, ONLY: GetTotalNodes
USE Test_Method, ONLY: OK
IMPLICIT NONE

CALL OK(GetTotalNodes(TypeElemNameOpt%point, 1_I4B) .EQ. 1_I4B, &
        "point")
CALL OK(GetTotalNodes(TypeElemNameOpt%line, 3_I4B) .EQ. 4_I4B, &
        "line, order 3")
CALL OK(GetTotalNodes(TypeElemNameOpt%triangle, 2_I4B) .EQ. 6_I4B, &
        "triangle, order 2")
CALL OK(GetTotalNodes(TypeElemNameOpt%quadrangle, 2_I4B) .EQ. 9_I4B, &
        "quadrangle, order 2")
CALL OK(GetTotalNodes(TypeElemNameOpt%tetrahedron, 2_I4B) .EQ. 10_I4B, &
        "tetrahedron, order 2")
CALL OK(GetTotalNodes(TypeElemNameOpt%hexahedron, 2_I4B) .EQ. 27_I4B, &
        "hexahedron, order 2")
CALL OK(GetTotalNodes(TypeElemNameOpt%prism, 2_I4B) .EQ. 18_I4B, &
        "prism, order 2")
CALL OK(GetTotalNodes(TypeElemNameOpt%pyramid, 2_I4B) .EQ. 14_I4B, &
        "pyramid, order 2")

! order 1 gives the same as the linear element type
CALL OK(GetTotalNodes(TypeElemNameOpt%triangle, 1_I4B) .EQ. &
        GetTotalNodes(Triangle3), "triangle, order 1 = Triangle3")
CALL OK(GetTotalNodes(TypeElemNameOpt%hexahedron, 1_I4B) .EQ. &
        GetTotalNodes(Hexahedron8), "hexahedron, order 1 = Hexahedron8")

! unknown topology
CALL OK(GetTotalNodes(-1_I4B, 2_I4B) .EQ. 0_I4B, "unknown topology")

END PROGRAM main
! ok 1 - point
! ok 2 - line, order 3
! ok 3 - triangle, order 2
! ok 4 - quadrangle, order 2
! ok 5 - tetrahedron, order 2
! ok 6 - hexahedron, order 2
! ok 7 - prism, order 2
! ok 8 - pyramid, order 2
! ok 9 - triangle, order 1 = Triangle3
! ok 10 - hexahedron, order 1 = Hexahedron8
! ok 11 - unknown topology
