!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: VTK node order of Lagrange triangles of order 1 to 10
!
! Description:
!   For each order, GetVTKElementTypeFromTopology_ and
!   GetVTKElementType_ give nptrs, vtk_type and tsize. Node nptrs(i) of
!   the gmsh (easifem) element must be at the same place as VTK node i.
!
!   The gmsh points are EquidistancePoint_Triangle on the unit triangle.
!   The VTK points come from VtkPoint, a port of
!   vtkHigherOrderTriangle::BarycentricIndex, where VTK node i is at
!   (b(1), b(2)) / order.

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE GlobalData, ONLY: Triangle3
USE GlobalData, ONLY: Triangle6
USE GlobalData, ONLY: Triangle10
USE GlobalData, ONLY: Triangle15
USE GlobalData, ONLY: Triangle21
USE GlobalData, ONLY: Triangle28
USE GlobalData, ONLY: Triangle36
USE GlobalData, ONLY: Triangle45
USE GlobalData, ONLY: Triangle55
USE GlobalData, ONLY: Triangle66
USE ReferenceElement_Method, ONLY: GetVTKElementType_
USE ReferenceElement_Method, ONLY: GetVTKElementTypeFromTopology_
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: EquidistancePoint_Triangle
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: elemTypes(10) = [Triangle3, Triangle6, &
                                            Triangle10, Triangle15, &
                                            Triangle21, Triangle28, &
                                            Triangle36, Triangle45, &
                                            Triangle55, Triangle66]
INTEGER(I4B), PARAMETER :: vtkTypes(10) = [5, 22, 69, 69, 69, 69, &
                                           69, 69, 69, 69]
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
INTEGER(I4B) :: order, nns, tsize, tsize2, ii
INTEGER(I4B) :: nptrs(66), nptrs2(66)
INTEGER(INT8) :: vtk_type, vtk_type2
REAL(DFP), ALLOCATABLE :: xg(:, :)
REAL(DFP) :: xv(2)
LOGICAL :: isok
CHARACTER(2) :: str

DO order = 1, 10
  WRITE (str, "(I2)") order
  nns = (order + 1) * (order + 2) / 2

  CALL GetVTKElementTypeFromTopology_(TypeElemNameOpt%triangle, order, &
                                      vtk_type, nptrs, tsize)
  CALL OK(tsize .EQ. nns, "order "//str//": tsize")
  CALL OK(vtk_type .EQ. vtkTypes(order), "order "//str//": vtk_type")

  CALL GetVTKElementType_(elemTypes(order), vtk_type2, nptrs2, tsize2)
  CALL OK(tsize2 .EQ. tsize .AND. vtk_type2 .EQ. vtk_type .AND. &
          ALL(nptrs2(1:tsize) .EQ. nptrs(1:tsize)), &
          "order "//str//": elemType gives the same")

  xg = EquidistancePoint_Triangle(order=order)
  isok = .TRUE.
  DO ii = 1, tsize
    xv = VtkPoint(ii - 1, order)
    isok = isok .AND. ALL(ABS(xg(1:2, nptrs(ii)) - xv) .LT. tol)
  END DO
  CALL OK(isok, "order "//str//": gmsh nodes are at the VTK places")
END DO

! unsupported order
CALL GetVTKElementTypeFromTopology_(TypeElemNameOpt%triangle, 11_I4B, &
                                    vtk_type, nptrs, tsize)
CALL OK(tsize .EQ. 0 .AND. vtk_type .EQ. -1, "order 11 is unsupported")

CONTAINS

! Parametric point of VTK Lagrange triangle node index (0-based)
FUNCTION VtkPoint(index0, order0) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: index0, order0
  REAL(DFP) :: ans(2)
  INTEGER(I4B) :: b(0:2), idx, n, nmax, nmin, dim, offset

  idx = index0
  n = order0
  nmax = order0
  nmin = 0
  DO WHILE (idx .NE. 0 .AND. idx .GE. 3 * n)
    idx = idx - 3 * n
    nmax = nmax - 2
    nmin = nmin + 1
    n = n - 3
  END DO

  IF (idx .LT. 3) THEN
    b(idx) = nmin
    b(MOD(idx + 1, 3)) = nmin
    b(MOD(idx + 2, 3)) = nmax
  ELSE
    idx = idx - 3
    dim = idx / (n - 1)
    offset = idx - dim * (n - 1)
    b(MOD(dim + 1, 3)) = nmin
    b(MOD(dim + 2, 3)) = (nmax - 1) - offset
    b(dim) = nmin + 1 + offset
  END IF

  ans = REAL(b(0:1), DFP) / REAL(order0, DFP)
END FUNCTION VtkPoint

END PROGRAM main
