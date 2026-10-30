! This program is a part of EASIFEM library
! Copyright (C) 2020-2021 Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>
!

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Triangle geometry and interpolation utilities.
!
! Geometry kernels were adapted by Vikas Sharma from John Burkardt; the
! original source attribution names assets/geometry_burkardt_triangle.inc.
! Vertex coordinates are columns. Planar area is signed; spatial area is
! nonnegative. Distance routines measure distance to triangle edges, not the
! filled face. See the diameter and area-vector caveats in their public
! documentation.
!
! Arguments:
!   None at module scope; see individual public interfaces.
!
! Returns:
!   Geometry, reference element and interpolation procedures.
!
! Formula:
! $$A=\tfrac12\|(v_2-v_1)\times(v_3-v_1)\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_area_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_area_2d(t2)
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, "Right triangle area")
! END PROGRAM main
!```

MODULE Triangle_Method
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: DFP
IMPLICIT NONE
PRIVATE
PUBLIC :: triangle_angles_2d
PUBLIC :: triangle_angles_3d
PUBLIC :: triangle_area_2d
PUBLIC :: triangle_area_3d
PUBLIC :: triangle_area_3d_2
PUBLIC :: triangle_area_3d_3
PUBLIC :: triangle_area_heron
PUBLIC :: triangle_area_vector_3d
PUBLIC :: triangle_barycentric_2d
PUBLIC :: triangle_centroid_2d
PUBLIC :: triangle_centroid_3d
PUBLIC :: triangle_circumcenter_2d
PUBLIC :: triangle_circumcenter_2d_2
PUBLIC :: triangle_circumcenter
PUBLIC :: triangle_circumcircle_2d
PUBLIC :: triangle_circumcircle_2d_2
PUBLIC :: triangle_circumradius_2d
PUBLIC :: triangle_contains_line_exp_3d
PUBLIC :: triangle_contains_line_par_3d
PUBLIC :: triangle_contains_point_2d_1
PUBLIC :: triangle_contains_point_2d_2
PUBLIC :: triangle_contains_point_2d_3
PUBLIC :: triangle_diameter_2d
PUBLIC :: triangle_edge_length_2d
PUBLIC :: triangle_incenter_2d
PUBLIC :: triangle_incircle_2d
PUBLIC :: triangle_inradius_2d
PUBLIC :: triangle_orthocenter_2d
PUBLIC :: triangle_point_dist_2d
PUBLIC :: triangle_point_dist_3d
PUBLIC :: triangle_point_dist_signed_2d
PUBLIC :: triangle_point_near_2d
PUBLIC :: triangle_quality_2d
PUBLIC :: triangle_sample

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Computes the angles of a triangle in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   angle : REAL(DFP)(3).
!
! Formula:
! $$\theta_1+\theta_2+\theta_3=\pi$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_angles_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_angles_2d(t2)
! CALL OK(ABS(SUM(y)-ACOS(-1.0_DFP)) < 1.0E-11_DFP, &
!   "Interior angles sum to pi radians")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_angles_2d(t) RESULT(angle)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex in xij format
    REAL(DFP) :: angle(3)
    !! The angles opposite sides P1-P2, P2-P3 and P3-P1, in radians.
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Computes the angles of a triangle in 3D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   angle : REAL(DFP)(3).
!
! Formula:
! $$\theta_1+\theta_2+\theta_3=\pi$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_angles_3d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_angles_3d(t3)
! CALL OK(ABS(SUM(y)-ACOS(-1.0_DFP)) < 1.0E-11_DFP, &
!   "Interior angles sum to pi radians")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_angles_3d(t) RESULT(angle)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertices in xij format
    REAL(DFP) :: angle(3)
    !! angle
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Compute area of the triangle.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle. The 2D
! area is signed and changes sign on vertex reversal.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   area : REAL(DFP).
!
! Formula:
! $$A=\tfrac12\|(v_2-v_1)\times(v_3-v_1)\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_area_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_area_2d(t2)
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, "Right triangle area")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_area_2d(t) RESULT(area)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: area
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Compute area of triangle in 3D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   area : REAL(DFP).
!
! Formula:
! $$A=\tfrac12\|(v_2-v_1)\times(v_3-v_1)\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_area_3d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_area_3d(t3)
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, "Right triangle area")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_area_3d(t) RESULT(area)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: area
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Area of triangle in 3D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   area : REAL(DFP).
!
! Formula:
! $$A=\tfrac12\|(v_2-v_1)\times(v_3-v_1)\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_area_3d_2
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_area_3d_2(t3)
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, "Right triangle area")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_area_3d_2(t) RESULT(area)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: area
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Area of triangle using Heron's formula.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   area : REAL(DFP).
!
! Formula:
! $$A=\tfrac12\|(v_2-v_1)\times(v_3-v_1)\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_area_3d_3
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_area_3d_3(t3)
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, "Right triangle area")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_area_3d_3(t) RESULT(area)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: area
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Area of triangle using Herons formula.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   s : REAL(DFP), INTENT(IN)(3). Use the declaration and description for this
! overload.
!
! Returns:
!   area : REAL(DFP).
!
! Formula:
! $$A=\tfrac12\|(v_2-v_1)\times(v_3-v_1)\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_area_heron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_area_heron([2.0_DFP, 1.0_DFP, SQRT(5.0_DFP)])
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, "Right triangle area")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_area_heron(s) RESULT(area)
    REAL(DFP), INTENT(IN) :: s(3)
    REAL(DFP) :: area
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Compute the area vector of a tri in 3D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle. The
! returned vector is the full cross product, not half of it.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   area_vector : REAL(DFP)(3).
!
! Formula:
! $$a=(v_2-v_1)\times(v_3-v_1),\quad \|a\|=2A$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_area_vector_3d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_area_vector_3d(t3)
! CALL OK(MAXVAL(ABS(y-[0.0_DFP, 0.0_DFP, 2.0_DFP])) &
!   < 1.0E-11_DFP, "Cross product is twice the oriented area")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_area_vector_3d(t) RESULT(area_vector)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: area_vector(3)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Find the barycentric coordinates of a point in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   p : REAL(DFP), INTENT(IN)(2). Query point or output point as indicated by
! INTENT.
!
! Returns:
!   xsi : REAL(DFP)(3).
!
! Formula:
! $$p=\sum_i\lambda_i v_i,\quad\sum_i\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_barycentric_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_barycentric_2d(t2, [0.5_DFP, 0.25_DFP])
! CALL OK(ABS(SUM(y)-1.0_DFP) < 1.0E-11_DFP, "Barycentric sum")
! CALL OK(MAXVAL(ABS(MATMUL(t2, y)- &
!   [0.5_DFP, 0.25_DFP])) < 1.0E-11_DFP, "Reconstruct point")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_barycentric_2d(t, p) RESULT(xsi)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex
    REAL(DFP), INTENT(IN) :: p(2)
    !! point
    REAL(DFP) :: xsi(3)
    !! barycentric points
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Compute the centroid of a triangle in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   centroid : REAL(DFP)(2).
!
! Formula:
! $$c=(v_1+v_2+v_3)/3$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_centroid_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y(2)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_centroid_2d(t2)
! CALL OK(MAXVAL(ABS(y-SUM(t2, DIM=2)/3.0_DFP)) &
!   < 1.0E-11_DFP, "Centroid is vertex mean")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_centroid_2d(t) RESULT(centroid)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: centroid(2)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Computes the centroid of a triangle in 3D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   centroid : REAL(DFP)(3).
!
! Formula:
! $$c=(v_1+v_2+v_3)/3$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_centroid_3d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_centroid_3d(t3)
! CALL OK(MAXVAL(ABS(y-SUM(t3, DIM=2)/3.0_DFP)) &
!   < 1.0E-11_DFP, "Centroid is vertex mean")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_centroid_3d(t) RESULT(centroid)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: centroid(3)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Cirumcenter of a triangle in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   pc : REAL(DFP)(2).
!
! Formula:
! $$\|c-v_1\|=\|c-v_2\|=\|c-v_3\|=R$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_circumcenter_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y(2)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_circumcenter_2d(t2)
! CALL OK(MAXVAL(ABS(y-[1.0_DFP, 0.5_DFP])) < 1.0E-11_DFP, &
!   "Circumcenter is hypotenuse midpoint")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_circumcenter_2d(t) RESULT(pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: pc(2)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Circum center in 2d.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   pc : REAL(DFP)(2).
!
! Formula:
! $$\|c-v_1\|=\|c-v_2\|=\|c-v_3\|=R$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_circumcenter_2d_2
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y(2)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_circumcenter_2d_2(t2)
! CALL OK(MAXVAL(ABS(y-[1.0_DFP, 0.5_DFP])) < 1.0E-11_DFP, &
!   "Circumcenter is hypotenuse midpoint")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_circumcenter_2d_2(t) RESULT(pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: pc(2)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Computes the circumcenter of a triangle in ND.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Number of samples, coordinate dimension or
! grid count as specified by the routine.
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   p : REAL(DFP)(n).
!
! Formula:
! $$\|c-v_1\|=\|c-v_2\|=\|c-v_3\|=R$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_circumcenter
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y(2)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_circumcenter(2_I4B, t2)
! CALL OK(MAXVAL(ABS(y-[1.0_DFP, 0.5_DFP])) < 1.0E-11_DFP, &
!   "Circumcenter is hypotenuse midpoint")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_circumcenter(n, t) RESULT(p)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! shape (n,3)
    REAL(DFP) :: p(n)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Computes the circumcircle of a triangle in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   pc : REAL(DFP), INTENT(OUT)(2). Circle center or orthocenter.
!   r : REAL(DFP), INTENT(OUT). Circle radius.
!
! Returns:
!   Updates pc(2), r.
!
! Formula:
! $$R=abc/(4A)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_circumcircle_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y, center(2)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! CALL triangle_circumcircle_2d(t2, y, center)
! CALL OK(ABS(y-SQRT(5.0_DFP)/2.0_DFP) < 1.0E-11_DFP, &
!   "Circumradius of right triangle")
! CALL OK(MAXVAL(ABS(center-[1.0_DFP, 0.5_DFP])) &
!   < 1.0E-11_DFP, "Circumcenter")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE triangle_circumcircle_2d(t, r, pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(OUT) :: pc(2)
    !! circum center
    REAL(DFP), INTENT(OUT) :: r
    !! circum radius
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Circumcircle.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   pc : REAL(DFP), INTENT(OUT)(2). Circle center or orthocenter.
!   r : REAL(DFP), INTENT(OUT). Circle radius.
!
! Returns:
!   Updates pc(2), r.
!
! Formula:
! $$R=abc/(4A)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_circumcircle_2d_2
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y, center(2)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! CALL triangle_circumcircle_2d_2(t2, y, center)
! CALL OK(ABS(y-SQRT(5.0_DFP)/2.0_DFP) < 1.0E-11_DFP, &
!   "Circumradius of right triangle")
! CALL OK(MAXVAL(ABS(center-[1.0_DFP, 0.5_DFP])) &
!   < 1.0E-11_DFP, "Circumcenter")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE triangle_circumcircle_2d_2(t, r, pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(OUT) :: pc(2)
    !! circum center
    REAL(DFP), INTENT(OUT) :: r
    !! circum radius
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Triangle circumradius in 2d.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   r : REAL(DFP).
!
! Formula:
! $$R=abc/(4A)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_circumradius_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_circumradius_2d(t2)
! CALL OK(ABS(y-SQRT(5.0_DFP)/2.0_DFP) < 1.0E-11_DFP, &
!   "Circumradius")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_circumradius_2d(t) RESULT(r)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: r
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Is a line inside the triangle in 3D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   p1 : REAL(DFP), INTENT(IN)(3). First explicit line endpoint.
!   p2 : REAL(DFP), INTENT(IN)(3). Second explicit line endpoint.
!   inside : LOGICAL(LGT), INTENT(OUT). Whether the point or line intersection
! is contained.
!   pint : REAL(DFP), INTENT(OUT)(3). Use the declaration and description for
! this overload.
!
! Returns:
!   Updates inside, pint(3).
!
! Formula:
! $$p=p_0+s d,\quad p\in\mathrm{triangle}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE Triangle_Method, ONLY: triangle_contains_line_exp_3d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: p(3)
! LOGICAL(LGT) :: inside
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! CALL triangle_contains_line_exp_3d(t3, &
!   [0.5_DFP, 0.25_DFP, -1.0_DFP], [0.5_DFP, 0.25_DFP, 1.0_DFP], inside, p)
! CALL OK(inside, "Line crosses triangle interior")
! CALL OK(ABS(p(3)) < 1.0E-11_DFP, "Intersection lies in plane")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE triangle_contains_line_exp_3d(t, p1, p2, &
    & inside, pint)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p1(3)
    REAL(DFP), INTENT(IN) :: p2(3)
    LOGICAL(LGT), INTENT(OUT) :: inside
    REAL(DFP), INTENT(OUT) :: pint(3)
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Finds if a line is inside a triangle in 3D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   p0 : REAL(DFP), INTENT(IN)(3). Use the declaration and description for
! this overload.
!   pd : REAL(DFP), INTENT(IN)(3). Use the declaration and description for
! this overload.
!   p : REAL(DFP), INTENT(OUT)(3). Query point or output point as indicated by
! INTENT.
!   inside : LOGICAL(LGT), INTENT(OUT). Whether the point or line intersection
! is contained.
!
! Returns:
!   Updates p(3), inside.
!
! Formula:
! $$p=p_0+s d,\quad p\in\mathrm{triangle}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE Triangle_Method, ONLY: triangle_contains_line_par_3d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: p(3)
! LOGICAL(LGT) :: inside
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! CALL triangle_contains_line_par_3d(t3, &
!   [0.5_DFP, 0.25_DFP, -1.0_DFP], [0.0_DFP, 0.0_DFP, 1.0_DFP], inside, p)
! CALL OK(inside, "Line crosses triangle interior")
! CALL OK(ABS(p(3)) < 1.0E-11_DFP, "Intersection lies in plane")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE triangle_contains_line_par_3d(t, p0, pd, inside, p)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p0(3)
    REAL(DFP), INTENT(IN) :: pd(3)
    REAL(DFP), INTENT(OUT) :: p(3)
    LOGICAL(LGT), INTENT(OUT) :: inside
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Finds if a point is inside the triangle in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle. Variant 1
! requires counterclockwise vertex ordering; the example uses that ordering.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   p : REAL(DFP), INTENT(IN)(2). Query point or output point as indicated by
! INTENT.
!
! Returns:
!   inside : LOGICAL(LGT).
!
! Formula:
! $$p\in T\iff\lambda_i(p)\ge0\quad(i=1,2,3)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE Triangle_Method, ONLY: triangle_contains_point_2d_1
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! LOGICAL(LGT) :: inside
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! inside = triangle_contains_point_2d_1(t2, [0.5_DFP, 0.25_DFP])
! CALL OK(inside, "Interior point is contained")
! CALL OK(.NOT. triangle_contains_point_2d_1(t2, [3.0_DFP, 2.0_DFP]), &
!   "Exterior point is rejected")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_contains_point_2d_1(t, p) RESULT(inside)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(2)
    LOGICAL(LGT) :: inside
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Finds if a poiint is inside a triangle in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle. Variant 1
! requires counterclockwise vertex ordering; the example uses that ordering.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   p : REAL(DFP), INTENT(IN)(2). Query point or output point as indicated by
! INTENT.
!
! Returns:
!   inside : LOGICAL(LGT).
!
! Formula:
! $$p\in T\iff\lambda_i(p)\ge0\quad(i=1,2,3)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE Triangle_Method, ONLY: triangle_contains_point_2d_2
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! LOGICAL(LGT) :: inside
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! inside = triangle_contains_point_2d_2(t2, [0.5_DFP, 0.25_DFP])
! CALL OK(inside, "Interior point is contained")
! CALL OK(.NOT. triangle_contains_point_2d_2(t2, [3.0_DFP, 2.0_DFP]), &
!   "Exterior point is rejected")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_contains_point_2d_2(t, p) RESULT(inside)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(2)
    LOGICAL(LGT) :: inside
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Returns true if the point is contained inside the triangle.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle. Variant 1
! requires counterclockwise vertex ordering; the example uses that ordering.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   p : REAL(DFP), INTENT(IN)(2). Query point or output point as indicated by
! INTENT.
!
! Returns:
!   inside : LOGICAL(LGT).
!
! Formula:
! $$p\in T\iff\lambda_i(p)\ge0\quad(i=1,2,3)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE Triangle_Method, ONLY: triangle_contains_point_2d_3
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! LOGICAL(LGT) :: inside
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! inside = triangle_contains_point_2d_3(t2, [0.5_DFP, 0.25_DFP])
! CALL OK(inside, "Interior point is contained")
! CALL OK(.NOT. triangle_contains_point_2d_3(t2, [3.0_DFP, 2.0_DFP]), &
!   "Exterior point is rejected")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_contains_point_2d_3(t, p) RESULT(inside)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(2)
    LOGICAL(LGT) :: inside
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Compute diameter 2d.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle. The
! implementation squares sums of coordinate differences instead of summing
! their squares. Its result here is 2 rather than the geometric diameter
! sqrt(5). This example records current behavior, not geometric correctness.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   diameter : REAL(DFP).
!
! Formula:
! $$D_{\mathrm{intended}}=\text{diameter of the smallest enclosing circle}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_diameter_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_diameter_2d(t2)
! CALL OK(ABS(y-2.0_DFP) < 1.0E-11_DFP, &
!   "Current implementation result for this triangle")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_diameter_2d(t) RESULT(diameter)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: diameter
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Returns edge lengths of a triangle in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   edge_length : REAL(DFP)(3).
!
! Formula:
! $$\ell_i=\|v_{i+1}-v_i\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_edge_length_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_edge_length_2d(t2)
! CALL OK(ABS(SUM(y)-(3.0_DFP+SQRT(5.0_DFP))) &
!   < 1.0E-11_DFP, "Perimeter from edge lengths")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_edge_length_2d(t) RESULT(edge_length)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: edge_length(3)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         Returns grid points within a triangle in 2D
!
!# Introduction
!
!    The gridpoints are computed by repeated halving of the triangle.
!    The 0-th set of grid points is the vertices themselves.
!    The first set of grid points is the midpoints of the sides.
!    These points can be used to draw 4 triangles that make up the original
!    triangle.  The second set of grid points is the side midpoints and
!     centers
!    of these four triangles.
!
!     SUB_NUM                     GRID_NUM
!    -----                        -----
!        0      1                  =  1  (centroid)
!        1      1 + 2              =  3  (vertices)
!        2      1 + 2 + 3          =  6
!        3      1 + 2 + 3 + 4      = 10
!        4      1 + 2 + 3 + 4 + 5  = 15
!
!    GRID_NUM is the sum of the integers from 1 to SUB_NUM+1 or
!
!      GRID_NUM = (SUB_NUM+1) * (SUB_NUM+2) / 2
!
!# Parameters:
!
!- Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!- Input, integer ( kind = 4 ) SUB_NUM, the number of subdivisions.
!- Input, integer ( kind = 4 ) GRID_MAX, the maximum number of grid points.
!- Output, integer ( kind = 4 ) GRID_NUM, the number of grid points
! returned.
!- Output, REAL ( kind = 8 ) G(2,GRID_MAX), the grid points.
!

INTERFACE
  MODULE PURE SUBROUTINE triangle_gridpoints_2d(t, sub_num, grid_max, &
      & grid_num, g)
    REAL(DFP), INTENT(IN) :: t(:, :)
    INTEGER(I4B), INTENT(IN) :: sub_num
    INTEGER(I4B), INTENT(IN) :: grid_max
    INTEGER(I4B), INTENT(OUT) :: grid_num
    REAL(DFP), INTENT(OUT) :: g(2, grid_max)
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Computes the incenter of a triangle in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   pc : REAL(DFP)(2).
!
! Formula:
! $$r=2A/(a+b+c),\quad c=(a v_1+b v_2+c v_3)/(a+b+c)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_incenter_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y(2)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_incenter_2d(t2)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP-SQRT(5.0_DFP))/2.0_DFP)) < 1.0E-11_DFP, &
!   "Incenter coordinates of right triangle")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_incenter_2d(t) RESULT(pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: pc(2)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Computes the inscribed circle of a triangle in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   pc : REAL(DFP), INTENT(OUT)(2). Circle center or orthocenter.
!   r : REAL(DFP), INTENT(OUT). Circle radius.
!
! Returns:
!   Updates pc(2), r.
!
! Formula:
! $$r=2A/(a+b+c),\quad c=(a v_1+b v_2+c v_3)/(a+b+c)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_incircle_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y, center(2)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! CALL triangle_incircle_2d(t2, y, center)
! CALL OK(ABS(y-(3.0_DFP-SQRT(5.0_DFP))/2.0_DFP) < 1.0E-11_DFP, "Inradius")
! CALL OK(MAXVAL(ABS(center-y)) < 1.0E-11_DFP, "Incenter")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE triangle_incircle_2d(t, r, pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(OUT) :: pc(2)
    REAL(DFP), INTENT(OUT) :: r
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Radius of the inscribed circle of a triangle in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   r : REAL(DFP).
!
! Formula:
! $$r=2A/(a+b+c),\quad c=(a v_1+b v_2+c v_3)/(a+b+c)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_inradius_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_inradius_2d(t2)
! CALL OK(ABS(y-(3.0_DFP-SQRT(5.0_DFP))/2.0_DFP) < 1.0E-11_DFP, "Inradius")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_inradius_2d(t) RESULT(r)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: r
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         finds if a triangle is degenerate in ND.
!
!# Introduction
!
!    A triangle in ND is described by the coordinates of its 3 vertices.
!    A triangle in ND is degenerate if any two vertices are equal.
!
!# Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!    Input, REAL ( kind = 8 ) T(DIM_NUM,3), the triangle vertices.
!    Output, logical ( kind = 4 ) TRIANGLE_IS_DEGENERATE_ND, is TRUE if the
!    triangle is degenerate.

INTERFACE
  MODULE PURE FUNCTION triangle_is_degenerate_nd(dim_num, t) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim_num
    REAL(DFP), INTENT(IN) :: t(dim_num, 3)
    LOGICAL(LGT) :: ans
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: next triangle lattice layer point.
!
!# Introduction
!
!    The triangle lattice layer L is bounded by the lines
!
!      0 <= X,
!      0 <= Y,
!      L - 1 < X / C(1) + Y / C(2) <= L.
!
!    In particular, layer L = 0 always contains the single point (0,0).
!
!    This FUNCTION returns, one at a time, the points that lie within
!    a given triangle lattice layer.
!
!    Thus, if we set C(1) = 2, C(2) = 3, then we get the following layers:
!
!    L = 0: (0,0)
!    L = 1: (1,0), (2,0), (0,1), (1,1), (0,2), (0,3)
!    L = 2: (3,0), (4,0), (2,1), (3,1), (1,2), (2,2), (1,3), (2,3),
!           (0,4), (1,4), (0,5), (0,6).
!
!# Parameters:
!
!- Input, integer ( kind = 4 ) C(3), coefficients defining the
! lattice layer.  Entry C(3) contains the layer index. C(1) and C(2) should
! be positive, and C(3) must be nonnegative.
!- Input/output, integer ( kind = 4 ) V(2).  On first call for a given layer,
!    the input value of V is not important.  On a repeated call for the same
!    layer, the input value of V should be the output value from the previous
!    call.  On output, V contains the next lattice layer point.
!- Input/output, logical ( kind = 4 ) MORE.  On input, set MORE to FALSE to
!    indicate that this is the first call for a given layer.  Thereafter, the
!    input value should be the output value from the previous call.
! On output, MORE is TRUE if the returned value V is a new point.
!    If the output value is FALSE, then no more points were found,
!    and V was reset to 0, and the lattice layer has been exhausted.

INTERFACE
  MODULE PURE SUBROUTINE triangle_lattice_layer_point_next(c, v, more)
    INTEGER(I4B), INTENT(IN) :: c(3)
    INTEGER(I4B), INTENT(INOUT) :: v(2)
    LOGICAL(LGT), INTENT(INOUT) :: more
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         returns the next triangle lattice point
!
!# Introduction
!
!    The lattice triangle is defined by the vertices:
!
!      (0,0), (C(3)/C(1), 0) and (0,C(3)/C(2))
!
!    The lattice triangle is bounded by the lines
!
!      0 <= X,
!      0 <= Y
!      X / C(1) + Y / C(2) <= C(3)
!
!    Lattice points are listed one at a time, starting at the origin,
!    with X increasing first.
!
!# Parameters:
!
!    Input, integer ( kind = 4 ) C(3), coefficients defining the
!    lattice triangle.  These should be positive.
!
!    Input/output, integer ( kind = 4 ) V(2).  On first call, the input
!    value is not important.  On a repeated call, the input value should
!    be the output value from the previous call.  On output, V contains
!    the next lattice point.
!
!    Input/output, logical ( kind = 4 ) MORE.  On input, set MORE to FALSE to
!    indicate that this is the first call for a given triangle.  Thereafter,
!    the input value should be the output value from the previous call.  On
!    output, MORE is TRUE if the returned value V is a new lattice point.
!    If the output value is FALSE, then no more lattice points were found,
!    and V was reset to 0, and the routine should not be called further
!    for this triangle.

INTERFACE
  MODULE PURE SUBROUTINE triangle_lattice_point_next(c, v, more)
    INTEGER(I4B), INTENT(IN) :: c(3)
    INTEGER(I4B), INTENT(INOUT) :: v(2)
    LOGICAL(LGT), INTENT(INOUT) :: more
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         implicit line intersects a triangle in 2D.
!
!# Introduction
!
!    An implicit line is the set of points ( X, Y ) satisfying
!
!      A * X + B * Y + C = 0
!
!    where at least one of A and B is not zero.
!
!# Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, REAL ( kind = 8 ) A, B, C, determine the equation of the line:
!    A*X + B*Y + C = 0.
!
!    Output, integer ( kind = 4 ) INT_NUM, the number of points of
! intersection
!    of the line with the triangle.  INT_NUM may be 0, 1, 2 or 3.
!
!    Output, REAL ( kind = 8 ) PINT(2,3), contains the intersection points.

INTERFACE
  MODULE PURE SUBROUTINE triangle_line_imp_int_2d(t, a, b, c, int_num, pint)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: a, b, c
    INTEGER(I4B), INTENT(OUT) :: int_num
    INTEGER(I4B), INTENT(OUT) :: pint(2, 3)
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         determines the orientation of a triangle in 2D.
!
!# Introduction
!
!    Three distinct non-colinear points in the plane define a circle.
!    If the points are visited in the order P1, P2, and then
!    P3, this motion defines a clockwise or counter clockwise
!    rotation along the circle.
!
!# Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, integer ( kind = 4 ) TRIANGLE_ORIENTATION_2D, reports if the
!    three points lie clockwise on the circle that passes through them.
!    The possible return values are:
!    0, the points are distinct, noncolinear, and lie counter clockwise
!    on their circle.
!    1, the points are distinct, noncolinear, and lie clockwise
!    on their circle.
!    2, the points are distinct and colinear.
!    3, at least two of the points are identical.

INTERFACE
  MODULE PURE FUNCTION triangle_orientation_2d(t) RESULT(ans)
    REAL(DFP), INTENT(IN) :: t(:, :)
    INTEGER(I4B) :: ans
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Computes the orthocenter of a triangle in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   pc : REAL(DFP)(2).
!
! Formula:
! $$h=c_{\mathrm{centroid}}\,3-2c_{\mathrm{circumcenter}}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_orthocenter_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y(2)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_orthocenter_2d(t2)
! CALL OK(MAXVAL(ABS(y)) < 1.0E-11_DFP, &
!   "Altitudes of right triangle meet at the origin")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_orthocenter_2d(t) RESULT(pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: pc(2)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Distance ( triangle, point ) in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle. Distance
! is to the boundary edges, including for interior points; signed 2D distance
! is negative inside a counterclockwise triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   p : REAL(DFP), INTENT(IN)(:). Query point or output point as indicated by
! INTENT.
!
! Returns:
!   dist : REAL(DFP).
!
! Formula:
! $$d=\min_{q\in\partial T}\|p-q\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_point_dist_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_point_dist_2d(t2, [-1.0_DFP, 0.0_DFP])
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, &
!   "Distance of an exterior point from the boundary")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_point_dist_2d(t, p) RESULT(dist)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(:)
    REAL(DFP) :: dist
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Distance ( triangle, point ) in 3D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle. Distance
! is to the boundary edges, including for interior points; signed 2D distance
! is negative inside a counterclockwise triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   p : REAL(DFP), INTENT(IN)(:). Query point or output point as indicated by
! INTENT.
!
! Returns:
!   dist : REAL(DFP).
!
! Formula:
! $$d=\min_{q\in\partial T}\|p-q\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_point_dist_3d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_point_dist_3d(t3, [-1.0_DFP, 0.0_DFP, 0.0_DFP])
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, &
!   "Distance of an exterior point from the boundary")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_point_dist_3d(t, p) RESULT(dist)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(:)
    REAL(DFP) :: dist
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Signed distance in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle. Distance
! is to the boundary edges, including for interior points; signed 2D distance
! is negative inside a counterclockwise triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   p : REAL(DFP), INTENT(IN)(2). Query point or output point as indicated by
! INTENT.
!
! Returns:
!   dist_signed : REAL(DFP).
!
! Formula:
! $$d=\min_{q\in\partial T}\|p-q\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_point_dist_signed_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_point_dist_signed_2d(t2, [-1.0_DFP, 0.0_DFP])
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, &
!   "Distance of an exterior point from the boundary")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_point_dist_signed_2d(t, p) RESULT(dist_signed)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(2)
    REAL(DFP) :: dist_signed
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Computes the nearest point on a triangle in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   p : REAL(DFP), INTENT(IN)(2). Query point or output point as indicated by
! INTENT.
!   pn : REAL(DFP), INTENT(OUT)(2). Nearest point on the segment.
!   dist : REAL(DFP), INTENT(OUT). Distance from the query point to the
! segment or triangle boundary.
!
! Returns:
!   Updates pn(2), dist.
!
! Formula:
! $$q=\mathop{\mathrm{argmin}}_{q\in\partial T}\|p-q\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_point_near_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y, nearest(2)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! CALL triangle_point_near_2d(t2, [-1.0_DFP, 0.0_DFP], nearest, y)
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, "Boundary distance")
! CALL OK(MAXVAL(ABS(nearest)) < 1.0E-11_DFP, "Nearest vertex")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE triangle_point_near_2d(t, p, pn, dist)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(2)
    REAL(DFP), INTENT(OUT) :: pn(2)
    REAL(DFP), INTENT(OUT) :: dist
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: "quality" of a triangle in 2D.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!
! Returns:
!   quality : REAL(DFP).
!
! Formula:
! $$Q=2r/R$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_quality_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_quality_2d(t2)
! CALL OK(y > 0.0_DFP .AND. y <= 1.0_DFP, &
!   "Nondegenerate triangle quality is in (0,1]")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_quality_2d(t) RESULT(quality)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: quality
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: count lattice points.
!
!# Introduction
!
!    The triangle is assumed to be a right triangle which, without loss
!    of generality, has the coordinates:
!
!    ( (0,0), (a,0), (0,b) )
!
!    The routine returns the number of integer lattice points that appear
!    inside the triangle or on its edges or vertices.
!
!    The formula for this FUNCTION occurred to me (JVB) after some thought,
!    on 06 July 2009.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, define the vertices.
!
!    Output, integer ( kind = 4 ) N, the number of lattice points.

INTERFACE
  MODULE PURE FUNCTION triangle_right_lattice_point_num_2d(a, b) RESULT(n)
    INTEGER(I4B), INTENT(IN) :: a
    INTEGER(I4B), INTENT(IN) :: b
    INTEGER(I4B) :: n
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Returns random points in a triangle.
!
! Vertex coordinates are columns of t. Use a nondegenerate triangle. The
! public seed argument is INTENT(IN), so a caller cannot advance it through
! this interface.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(:, :). Triangle vertex coordinates, one vertex
! per column.
!   n : INTEGER(I4B), INTENT(IN). Number of samples, coordinate dimension or
! grid count as specified by the routine.
!   seed : INTEGER(I4B), INTENT(IN). Random generator seed; this interface
! does not return an advanced seed.
!
! Returns:
!   p : REAL(DFP)(2, n).
!
! Formula:
! $$p=\lambda_1v_1+\lambda_2v_2+\lambda_3v_3,\quad\lambda_i\ge0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Triangle_Method, ONLY: triangle_sample
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y(2, 8)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! y = triangle_sample(t2, n=8_I4B, seed=12345_I4B)
! CALL OK(MINVAL(y) >= 0.0_DFP, "Samples have nonnegative coordinates")
! CALL OK(MAXVAL(y(1, :)/2.0_DFP+y(2, :)) <= 1.0_DFP, &
!   "Samples lie inside the right triangle")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION triangle_sample(t, n, seed) RESULT(p)
    REAL(DFP), INTENT(IN) :: t(:, :)
    INTEGER(I4B), INTENT(IN) :: n
    INTEGER(I4B), INTENT(IN) :: seed
    REAL(DFP) :: p(2, n)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:          count lattice points.
!
!# Introduction
!
!    The triangle is assumed to be the unit triangle:
!
!$$
!    ( (0,0), (1,0), (0,1) )
!$$
!
!    or a copy of this triangle scaled by an integer S:
!
!$$
!    ( (0,0), (S,0), (0,S) ).
!$$
!
!    The routine returns the number of integer lattice points that appear
!    inside the triangle or on its edges or vertices.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) S, the scale factor.
!
!    Output, integer ( kind = 4 ) N, the number of lattice points.
!

INTERFACE
  MODULE PURE FUNCTION triangle01_lattice_point_num_2d(s) RESULT(n)
    INTEGER(I4B), INTENT(IN) :: s
    INTEGER(I4B) :: n
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         converts from barycentric to XY coordinates in 2D.
!
!# Introduction
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, REAL ( kind = 8 ) XSI(3), the barycentric coordinates of a point.
!    XSI(1) + XSI(2) + XSI(3) should equal 1, but this is not checked.
!
!    Output, REAL ( kind = 8 ) P(2), the XY coordinates of the point.
!

INTERFACE
  MODULE PURE FUNCTION triangle_xsi_to_xy_2d(t, xsi) RESULT(p)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: xsi(3)
    REAL(DFP) :: p(2)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:          converts from XY to barycentric in 2D.
!
!# Introduction
!
!# Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, REAL ( kind = 8 ) P(2), the XY coordinates of a point.
!
!    Output, REAL ( kind = 8 ) XSI(3), the barycentric coordinates of the
! point.
!    XSI1 + XSI2 + XSI3 should equal 1.
!

INTERFACE
  MODULE PURE FUNCTION triangle_xy_to_xsi_2d(t, p) RESULT(xsi)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(2)
    REAL(DFP) :: xsi(3)
  END FUNCTION
END INTERFACE

END MODULE Triangle_Method
