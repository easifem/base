! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
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
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary: This module contains methods for [[ReferenceQuadrangle_]]

MODULE ReferenceQuadrangle_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns lagrange Quadrangle element of higher order
!
!# Introduction
!
! * This routine retuns the lagrance element of higher order
! * This routine will be called by [[ReferenceQuadrangle_:LagrangeElement]]
! * Currently upto 3rd order Quadrangle elements are supported.
!
!### Usage
!
!```fortran
! subroutine test1
!   type( ReferenceQuadrangle_ ) :: obj
!   call initiate( obj, nsd = 2 )
!   ! call initiate( obj, nsd = 2, xij = xij )
!   call display( obj, "obj : " )
! end
!```

INTERFACE
  MODULE SUBROUTINE initiate_ref_Quadrangle(obj, NSD, xij)
    CLASS(ReferenceQuadrangle_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
  END SUBROUTINE initiate_ref_Quadrangle
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Quadrangle
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                            ReferenceQuadrangle@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns lagrange Quadrangle element of higher order
!
!# Introduction
!         This routine retuns the lagrance element of higher order
! This routine will be called by [[ReferenceQuadrangle_:LagrangeElement]]
! Currently upto 3rd order Quadrangle elements are supported.
!
!### Usage
!
!```fortran
! subroutine test2
!   type( ReferenceQuadrangle_ ) :: obj
!   obj = referenceQuadrangle( nsd = 2 )
!   call display( obj, "obj : " )
! end
!```

INTERFACE
  MODULE FUNCTION reference_Quadrangle(NSD, xij) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    TYPE(ReferenceQuadrangle_) :: obj
  END FUNCTION reference_Quadrangle
END INTERFACE

INTERFACE ReferenceQuadrangle
  MODULE PROCEDURE reference_Quadrangle
END INTERFACE ReferenceQuadrangle

PUBLIC :: ReferenceQuadrangle

!----------------------------------------------------------------------------
!                                     ReferenceQuadrangle_Pointer@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns lagrange Quadrangle element of higher order
!
!# Introduction
!         This routine retuns the lagrance element of higher order
! This routine will be called by [[ReferenceQuadrangle_:LagrangeElement]]
! Currently upto 3rd order Quadrangle elements are supported.
!
!### Usage
!
!```fortran
! subroutine test3
!   class( ReferenceElement_ ), pointer :: obj => null()
!   obj => referenceQuadrangle_pointer( nsd = 2 )
!   call display( obj, "obj : " )
! end
!```

INTERFACE
  MODULE FUNCTION reference_Quadrangle_Pointer(NSD, xij) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CLASS(ReferenceQuadrangle_), POINTER :: obj
  END FUNCTION reference_Quadrangle_Pointer
END INTERFACE

INTERFACE ReferenceQuadrangle_Pointer
  MODULE PROCEDURE reference_Quadrangle_Pointer
END INTERFACE ReferenceQuadrangle_Pointer

PUBLIC :: ReferenceQuadrangle_Pointer

!----------------------------------------------------------------------------
!                                                LagrangeElement@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary:         Higher order lagrange elements
!
!### Usage
!
!```fortran
! subroutine test4
!   class( ReferenceElement_ ), pointer :: obj_ptr => null()
!   type( ReferenceQuadrangle_ ) :: obj
!   obj_ptr => referenceQuadrangle_pointer( nsd = 2 )
!   call obj_ptr%LagrangeElement( order = 2, Highorderobj = obj )
!   call display( obj, "higher order obj : ")
!   call obj_ptr%LagrangeElement( order = 3, Highorderobj = obj )
!   call display( obj, "3rd order obj : ")
! end
!```

INTERFACE
  MODULE SUBROUTINE highorderElement_Quadrangle(refelem, order, obj, &
    & ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), INTENT(IN) :: order
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE highorderElement_Quadrangle
END INTERFACE

PUBLIC :: highorderElement_Quadrangle

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Quadrangle(refelem, xij) RESULT(Ans)
    CLASS(ReferenceQuadrangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Quadrangle
END INTERFACE

PUBLIC :: Measure_Simplex_Quadrangle

!----------------------------------------------------------------------------
!                                                         Quadrangle_quality
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION Quadrangle_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceQuadrangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Quadrangle_Quality
END INTERFACE

PUBLIC :: Quadrangle_Quality

!-----------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Aug 2022
! summary:         Area of quadrangle in 3D
!
!# Introduction
!
!- QUADAREA3D computes the area of a quadrilateral in 3D.
!- A quadrilateral is a polygon defined by 4 vertices.
! It is assumed that the four vertices of the quadrilateral
! are coplanar.
!- This algorithm computes the area of the related Varignon parallelogram
! first.

INTERFACE
  MODULE PURE SUBROUTINE QuadArea3D(q, area)
    REAL(DFP), INTENT(IN) :: q(3, 4)
    REAL(DFP), INTENT(OUT) :: area
  END SUBROUTINE QuadArea3D
END INTERFACE

PUBLIC :: QuadArea3D

!-----------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Aug 2022
! summary:         QuadArea2D
!
!# Introduction
!
!- QUADAREA2D computes the area of a quadrilateral in 2D.
!- A quadrilateral is a polygon defined by 4 vertices.
! This algorithm should be able to handle nonconvex quadrilaterals.
! The vertices of the quadrilateral should be listed in counter clockwise
! order, so that the area is positive.

INTERFACE
  MODULE PURE SUBROUTINE QuadArea2D(q, area)
    REAL(DFP), INTENT(IN) :: q(2, 4)
    REAL(DFP), INTENT(OUT) :: area
  END SUBROUTINE QuadArea2D
END INTERFACE

PUBLIC :: QuadArea2D

END MODULE ReferenceQuadrangle_Method
