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

!> authors: Vikas Sharma, Ph. D.
! date: 	5 March 2021
! summary: This module contains methods for [[ReferenceQuadrangle_]]

MODULE ReferenceQuadrangle_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Quadrangle
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns lagrange Quadrangle element of higher order
!
!### Introduction
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
MODULE PURE SUBROUTINE initiate_ref_Quadrangle( obj, NSD, XiJ )
  CLASS( ReferenceQuadrangle_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Quadrangle
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Quadrangle
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                            ReferenceQuadrangle@Quadrangle
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns lagrange Quadrangle element of higher order
!
!### Introduction
! 	This routine retuns the lagrance element of higher order
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
MODULE PURE FUNCTION reference_Quadrangle( NSD, XiJ ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceQuadrangle_ ) :: obj
END FUNCTION reference_Quadrangle
END INTERFACE

INTERFACE ReferenceQuadrangle
  MODULE PROCEDURE reference_Quadrangle
END INTERFACE ReferenceQuadrangle

PUBLIC :: ReferenceQuadrangle

!----------------------------------------------------------------------------
!                                     ReferenceQuadrangle_Pointer@Quadrangle
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns lagrange Quadrangle element of higher order
!
!### Introduction
! 	This routine retuns the lagrance element of higher order
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
MODULE PURE FUNCTION reference_Quadrangle_Pointer( NSD, XiJ ) RESULT( obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceQuadrangle_ ), POINTER :: obj
END FUNCTION reference_Quadrangle_Pointer
END INTERFACE

INTERFACE ReferenceQuadrangle_Pointer
  MODULE PROCEDURE reference_Quadrangle_Pointer
END INTERFACE ReferenceQuadrangle_Pointer

PUBLIC :: ReferenceQuadrangle_Pointer

!----------------------------------------------------------------------------
!                                                LagrangeElement@Quadrangle
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: 	Higher order lagrange elements
!
!### Usage
!
!```fortran
! subroutine test4
!   class( ReferenceElement_ ), pointer :: obj_ptr => null()
!   type( ReferenceQuadrangle_ ) :: obj
!   obj_ptr => referenceQuadrangle_pointer( nsd = 2 )
!   call obj_ptr%LagrangeElement( order = 2, HighOrderobj = obj )
!   call display( obj, "higher order obj : ")
!   call obj_ptr%LagrangeElement( order = 3, HighOrderobj = obj )
!   call display( obj, "3rd order obj : ")
! end
!```

INTERFACE
MODULE PURE SUBROUTINE LagrangeElement_Quadrangle( RefElem, Order, obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  CLASS( ReferenceElement_ ), INTENT( INOUT ) :: obj
END SUBROUTINE LagrangeElement_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Quadrangle( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceQuadrangle_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Quadrangle
END INTERFACE

PUBLIC :: Measure_Simplex_Quadrangle


!----------------------------------------------------------------------------
!                                                         Quadrangle_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Quadrangle_Quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceQuadrangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Quadrangle_Quality
END INTERFACE

PUBLIC :: Quadrangle_Quality

END MODULE ReferenceQuadrangle_Method