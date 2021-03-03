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
! date: 1 March 2021
! summary: This submodule contains method for [[ReferenceElement_]]

MODULE ReferenceElement_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE display_ref_elem( Obj, msg, unitno )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitno
END SUBROUTINE display_ref_elem
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE display_ref_topo( Obj, msg, unitno )
  CLASS( ReferenceTopology_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitno
END SUBROUTINE display_ref_topo
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Display@Constructor
!----------------------------------------------------------------------------

INTERFACE Display
  MODULE PROCEDURE display_ref_elem, display_ref_topo
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                              ReferenceTopology@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: 	This function returns the instance of [[ReferenceTopology_]]
!
!### Introduction
! 	This function returns the instance of [[ReferenceTopology_]].
! The possible valaues of Name can be
!
! * `Line, Line2, Line3, Line4, Line5, Line6`
! * `Triangle, Triangle3, Triangle6, Triangle9, Triangle10, Triangle12, Triangl15a, Triangl15b, Triangl15, Triangl21`
! * `Quadrangle, Quadrangle4, Quadrangle9, Quadrangle8`
! * `Tetrahedron, Tetrahedron4, Tetrahedron10, Tetrahedron20, Tetrahedron35, Tetrahedron56`
! * `Hexahedron, Hexahedron8, Hexahedron27, Hexahedron20, Hexahedron64, Hexahedron125`
! * `Prism, Prism6, Prism15, Prism18`
! * `Pyramid, Pyramid5, Pyramid14, Pyramid13`
! * `Point, Point1`
!
!### Usage
!
!```fortran
! type( ReferenceTopology_ ) :: obj
! obj = ReferenceTopology( Nptrs = [1,2,3], Name=Triangle3 )
! call display( obj, "obj=")
!```

INTERFACE
MODULE PURE FUNCTION reference_topology( Nptrs, Name ) RESULT( Obj )
  TYPE( ReferenceTopology_ ) :: Obj
  INTEGER( I4B), INTENT( IN ) :: Nptrs( : )
  INTEGER( I4B), INTENT( IN ) :: Name
END FUNCTION reference_topology
END INTERFACE

INTERFACE ReferenceTopology
  MODULE PROCEDURE reference_topology
END INTERFACE ReferenceTopology

PUBLIC :: ReferenceTopology

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: 	This subroutine reset the instance of [[ReferenceTopology_]]
!
!### Usage
!
!```fortran
! type( ReferenceTopology_ ) :: obj
! obj = ReferenceTopology( Nptrs = [1,2,3], Name=Triangle3 )
! call display( obj, "obj=")
! call deallocatedata( obj ) !<------
!```

INTERFACE
MODULE PURE SUBROUTINE deallocatedata_ref_topology( Obj )
  CLASS( ReferenceTopology_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE deallocatedata_ref_topology
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE deallocatedata_ref_topology
END INTERFACE

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                           NNE@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: 	This function returns the totat nodes inside the referenc topology
!
!### Usage
!
!```fortran
! type( ReferenceTopology_ ) :: obj
! obj = ReferenceTopology( Nptrs = [1,2,3], Name=Triangle3 )
! call display( obj, "obj=")
! call display( .NNE. obj, "nne =")
!```

INTERFACE
MODULE PURE FUNCTION tNodes_RefTopo( Obj ) RESULT( Ans )
  CLASS( ReferenceTopology_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION tNodes_RefTopo
END INTERFACE

!----------------------------------------------------------------------------
!                                                           NNE@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: 	This function returns the total number of nodes in the reference element
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION tNodes_RefElem( Obj ) RESULT( Ans )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION tNodes_RefElem
END INTERFACE

!----------------------------------------------------------------------------
!                                                            NNE@Constructor
!----------------------------------------------------------------------------

INTERFACE OPERATOR( .NNE. )
  MODULE PROCEDURE tNodes_RefTopo, tNodes_RefElem
END INTERFACE

PUBLIC :: OPERATOR( .NNE. )

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: 	This routine deallocates the data stored inside the [[ReferenceElement_]]
!
!### Usage
!
!```fortran
!	todo
!```

INTERFACE
MODULE PURE SUBROUTINE deallocatedata_ref_elem( Obj )
  CLASS( ReferenceElement_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE deallocatedata_ref_elem
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE deallocatedata_ref_elem
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	2 March 2021
! summary: 	This subroutine copies one reference element into other
!
!### Introduction
!
! This subroutine copies one reference element into other
! This subroutine also defines an assignment operator for `Obj1=Obj2` type opertions
!
!### Usage
!
!```fortran
!	todo
!```

INTERFACE
MODULE PURE SUBROUTINE init_refelem( Obj, AnotherObj )
  CLASS( ReferenceElement_ ), INTENT( INOUT ) :: Obj
  CLASS( ReferenceElement_ ), INTENT( IN ) :: AnotherObj
END SUBROUTINE init_refelem
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE init_refelem
END INTERFACE Initiate

PUBLIC :: Initiate

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE init_refelem
END INTERFACE

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                            Initiate@Line
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: This subroutine constructs an instance of line reference element
!
!### Introduction
! This routine constructs an instance of [[ReferenceLine_]] element of order equal to 1.
!
! - `XiJ` denotes the nodal coordinate, if it is not present than RESHAPE( [-1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 2] ) is used.
!
!@note
! 	Note that SIZE(XiJ,1) should be equal to 3, i.e., x,y,z coord. Also note that this routine creats a linear element.
!@endnote
!
!### Usage
!
!```fortran
! type( ReferenceLine_ ) :: obj1
! real( dfp ) :: xij( 3, 2 )
! call random_number( xij )
! call initiate( Obj=obj1, NSD=3, xij )
! call display( obj1, "obj1 : " )
!```

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Line( Obj, NSD, XiJ )
  CLASS( ReferenceLine_ ), INTENT( INOUT ) :: Obj
    !! The instance
  INTEGER( I4B ), INTENT( IN ) :: NSD
    !! Spatial dimension of the problem
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
    !! Coords of element
END SUBROUTINE initiate_ref_Line
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Line
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       ReferenceLine@Line
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: This routine constructs an instance of line reference element
!
!### Introduction
! This routine constructs an instance of [[ReferenceLine_]] element of order equal to 1.
!
! - `XiJ` denotes the nodal coordinate, if it is not present than RESHAPE( [-1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 2] ) is used.
!
!@note
! 	Note that SIZE(XiJ,1) should be equal to 3, i.e., x,y,z coord. Also note that this routine creats a linear element.
!@endnote
!
!### Usage
!
!```fortran
! type( ReferenceLine_ ) :: obj
! obj = ReferenceLine(nsd=3)
! call display( obj, 'obj : ' )
!```

INTERFACE
MODULE PURE FUNCTION reference_line(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceLine_ ) :: Obj
END FUNCTION reference_line
END INTERFACE

INTERFACE ReferenceLine
  MODULE PROCEDURE reference_line
END INTERFACE ReferenceLine

PUBLIC :: ReferenceLine

!----------------------------------------------------------------------------
!                                                ReferenceLine_Pointer@Line
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: This routine constructs an instance of line reference element
!
!### Introduction
! This routine constructs an instance of [[ReferenceLine_]] element of order equal to 1.
!
! - `XiJ` denotes the nodal coordinate, if it is not present than RESHAPE( [-1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 2] ) is used.
!
!@note
! 	Note that SIZE(XiJ,1) should be equal to 3, i.e., x,y,z coord. Also note that this routine creats a linear element.
!@endnote
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: obj => NULL()
! obj => ReferenceLine_Pointer( nsd = 3 )
! call display( obj, "obj : ")
!```

INTERFACE
MODULE PURE FUNCTION reference_line_pointer_1(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceLine_ ), POINTER :: Obj
END FUNCTION reference_line_pointer_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ReferenceLine_Pointer@Line
!----------------------------------------------------------------------------

INTERFACE ReferenceLine_Pointer
  MODULE PROCEDURE reference_line_Pointer_1
END INTERFACE ReferenceLine_Pointer

PUBLIC :: ReferenceLine_Pointer

!----------------------------------------------------------------------------
!                                                       LagrangeElement@Line
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This function returns lagrange element on line
!
!### Introduction
! Returns lagrange line element of higher order. By lagrange element we means
! standard finite elements, with equi-distance lagrange interpolation points.
!
!### Usage
!
!```fortran
! type( ReferenceLine_ ) :: obj1, obj3
! real( dfp ) :: xij( 3, 2 )
! call random_number( xij )
! call initiate( Obj=obj1, NSD=3, XiJ=xij )
! call display( obj1, "obj1 : " )
! call obj1%LagrangeElement( Order=2, HighOrderObj=obj3 ) <---
! call display( obj3, "Second Order Lagrange Element : ")
!```

INTERFACE
MODULE PURE SUBROUTINE LagrangeElement_Line( RefElem, Order, Obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
    !! Linear line element
  INTEGER( I4B ), INTENT( IN ) :: Order
    !! Order or generated element
  CLASS( ReferenceElement_ ),  INTENT( INOUT ) ::  Obj
    !! High order lagrange line element
END SUBROUTINE LagrangeElement_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                      MeasureSimplex@Line
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: 	This function returns the measure of linear line element
!
!### Introduction
!
! This function returns the measure of linear line element. Its generic form is given by [[ReferenceElement_Method:MeasureSimplex]]
!
!
!### Usage
!
!```fortran
! type( ReferenceLine_ ) :: obj
! real( dfp ) :: xij( 3, 2 )
! call random_number( xij )
! call initiate( Obj=obj, NSD=3, XiJ=xij )
! call display( MeasureSimplex(obj, obj%xij), "Measure :: ")
!```

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Line( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceLine_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                             line_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION line_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceLine_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION line_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Initiate@Triangle
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: This routine constructs an instance of [[ReferenceTriangle_]]
!
!### Introduction
! * This routine contructs an instance of [[ReferenceTriangle_]]
! * User can specify the coordinates of the trinagle
!@note
! 	This routine will contruct a three node triangle. Also, SHAPE(XiJ) = [3,3]
!@endnote
!
!### Usage
!
!```fortran
! subroutine test1
!   type( ReferenceTriangle_ ) :: obj
!   real( dfp ) :: xij( 3, 3 )
!   xij( 1, 1:3 ) = [1.0, 2.0, 1.0]
!   xij( 2, 1:3 ) = [0.0, 0.0, 1.0]
!   xij( 3, : ) = 0.0
!   call initiate( obj, nsd = 2, xij = xij )
!   call display( obj, "obj : " )
! end
!```


INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Triangle( Obj, NSD, XiJ )
  CLASS( ReferenceTriangle_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Triangle
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Triangle
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                 ReferenceTriangle@Triangle
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: This function returns an instance of [[ReferenceTriangle_]]
!
!### Introduction
! * This routine contructs an instance of [[ReferenceTriangle_]]
! * User can specify the coordinates of the trinagle
!@note
! 	This routine will contruct a three node triangle. Also, SHAPE(XiJ) = [3,3]
!@endnote
!
!### Usage
!
!```fortran
! subroutine test2
!   type( ReferenceTriangle_ ) :: obj
!   obj = referenceTriangle( nsd = 2 )
!   call display( obj, "obj : " )
! end
!```

INTERFACE
MODULE PURE FUNCTION reference_Triangle(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceTriangle_ ) :: Obj
END FUNCTION reference_Triangle
END INTERFACE

INTERFACE ReferenceTriangle
  MODULE PROCEDURE reference_Triangle
END INTERFACE ReferenceTriangle

PUBLIC :: ReferenceTriangle

!----------------------------------------------------------------------------
!                                         ReferenceTriangle_Pointer@Triangle
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	3 March 2021
! summary: This function returns an instance of [[ReferenceTriangle_]]
!
!### Introduction
! * This routine contructs an instance of [[ReferenceTriangle_]]
! * User can specify the coordinates of the trinagle
!@note
! 	This routine will contruct a three node triangle. Also, SHAPE(XiJ) = [3,3]
!@endnote
!
!### Usage
!
!```fortran
! subroutine test3
!   class( ReferenceElement_ ), pointer :: obj => null()
!   obj => referenceTriangle_pointer( nsd = 2 )
!   call display( obj, "obj : " )
! end
!```

INTERFACE
MODULE PURE FUNCTION reference_Triangle_pointer(NSD, XiJ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceTriangle_ ), POINTER :: Obj
END FUNCTION reference_Triangle_pointer
END INTERFACE

INTERFACE ReferenceTriangle_Pointer
  MODULE PROCEDURE reference_Triangle_Pointer
END INTERFACE ReferenceTriangle_Pointer

PUBLIC :: ReferenceTriangle_Pointer

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Triangle
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns lagrange Triangle element of higher order
!
!### Introduction
! 	This routine retuns the lagrance element of higher order
! This routine will be called by [[ReferenceTriangle_:LagrangeElement]]
! Currently upto 3rd order triangle elements are supported.
!
!
!### Usage
!
!```fortran
! subroutine test4
!   class( ReferenceElement_ ), pointer :: obj_ptr => null()
!   type( ReferenceTriangle_ ) :: obj
!   obj_ptr => referenceTriangle_pointer( nsd = 2 )
!   call obj_ptr%LagrangeElement( order = 2, HighOrderObj = obj )
!   call display( obj, "higher order obj : ")
!   call obj_ptr%LagrangeElement( order = 3, HighOrderObj = obj )
!   call display( obj, "3rd order obj : ")
! end
!```

INTERFACE
MODULE PURE SUBROUTINE LagrangeElement_Triangle( RefElem, Order, Obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  CLASS( ReferenceElement_ ), INTENT( INOUT) ::  Obj
END SUBROUTINE LagrangeElement_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Triangle( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Angles@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_angles( refelem, xij ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_angles
END INTERFACE

INTERFACE Angles
  MODULE PROCEDURE triangle_angles
END INTERFACE Angles

PUBLIC :: Angles

!----------------------------------------------------------------------------
!                                                             Area@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_area( refelem, xij ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_area
END INTERFACE

INTERFACE Area
  MODULE PROCEDURE triangle_area
END INTERFACE Area

PUBLIC :: Area

!----------------------------------------------------------------------------
!                                                        AreaVector@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_areaVector( refelem, xij ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans( 3 )
END FUNCTION triangle_areaVector
END INTERFACE

INTERFACE AreaVector
  MODULE PROCEDURE triangle_areaVector
END INTERFACE AreaVector

PUBLIC :: AreaVector

!----------------------------------------------------------------------------
!                                                      Barycentric@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_barycentric( refelem, xij, x ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ) :: Ans( 3 )
END FUNCTION triangle_barycentric
END INTERFACE

INTERFACE Barycentric
  MODULE PROCEDURE triangle_barycentric
END INTERFACE Barycentric

PUBLIC :: Barycentric

!----------------------------------------------------------------------------
!                                                          Centroid@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_centroid( refelem, xij ) RESULT( Ans )
  CLASS(ReferenceTriangle_), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_centroid
END INTERFACE

INTERFACE Centroid
  MODULE PROCEDURE triangle_centroid
END INTERFACE Centroid

PUBLIC :: Centroid

!----------------------------------------------------------------------------
!                                                      CircumCenter@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_circumcentre(  refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_circumcentre
END INTERFACE

INTERFACE CircumCenter
  MODULE PROCEDURE triangle_circumcentre
END INTERFACE CircumCenter

PUBLIC :: CircumCenter

!----------------------------------------------------------------------------
!                                                      CircumCircle@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_circumcircle( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(4)
    !! Ans(1) = radius and Ans(2:4) center
END FUNCTION triangle_circumcircle
END INTERFACE

INTERFACE CircumCircle
  MODULE PROCEDURE triangle_circumcircle
END INTERFACE CircumCircle

PUBLIC :: CircumCircle

!----------------------------------------------------------------------------
!                                                      CircumRadius@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_circumradius( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_circumradius
END INTERFACE

INTERFACE CircumRadius
  MODULE PROCEDURE triangle_circumradius
END INTERFACE CircumRadius

PUBLIC :: CircumRadius

!----------------------------------------------------------------------------
!                                                     ContainsLine@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE triangle_contains_line( refelem, xij, x1, x2, &
  & parametricLine, inside, xint )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:), x1(3), x2(3)
  LOGICAL(LGT), INTENT( IN ) :: parametricLine
  LOGICAL(LGT), INTENT (OUT) :: inside
  REAL( DFP ), INTENT( OUT ) :: xint(3)
END SUBROUTINE triangle_contains_line
END INTERFACE

INTERFACE ContainsLine
  MODULE PROCEDURE triangle_contains_line
END INTERFACE ContainsLine

PUBLIC :: ContainsLine

!----------------------------------------------------------------------------
!                                                    ContainsPoint@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_contains_point( refelem, xij, x ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:), x(:)
  LOGICAL(LGT) :: Ans
END FUNCTION triangle_contains_point
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Diameter@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_diameter( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_diameter
END INTERFACE

INTERFACE diameter
  MODULE PROCEDURE triangle_diameter
END INTERFACE diameter

PUBLIC :: diameter

!----------------------------------------------------------------------------
!                                                       EdgeLength@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_edge_length( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_edge_length
END INTERFACE

INTERFACE EdgeLength
  MODULE PROCEDURE triangle_edge_length
END INTERFACE EdgeLength

PUBLIC :: EdgeLength

!----------------------------------------------------------------------------
!                                                         Incenter@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_incenter( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(3)
END FUNCTION triangle_incenter
END INTERFACE

INTERFACE Incenter
  MODULE PROCEDURE triangle_incenter
END INTERFACE Incenter

PUBLIC :: Incenter

!----------------------------------------------------------------------------
!                                                         Incircle@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_incircle( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans(4)
END FUNCTION triangle_incircle
END INTERFACE

INTERFACE Incircle
  MODULE PROCEDURE triangle_incircle
END INTERFACE Incircle

PUBLIC :: Incircle

!----------------------------------------------------------------------------
!                                                         Inradius@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_inradius( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_inradius
END INTERFACE

INTERFACE Inradius
  MODULE PROCEDURE triangle_inradius
END INTERFACE Inradius

PUBLIC :: Inradius

!----------------------------------------------------------------------------
!                                                      Orthocenter@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_orthocenter( refelem, xij ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  REAL( DFP ) :: Ans( 3 )
END FUNCTION triangle_orthocenter
END INTERFACE

INTERFACE Orthocenter
  MODULE PROCEDURE triangle_orthocenter
END INTERFACE Orthocenter

PUBLIC :: Orthocenter

!----------------------------------------------------------------------------
!                                                DistanceFromPoint@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_point_dist( refelem, xij, x ) &
  & RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:), x(:)
  REAL( DFP ) :: Ans
END FUNCTION triangle_point_dist
END INTERFACE

INTERFACE DistanceFromPoint
  MODULE PROCEDURE triangle_point_dist
END INTERFACE DistanceFromPoint

PUBLIC :: DistanceFromPoint

!----------------------------------------------------------------------------
!                                                      NearestPoint@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE triangle_get_nearest_point( refelem, xij, x, xn, dist )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:), x(:)
  REAL( DFP ) , INTENT (INOUT) :: xn(:)
  REAL( DFP ), INTENT (OUT) :: dist
END SUBROUTINE triangle_get_nearest_point
END INTERFACE

INTERFACE NearestPoint
  MODULE PROCEDURE triangle_get_nearest_point
END INTERFACE NearestPoint

PUBLIC :: NearestPoint

!----------------------------------------------------------------------------
!                                                       RandomPoint@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_random_point( refelem, xij, n, seed ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ) :: n, seed
  REAL( DFP ) :: Ans(3, n)
END FUNCTION triangle_random_point
END INTERFACE

INTERFACE RandomPoint
  MODULE PROCEDURE triangle_random_point
END INTERFACE RandomPoint

PUBLIC :: RandomPoint

!----------------------------------------------------------------------------
!                                                          Quality@Triangle
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION triangle_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION triangle_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Quadrangle
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
MODULE PURE SUBROUTINE initiate_ref_Quadrangle( Obj, NSD, XiJ )
  CLASS( ReferenceQuadrangle_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Quadrangle
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Quadrangle
END INTERFACE Initiate

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
MODULE PURE FUNCTION reference_Quadrangle( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceQuadrangle_ ) :: Obj
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
MODULE PURE FUNCTION reference_Quadrangle_Pointer( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceQuadrangle_ ), POINTER :: Obj
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
!   call obj_ptr%LagrangeElement( order = 2, HighOrderObj = obj )
!   call display( obj, "higher order obj : ")
!   call obj_ptr%LagrangeElement( order = 3, HighOrderObj = obj )
!   call display( obj, "3rd order obj : ")
! end
!```

INTERFACE
MODULE PURE SUBROUTINE LagrangeElement_Quadrangle( RefElem, Order, Obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  CLASS( ReferenceElement_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE LagrangeElement_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Tetrahedron
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: This subroutine for constructing the object
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Tetrahedron( Obj, NSD, XiJ )
  CLASS( ReferenceTetrahedron_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Tetrahedron
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Tetrahedron
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferenceTetrahedron@Tetrahedron
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Tetrahedron( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceTetrahedron_ ) :: Obj
END FUNCTION reference_Tetrahedron
END INTERFACE

INTERFACE ReferenceTetrahedron
  MODULE PROCEDURE reference_Tetrahedron
END INTERFACE ReferenceTetrahedron

PUBLIC :: ReferenceTetrahedron

!----------------------------------------------------------------------------
!                                   ReferenceTetrahedron_Pointer@Tetrahedron
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Tetrahedron_Pointer( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceTetrahedron_ ), POINTER :: Obj
END FUNCTION reference_Tetrahedron_Pointer
END INTERFACE

INTERFACE ReferenceTetrahedron_Pointer
  MODULE PROCEDURE reference_Tetrahedron_Pointer
END INTERFACE ReferenceTetrahedron_Pointer

PUBLIC :: ReferenceTetrahedron_Pointer

!----------------------------------------------------------------------------
!                                               LagrangeElement@Tetrahedron
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE LagrangeElement_Tetrahedron( RefElem, Order, Obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  CLASS ( ReferenceElement_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE LagrangeElement_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Hexahedron
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This subroutine for constructing the object

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Hexahedron( Obj, NSD, XiJ )
  CLASS( ReferenceHexahedron_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Hexahedron
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Hexahedron
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferenceHexahedron@Hexahedron
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Hexahedron( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferenceHexahedron_ ) :: Obj
END FUNCTION reference_Hexahedron
END INTERFACE

INTERFACE ReferenceHexahedron
  MODULE PROCEDURE reference_Hexahedron
END INTERFACE ReferenceHexahedron

PUBLIC :: ReferenceHexahedron

!----------------------------------------------------------------------------
!                                    ReferenceHexahedron_Pointer@Hexahedron
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Hexahedron_Pointer( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferenceHexahedron_ ), POINTER :: Obj
END FUNCTION reference_Hexahedron_Pointer
END INTERFACE

INTERFACE ReferenceHexahedron_Pointer
  MODULE PROCEDURE reference_Hexahedron_Pointer
END INTERFACE ReferenceHexahedron_Pointer

PUBLIC :: ReferenceHexahedron_Pointer

!----------------------------------------------------------------------------
!                                                LagrangeElement@Hexahedron
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE LagrangeElement_Hexahedron( RefElem, Order, Obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  CLASS( ReferenceElement_ ), INTENT( INOUT) ::  Obj
END SUBROUTINE LagrangeElement_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Initiate@Pyramid
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	1 March 2021
! summary: This subroutine for constructing the object

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Pyramid( Obj, NSD, XiJ )
  CLASS( ReferencePyramid_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Pyramid
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Pyramid
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                   ReferencePyramid@Pyramid
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Pyramid( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferencePyramid_ ) :: Obj
END FUNCTION reference_Pyramid
END INTERFACE

INTERFACE ReferencePyramid
  MODULE PROCEDURE reference_Pyramid
END INTERFACE ReferencePyramid

PUBLIC :: ReferencePyramid

!----------------------------------------------------------------------------
!                                           ReferencePyramid_Pointer@Pyramid
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Pyramid_Pointer( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferencePyramid_ ), POINTER :: Obj
END FUNCTION reference_Pyramid_Pointer
END INTERFACE

INTERFACE ReferencePyramid_Pointer
  MODULE PROCEDURE reference_Pyramid_Pointer
END INTERFACE ReferencePyramid_Pointer

PUBLIC :: ReferencePyramid_Pointer

!----------------------------------------------------------------------------
!                                                  LagrangeElement@Pyramid
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE LagrangeElement_Pyramid( RefElem, Order, Obj )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  CLASS( ReferenceElement_ ), INTENT( INOUT ) ::  Obj
END SUBROUTINE LagrangeElement_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Initiate@Prism
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This subroutine for constructing the object

INTERFACE
MODULE PURE SUBROUTINE initiate_ref_Prism( Obj, NSD, XiJ )
  CLASS( ReferencePrism_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ( :, : )
END SUBROUTINE initiate_ref_Prism
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Prism
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                       ReferencePrism@Prism
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Prism( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  TYPE( ReferencePrism_ ) :: Obj
END FUNCTION reference_Prism
END INTERFACE

INTERFACE ReferencePrism
  MODULE PROCEDURE reference_Prism
END INTERFACE ReferencePrism

PUBLIC :: ReferencePrism

!----------------------------------------------------------------------------
!                                               ReferencePrism_Pointer@Prism
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION reference_Prism_Pointer( NSD, XiJ ) RESULT( Obj )
  INTEGER( I4B ), INTENT( IN ) :: NSD
  REAL( DFP ), INTENT( IN ), OPTIONAL :: XiJ(:,:)
  CLASS( ReferencePrism_ ), POINTER :: Obj
END FUNCTION reference_Prism_Pointer
END INTERFACE

INTERFACE ReferencePrism_Pointer
  MODULE PROCEDURE reference_Prism_Pointer
END INTERFACE ReferencePrism_Pointer

PUBLIC :: ReferencePrism_Pointer

!----------------------------------------------------------------------------
!                                                     LagrangeElement@Prism
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE LagrangeElement_Prism( RefElem, Order, Obj )
  CLASS( ReferencePrism_ ), INTENT( IN ) :: RefElem
  INTEGER( I4B ), INTENT( IN ) :: Order
  CLASS( ReferencePrism_ ), INTENT( INOUT) ::  Obj
END SUBROUTINE LagrangeElement_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ElementType@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns element name in integer from element name
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Element_Type( ElemName ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: ElemName
  INTEGER( I4B ) :: Ans
END FUNCTION Element_Type
END INTERFACE

INTERFACE ElementType
  MODULE PROCEDURE Element_Type
END INTERFACE ElementType

PUBLIC :: ElementType

!----------------------------------------------------------------------------
!                                                       ElementName@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns element name in character from element number/type
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Element_Name( ElemType ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: ElemType
  CHARACTER( LEN = 50 ) :: Ans
END FUNCTION Element_Name
END INTERFACE

INTERFACE ElementName
  MODULE PROCEDURE Element_Name
END INTERFACE ElementName

PUBLIC :: ElementName

!----------------------------------------------------------------------------
!                                              TotalNodesInElement@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns total numbers of nodes present in a given element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Total_Nodes_In_Element(ElemType) RESULT( Ans )
  INTEGER( I4B ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION Total_Nodes_In_Element
END INTERFACE

INTERFACE TotalNodesInElement
  MODULE PROCEDURE Total_Nodes_In_Element
END INTERFACE TotalNodesInElement

PUBLIC :: TotalNodesInElement

!----------------------------------------------------------------------------
!                                                     ElementOrder@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns the order of an element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Element_Order( ElemType ) RESULT( Ans )
  INTEGER( I4B ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION Element_Order
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ElementOrder@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns the order of an element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Element_Order_RefElem( RefElem ) RESULT( Ans )
  INTEGER( I4B ) :: Ans
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
END FUNCTION Element_Order_RefElem
END INTERFACE

INTERFACE ElementOrder
  MODULE PROCEDURE Element_Order_RefElem, Element_Order
END INTERFACE ElementOrder

PUBLIC :: ElementOrder

!----------------------------------------------------------------------------
!                                                      XiDimension@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns the xidimension of an element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Elem_XiDimension( ElemType ) RESULT( Ans )
  INTEGER( I4B ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION Elem_XiDimension
END INTERFACE

INTERFACE XiDimension
  MODULE PROCEDURE Elem_XiDimension
END INTERFACE XiDimension

PUBLIC :: XiDimension

!----------------------------------------------------------------------------
!                                                          isVolume@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a volume element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isVolume( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isVolume
END INTERFACE

PUBLIC :: isVolume

!----------------------------------------------------------------------------
!                                                        isSurface@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Surface element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isSurface( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isSurface
END INTERFACE

PUBLIC :: isSurface

!----------------------------------------------------------------------------
!                                                           isLine@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Line element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isLine( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isLine
END INTERFACE

PUBLIC :: isLine

!----------------------------------------------------------------------------
!                                                          isPoint@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Point element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isPoint( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isPoint
END INTERFACE

PUBLIC :: isPoint

!----------------------------------------------------------------------------
!                                                        isTriangle@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Triangle element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isTriangle( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isTriangle
END INTERFACE

PUBLIC :: isTriangle

!----------------------------------------------------------------------------
!                                                      isQuadrangle@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Quadrangle element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isQuadrangle( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isQuadrangle
END INTERFACE

PUBLIC :: isQuadrangle

!----------------------------------------------------------------------------
!                                                    isTetrahedron@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Tetrahedron element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isTetrahedron( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isTetrahedron
END INTERFACE

PUBLIC :: isTetrahedron

!----------------------------------------------------------------------------
!                                                     isHexahedron@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Hexahedron element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isHexahedron( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isHexahedron
END INTERFACE

PUBLIC :: isHexahedron

!----------------------------------------------------------------------------
!                                                            isPrism@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Prism element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isPrism( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isPrism
END INTERFACE

PUBLIC :: isPrism

!----------------------------------------------------------------------------
!                                                         isPyramid@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a Pyramid element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isPyramid( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isPyramid
END INTERFACE

PUBLIC :: isPyramid

!----------------------------------------------------------------------------
!                                               isSerendipityElement@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!  1. Returns true if element is a SerendipityElement element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION isSerendipityElement( ElemType ) RESULT( Ans )
  LOGICAL( LGT ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION isSerendipityElement
END INTERFACE

PUBLIC :: isSerendipityElement

!----------------------------------------------------------------------------
!                                                   ElementTopology@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1.  This will return the element topology
!     Line
!     Triangle
!     Quadrangle
!     Tetrahedron
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Elem_Topology( ElemType ) RESULT( Ans )
  INTEGER( I4B ) :: Ans
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END FUNCTION Elem_Topology
END INTERFACE

INTERFACE ElementTopology
  MODULE PROCEDURE Elem_Topology
END INTERFACE ElementTopology

PUBLIC :: ElementTopology

!----------------------------------------------------------------------------
!                                                       FacetMatrix@Geometry
!----------------------------------------------------------------------------
INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Returns the facet matrix
! Number of rows are equal to the number of facet in an element
! Number of columns = MAX( NNS )
! First column => ElementTopology
! Second Column => XiDimension
! Third column => NNS
! 4 to NNS + 3 => Local Nptrs
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Facet_Matrix_RefElem( RefElem ) RESULT( FM )
  INTEGER( I4B ), ALLOCATABLE :: FM( :, : )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
END FUNCTION Facet_Matrix_RefElem
END INTERFACE

INTERFACE FacetMatrix
  MODULE PROCEDURE Facet_Matrix_RefElem
END INTERFACE FacetMatrix

PUBLIC :: FacetMatrix

!----------------------------------------------------------------------------
!                                                    LocalNodeCoord@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! This routine will be removed in near future
! This routine is not included in generic LocalNodeCoord routine
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE SUBROUTINE Local_NodeCoord( NodeCoord, ElemType )
  ! Define intent of dummy variables
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: NodeCoord( :, : )
  INTEGER( I4B ), INTENT( IN ) :: ElemType
END SUBROUTINE Local_NodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                    LocalNodeCoord@Geometry
!----------------------------------------------------------------------------

INTERFACE
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! Returns the local NodeCoord of an element
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
MODULE PURE FUNCTION Local_NodeCoord_RefElem( RefElem ) RESULT( NodeCoord )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), ALLOCATABLE :: NodeCoord( :, : )
END FUNCTION Local_NodeCoord_RefElem
END INTERFACE

INTERFACE LocalNodeCoord
  MODULE PROCEDURE Local_NodeCoord_RefElem
END INTERFACE LocalNodeCoord

PUBLIC :: LocalNodeCoord

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

!----------------------------------------------------------------------------
!                                                         Quadrangle_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Quadrangle_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceQuadrangle_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Quadrangle_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Tetrahedron( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceTetrahedron_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                       tetrahedron_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION tetrahedron_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( Referencetetrahedron_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION tetrahedron_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Hexahedron( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Hexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Hexahedron_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Hexahedron_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Hexahedron_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                  MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Prism( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferencePrism_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Prism
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Prism_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Prism_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferencePrism_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Prism_quality
END INTERFACE


!----------------------------------------------------------------------------
!                                                  MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex_Pyramid( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferencePyramid_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Pyramid_quality
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Pyramid_quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferencePyramid_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Pyramid_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Measure_Simplex( RefElem, XiJ ) RESULT( Ans )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  REAL( DFP ), INTENT( IN ) :: XiJ( :, : )
  REAL( DFP ) :: Ans
END FUNCTION Measure_Simplex
END INTERFACE

INTERFACE MeasureSimplex
  MODULE PROCEDURE Measure_Simplex
END INTERFACE MeasureSimplex

PUBLIC :: MeasureSimplex

!----------------------------------------------------------------------------
!                                                    ElementQuality@Geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Element_Quality( refelem, xij, measure ) RESULT( Ans )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  REAL( DFP ) , INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ) :: Ans
END FUNCTION Element_Quality
END INTERFACE

INTERFACE ElementQuality
  MODULE PROCEDURE Element_Quality
END INTERFACE ElementQuality

PUBLIC :: ElementQuality

!----------------------------------------------------------------------------
!                                                     ContainsPoint@geometry
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION contains_point( refelem , xij, x ) RESULT( Ans )
  CLASS( ReferenceElement_ ), INTENT( IN ) :: refelem
  REAL( DFP ), INTENT( IN ) :: xij( :, : )
  REAL( DFP ), INTENT( IN ) :: x(:)
  LOGICAL( LGT ) :: Ans
END FUNCTION contains_point
END INTERFACE

INTERFACE ContainsPoint
  MODULE PROCEDURE contains_point
END INTERFACE ContainsPoint

PUBLIC :: ContainsPoint

!----------------------------------------------------------------------------
!                                                          getVTKelementType
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE get_vtk_elemType( ElemType, vtk_type, nptrs )
  INTEGER( I4B ), INTENT( IN ) :: ElemType
  INTEGER( Int8 ), INTENT (OUT) :: vtk_type
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT) :: nptrs( : )
END SUBROUTINE get_vtk_elemType
END INTERFACE

INTERFACE getVTKelementType
  MODULE PROCEDURE get_vtk_elemType
END INTERFACE getVTKelementType

PUBLIC :: getVTKelementType

END MODULE ReferenceElement_Method