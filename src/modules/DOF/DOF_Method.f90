MODULE DOF_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Initiate [[DOF_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate [[DOF_]] object
!
! - If size of all dof are same then set tNodes = [tNodes] otherwise
!   we need to provide length of all dofs
! - If a dof is scalar, such as pressure, temperature etc., then set
! its spaceCompo to -1.
! - For a time independent dof set `TimeCompo=1`.
! - The size of `Names`, `SpaceCompo`, `TimeCompo` should be same
!
! @note
! 	$\matbf{v}$ is a physical variable, however,
!   its component $v_1, v_2, v_3$ all are degrees of freedom.
! @endnote
!
!### Usage
!
! ```fortran
!	type( dof_ ) :: obj
! call initiate( obj, tNodes = [10], Names = ['x', 'y'], &
!   & SpaceCompo = [3,3], TimeCompo = [1,1], StorageFMT = FMT_Nodes )
! ```

MODULE PURE SUBROUTINE initiate_st_dof( Obj, tNodes, Names, SpaceCompo, &
  & TimeCompo, StorageFMT )
  CLASS( DOF_ ), INTENT( INOUT ) :: Obj
    !! degree of freedom object
  INTEGER( I4B ), INTENT( IN ) :: tNodes( : )
    !! number of nodes for each physical variable
  INTEGER( I4B ), INTENT( IN ) :: SpaceCompo( : )
    !! Space component of each physical variable
  INTEGER( I4B ), INTENT( IN ) :: TimeCompo( : )
    !! Time component of each physical variable
  INTEGER( I4B ), INTENT( IN ) :: StorageFMT
    !! Storage format `FMT_DOF`, `FMT_Nodes`
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Names( : )
    !! Names of each physical variable
END SUBROUTINE initiate_st_dof

END INTERFACE

INTERFACE
!! Initiate a fortran vector using [[dof_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate a fortran vector using information stored inside
! [[dof_]] object

MODULE PURE SUBROUTINE initiate_val( Val, Obj )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Val( : )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
END SUBROUTINE initiate_val
END INTERFACE

INTERFACE
!! Initiate [[RealVector_]] using [[dof_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate [[RealVector_]] using information stored inside
! [[dof_]] object

MODULE PURE SUBROUTINE initiate_realvector_scalar( Val, Obj )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Val
  CLASS( DOF_ ), INTENT( IN ) :: Obj
END SUBROUTINE initiate_realvector_scalar
END INTERFACE

INTERFACE
!! Initiate a vector of [[realvector_]] from [[dof_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate a vector of [[realvector_]] object
! Each entry Val( idof ) denotes degree of freedom `idof`

MODULE PURE SUBROUTINE initiate_realvector_vector( Val, Obj )
  TYPE( RealVector_ ), ALLOCATABLE, INTENT( INOUT ) :: Val( : )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
END SUBROUTINE initiate_realvector_vector
END INTERFACE

INTERFACE
!! Initiate two fortran vectors using [[dof_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate two fortran vectors using  the information
! stored inside the [[dof_]] object

MODULE PURE SUBROUTINE initiate_2val( Val1, Val2, Obj )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: Val1( : ), Val2( : )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
END SUBROUTINE initiate_2val
END INTERFACE

!> Generic interface to initiate Fortran vectors or [[realvectors_]] from
! [[dof_]] object
INTERFACE Initiate
  MODULE PROCEDURE initiate_st_dof, initiate_val, &
    & initiate_realvector_scalar, &
    & initiate_realvector_vector, &
    & initiate_2val
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                            DOF@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Constructor for [[dof_]] object

!> authors: Dr. Vikas Sharma
!
! This function return instance of [[dof_]]
! for more see [[dof_:initiate]]

MODULE PURE FUNCTION Constructor1( tNodes, Names, SpaceCompo, TimeCompo, &
  & StorageFMT ) RESULT( Obj )
  TYPE(DOF_) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tNodes( : ), SpaceCompo( : ), &
    & TimeCompo( : ), StorageFMT
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Names( : )
END FUNCTION Constructor1
END INTERFACE

!> Generic function to construct [[dof_]] object
INTERFACE DOF
  MODULE PROCEDURE Constructor1
END INTERFACE DOF

PUBLIC :: DOF

!----------------------------------------------------------------------------
!                                                     DOF_Pointer@Constructor
!----------------------------------------------------------------------------

!> Generic interface to get pointer to instance of [[dof_]] object
INTERFACE DOF_Pointer
  MODULE PROCEDURE Constructor_1
END INTERFACE DOF_Pointer

PUBLIC :: DOF_Pointer

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Deallocate data in [[dof_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine deallocates the data in [[dof_]] object

MODULE PURE SUBROUTINE deallocate_data( Obj )
  CLASS(DOF_), INTENT( INOUT ) :: Obj
END SUBROUTINE deallocate_data
END INTERFACE

!> Generic interface to deallocate data in [[dof_]]
INTERFACE DeallocateData
  MODULE PROCEDURE deallocate_data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                        Display@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Display content of [[dof_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine display the content of [[dof_]] object

MODULE SUBROUTINE display_obj( Obj, msg, UnitNo )
  CLASS(DOF_), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE display_obj
END INTERFACE

INTERFACE
!! Display content of fortran vec with [[DOF_]] object info

MODULE SUBROUTINE dof_display_vec( Vec, Obj, msg, unitno )
  REAL( DFP ), INTENT( IN ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE dof_display_vec
END INTERFACE

!> Generic subroutine to displacy content of [[dof_]]
INTERFACE Display
  MODULE PROCEDURE display_obj, dof_display_vec
END INTERFACE Display

PUBLIC :: Display


!----------------------------------------------------------------------------
!                                                            tNodes@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the total number of nodes

!> authors: Dr. Vikas Sharma
!
!  This function returns the total length of the vector which stores the
! dof stored inside `obj`.

  MODULE PURE FUNCTION get_tNodes( Obj ) RESULT( Ans )
    CLASS( DOF_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ) :: Ans
  END FUNCTION get_tNodes
END INTERFACE

INTERFACE OPERATOR( .tNodes. )
  MODULE PROCEDURE get_tNodes
END INTERFACE

PUBLIC :: OPERATOR( .tNodes. )

INTERFACE SIZE
  MODULE PROCEDURE get_tNodes
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                                           tNodes@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the total number of nodes

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the size of a given degree of freedom

MODULE PURE FUNCTION get_tNodes_idof( Obj, idof ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: idof
  INTEGER( I4B ) :: Ans
END FUNCTION get_tNodes_idof
END INTERFACE

INTERFACE OPERATOR( .tNodes. )
  MODULE PROCEDURE get_tNodes_idof
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE get_tNodes_idof
END INTERFACE SIZE

!----------------------------------------------------------------------------
!                                                             tDOF@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the total number of degree of freedom

!> authors: Dr. Vikas Sharma
!
! This function returns the total dof

  MODULE PURE FUNCTION get_tDOF( Obj ) RESULT( Ans )
    CLASS( DOF_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ) :: Ans
  END FUNCTION get_tDOF
END INTERFACE

INTERFACE OPERATOR( .tDOF. )
  MODULE PROCEDURE get_tDOF
END INTERFACE

PUBLIC :: OPERATOR( .tDOF. )

INTERFACE
!! This subroutine returns the total number of degrees of freedom

!> authors: Dr. Vikas Sharma
!
! This function returns the total number of dof in given physical name

MODULE PURE FUNCTION get_tDOF_iname( Obj, Name ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Name
  INTEGER( I4B ) :: Ans
END FUNCTION get_tDOF_iname
END INTERFACE

INTERFACE OPERATOR( .tDOF. )
  MODULE PROCEDURE get_tDOF_iname
END INTERFACE OPERATOR( .tDOF. )

!----------------------------------------------------------------------------
!                                                            tNames@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the total number of names

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the total number of names or total number of
! physical quantities

MODULE PURE FUNCTION get_tNames( Obj ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION get_tNames
END INTERFACE

INTERFACE OPERATOR( .tNames. )
  MODULE PROCEDURE get_tNames
END INTERFACE

PUBLIC :: OPERATOR( .tNames. )

!----------------------------------------------------------------------------
!                                                           Names@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the name of all physical variables stored in obj

MODULE PURE FUNCTION dof_all_names( Obj ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = 1 ), ALLOCATABLE :: Ans( : )
END FUNCTION dof_all_names
END INTERFACE

INTERFACE
!! This function returns the name of a physical variable

MODULE PURE FUNCTION dof_single_name( Obj, ii ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: ii
  CHARACTER( LEN = 1 ), ALLOCATABLE :: Ans
END FUNCTION dof_single_name
END INTERFACE

INTERFACE OPERATOR( .Names. )
  MODULE PROCEDURE dof_all_names, dof_single_name
END INTERFACE OPERATOR( .Names. )

PUBLIC :: OPERATOR( .Names. )

!----------------------------------------------------------------------------
!                                                         IndexOF@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION get_index_of_name( Obj, Name ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Name
  INTEGER( I4B ) :: Ans
END FUNCTION get_index_of_name
END INTERFACE

INTERFACE IndexOf
  MODULE PROCEDURE get_index_of_name
END INTERFACE IndexOf

PUBLIC :: IndexOf

!----------------------------------------------------------------------------
!                                                tSpaceComponents@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION get_tspace_compo( Obj ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION get_tspace_compo
END INTERFACE

INTERFACE OPERATOR( .tSpaceComponents. )
  MODULE PROCEDURE get_tspace_compo
END INTERFACE

PUBLIC :: OPERATOR( .tSpaceComponents. )

!----------------------------------------------------------------------------
!                                                tTimeComponents@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION get_tTime_compo( Obj ) RESULT( Ans )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION get_tTime_compo
END INTERFACE

INTERFACE OPERATOR( .tTimeComponents. )
  MODULE PROCEDURE get_tTime_compo
END INTERFACE

PUBLIC :: OPERATOR( .tTimeComponents. )

!----------------------------------------------------------------------------
!                                                   getArrayValues@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! Returns the values of degrees of freedom in a single vector

!> authors: Dr. Vikas Sharma
!
! This subroutine extracts the values of from `val` corresponding to
! degrees of freedom specified by `DOFNo(:)` and return it in `V`
!
! - `StorageFMT` can be 'Nodes_FMT' or `DOF_FMT`

MODULE PURE SUBROUTINE get_arrayvalues_single_vec( v, Val, Obj, DOFNo, &
  & StorageFMT, Nptrs )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: v( : )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT(IN ) :: DOFNo( : ), StorageFMT
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Nptrs( : )
END SUBROUTINE get_arrayvalues_single_vec
END INTERFACE

INTERFACE getArrayValues
  MODULE PROCEDURE get_arrayvalues_single_vec
END INTERFACE

PUBLIC :: getArrayValues

!----------------------------------------------------------------------------
!                                                   getArrayValues@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! Returns the values of degrees of freedom in a 2D array

!> authors: Dr. Vikas Sharma
!
! This subroutine extracts the values of from `val` corresponding to
! degrees of freedom specified by `DOFNo(:)` and return it in `V(:,:)`
! Values in `Val(:,:)` are stored in xiJ format.
!
! - Force3D will return a vector in 3D. if there are only two components
! then it will set the third component to 0
!

MODULE PURE SUBROUTINE get_arrayvalues_array( v, Val, Obj, DOFNo, force3D )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: v( :, : )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT(IN ) :: DOFNo( : )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: force3D
END SUBROUTINE get_arrayvalues_array
END INTERFACE

INTERFACE getArrayValues
  MODULE PROCEDURE get_arrayvalues_array
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ArrayValues@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION arrayvalues_single_vec( Val, Obj, DOFNo, &
    & StorageFMT, Nptrs, Force3D )  RESULT( Ans )
    REAL( DFP ), INTENT( IN ) :: Val( : )
    CLASS( DOF_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT(IN ) :: DOFNo( : ), StorageFMT
    INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Nptrs( : )
    LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: force3D
    REAL( DFP ), ALLOCATABLE :: Ans( : )
  END FUNCTION arrayvalues_single_vec
END INTERFACE

INTERFACE ArrayValues
  MODULE PROCEDURE arrayvalues_single_vec
END INTERFACE

PUBLIC :: ArrayValues


!----------------------------------------------------------------------------
!                                                        setValue@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! Set values in a vector of real numbers

!> authors: Dr. Vikas Sharma
!
! This subroutine is designed to set the values in a vector of real number
!
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `Vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `Val` denotes the nodal values of all dof defined inside `Obj`. Once
! storage pattern in `Val` can be `FMT_DOF` or `FMT_Nodes`.
! - To tackle this `Conversion`  can be set to `DOFToNodes`, `NodesToDOF`
! or `NONE`.
! - This subroutine effectivily performes `Vec( Nptrs ) = Val`
! - If `SIZE(val)==1` then all values are set to `val(1)`
! - If `SIZE(val) .EQ. SIZE(Nptrs)` then for each dof its value set to
!  `val`
! - If `SIZE(val)=tDOF*Size(Nptrs)` then each dof will be set to
! corresponding val

MODULE PURE SUBROUTINE dof_setValue_1( Vec, Obj, Nptrs, Val, Conversion )
  REAL( DFP ), INTENT( INOUT ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : ), Conversion( 1 )
  REAL( DFP ), INTENT( IN ) :: Val( : )
END SUBROUTINE dof_setValue_1
END INTERFACE

INTERFACE
!! Set values in a vector of real numbers

!> authors: Dr. Vikas Sharma
!
! This subroutine is designed to set the values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `Vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `Val` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes `Vec( Nptrs ) = Val`

MODULE PURE SUBROUTINE dof_setValue_2( Vec, Obj, Nptrs, Val, dofno )
  REAL( DFP ), INTENT( INOUT ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  INTEGER( I4B ), INTENT( IN ) :: dofno
END SUBROUTINE dof_setValue_2
END INTERFACE

!> Generic subroutine to set values in fortran vectors using [[dof_]] object
INTERFACE setValue
  MODULE PROCEDURE dof_setValue_1, dof_setValue_2
END INTERFACE setValue

PUBLIC :: setValue


!----------------------------------------------------------------------------
!                                                 addContribution@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! Set values in a vector of real numbers

!> authors: Dr. Vikas Sharma
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `Vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `Val` denotes the nodal values of all dof defined inside `Obj`. Once
! storage pattern in `Val` can be `FMT_DOF` or `FMT_Nodes`.
! - To tackle this `Conversion`  can be set to `DOFToNodes`, `NodesToDOF`
! or `NONE`.
!
! This subroutine effectivily performes
! `Vec( Nptrs ) = Vec(Nptrs) + scale * Val`

MODULE PURE SUBROUTINE dof_addValue_1( Vec, Obj, Nptrs, Val, scale, &
  & Conversion )
  REAL( DFP ), INTENT( INOUT ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : ), Conversion( 1 )
  REAL( DFP ), INTENT( IN ) :: Val( : ), scale
END SUBROUTINE dof_addValue_1
END INTERFACE

INTERFACE
!! Set values in a vector of real numbers

!> authors: Dr. Vikas Sharma
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `Vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `Val` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `Vec( Nptrs ) = Vec(Nptrs) + scale * Val`

MODULE PURE SUBROUTINE dof_addValue_2( Vec, Obj, Nptrs, Val, scale, dofno )
  REAL( DFP ), INTENT( INOUT ) :: Vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  REAL( DFP ), INTENT( IN ) :: Val( : ), scale
  INTEGER( I4B ), INTENT( IN ) :: dofno
END SUBROUTINE dof_addValue_2
END INTERFACE

!> Generic subroutine to add values in vectors using [[dof_]] object
INTERFACE addContribution
  MODULE PROCEDURE dof_addValue_1, dof_addValue_2
END INTERFACE addContribution

PUBLIC :: addContribution

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                DOF_Pointer
!----------------------------------------------------------------------------

!! Constructor for pointer to [[dof_]] object

!> authors: Dr. Vikas Sharma
!
! This function returns the pointer to instance of [[dof_]] object
! for more see [[dof_:initiate]]

FUNCTION Constructor_1( tNodes, Names, SpaceCompo, TimeCompo, &
  & StorageFMT ) RESULT( Obj )

  CLASS(DOF_), POINTER :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tNodes( : ), SpaceCompo( : ), &
    & TimeCompo( : ), StorageFMT
  CHARACTER( LEN = 1 ), INTENT( IN ) :: Names( : )
  ALLOCATE( Obj )
  CALL Initiate( Obj = Obj, Names = Names, tNodes = tNodes, &
    & SpaceCompo = SpaceCompo, TimeCompo = TimeCompo, StorageFMT = StorageFMT)
END FUNCTION Constructor_1



END MODULE DOF_Method