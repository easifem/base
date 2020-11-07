MODULE ElemshapeData_Method
  !! This module implements methods related to [[elemShapeData_]] datatype
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine allocate the memory for various matrices in the object

!> authors: Dr. Vikas Sharma
!
! This subroutine allocates the memory for various arrays in &
! [[elemshapedata_]] object
MODULE PURE SUBROUTINE initiate_obj( Obj, nsd, xidim, nns, nips )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
    !! object to be returned
  INTEGER( I4B ), INTENT( IN ) :: nsd
    !! spatial dimension
  INTEGER( I4B ), INTENT( IN ) :: xidim
    !! xidimension
  INTEGER( I4B ), INTENT( IN ) :: nns
    !! number of nodes in element
  INTEGER( I4B ), INTENT( IN ) :: nips
    !! number of integration points
END SUBROUTINE initiate_obj
END INTERFACE

INTERFACE AllocateData
  MODULE PROCEDURE initiate_obj
END INTERFACE AllocateData

PUBLIC :: AllocateData


!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine initiate time shape function data in [[stelemshapedata_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate time shape function data in [[stelemshapedata_]].
! To do so effeciently we construct local shape function for time element
! externally by using [[elemshapedata_]], and supply this information.
!
! This subroutine set `T`, `dTdTheta`, `Jt`, `Wt`, `Theta`
!
! - it will allocate `Obj`
! - the size of `Obj` will be equal to total number of integration points in
!   in time domain

MODULE PURE SUBROUTINE stsd_initiate( Obj, elemsd )
  TYPE( STElemShapeData_ ), ALLOCATABLE, INTENT( INOUT ) :: Obj( : )
  TYPE( ElemShapeData_ ), INTENT( IN ) :: elemsd
END SUBROUTINE stsd_initiate
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE stsd_initiate
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! this subroutine deallocates the data stored inside [[elemshapedata_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine deallocates the data stored inside [[elemshapedata_]] and
! [[stelemshapedata_]]
MODULE PURE SUBROUTINE deallocate_data( Obj )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE deallocate_data
END INTERFACE

INTERFACE DeallocateData
    MODULE PROCEDURE deallocate_data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                        Display@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine display the content of object

!> authors: Dr. Vikas Sharma
!
! This subroutine displays the content of [[elemshapedata_]] and
! [[stelemshapedata_]]
MODULE SUBROUTINE display_obj( Obj, Msg, UnitNo )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE display_obj
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE display_obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                     setThickness@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the thickness field

!> authors: Dr. Vikas Sharma
!
! This subroutine set the `thickness` field
! Here `Val` denotes the nodal value of thickeness
!
! $$d = d_{I} N^{I}$$
MODULE PURE SUBROUTINE set_thickness( Obj, Val, N )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : ), N( :, : )
  !! Nodal values of thickness
END SUBROUTINE set_thickness
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setThickness@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the thickness field

!> authors: Dr. Vikas Sharma
!
! This subroutine set the `thickness` field
! Here `Val` denotes the space-time nodal value of thickeness
!
! $$d = d_{I}^{a} N^{I} T_{a}$$
MODULE PURE SUBROUTINE stsd_set_thickness( Obj, Val, N, T )
  CLASS( STElemshapeData_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : ), N(:,:), T(:)
    !! Space-time nodal values of thickness
END SUBROUTINE stsd_set_thickness
END INTERFACE

INTERFACE setThickness
  MODULE PROCEDURE set_thickness, stsd_set_thickness
END INTERFACE

PUBLIC :: setThickness

!----------------------------------------------------------------------------
!                                              setBarycentricCoord@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the Barycentric coordinates

!> authors: Dr. Vikas Sharma
!
! This subroutine set the barycentric coordinates
!
! $$x_i = x_{iI} N^{I}$$

MODULE PURE SUBROUTINE set_coord( Obj, Val, N )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
    !! Nodal coordinates in `xiJ` format
  REAL( DFP ), INTENT( IN ) :: N(:,:)
    !! When element is not an isoparametric we can supply N.
END SUBROUTINE set_coord
END INTERFACE

!----------------------------------------------------------------------------
!                                              setBarycentricCoord@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the Barycentric coordinates

!> authors: Dr. Vikas Sharma
!
! This subroutine set the barycentric coordinates by using
! space-time nodal coordinates
!
! $$x=x_{I}^{a} N^I T_a$$

MODULE PURE SUBROUTINE stsd_set_coord( Obj, Val, N, T )
  CLASS( STElemshapeData_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
    !! space-time Nodal coordinates in `xiJ` format
  REAL( DFP ), INTENT( IN ) :: N( :, : ), T( : )
    !! N and T are required to handle non isoparametric elements
END SUBROUTINE stsd_set_coord
END INTERFACE

INTERFACE setBarycentricCoord
  MODULE PROCEDURE set_coord, stsd_set_coord
END INTERFACE setBarycentricCoord

PUBLIC :: setBarycentricCoord

!----------------------------------------------------------------------------
!                                                            setJs@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the determinent of jacobian

!> authors: Dr. Vikas Sharma
!
! This subroutine will internally set the value of jacobian
MODULE PURE SUBROUTINE set_Js( Obj )
  CLASS( ElemShapeData_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE set_Js
END INTERFACE

INTERFACE setJs
  MODULE PROCEDURE set_Js
END INTERFACE setJs

PUBLIC setJs

!----------------------------------------------------------------------------
!                                                          setdNdXt@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set $\frac{d N}{d X_t}$ internally

!> authors: Dr. Vikas Sharma
!
! This subroutine will internally set `dNdXt`.
! It use the inverse of jacobian stored internally, so make sure jacobian is
! set before calling this  subroutine.
MODULE PURE SUBROUTINE set_dNdXt_internally( Obj )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE set_dNdXt_internally
END INTERFACE

INTERFACE setdNdXt
  MODULE PROCEDURE set_dNdXt_internally
END INTERFACE setdNdXt

PUBLIC :: setdNdXt

!----------------------------------------------------------------------------
!                                                      setJacobian@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the jacobian

!> authors: Dr. Vikas Sharma
!
! This subroutine set the jacobian by using the nodal coordinates
!
! $$\frac{d x_i}{d \xi_j} = x_{iI}\frac{d N^I}{d \xi_j}$$
MODULE PURE SUBROUTINE set_jacobian( Obj, Val, dNdXi )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
    !! nodal coordinates in `xiJ` format
  REAL( DFP ), INTENT( IN ) :: dNdXi( :, :, : )
END SUBROUTINE set_jacobian
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setJacobian@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the jacobian using space-time nodal coords

!> authors: Dr. Vikas Sharma
!
! This subroutine set the jacobian by using space-time nodal coords, `dNdXi`
! `T` are used to handle non-isoparameteric elements.
!
! $$\frac{d x_i}{d \xi_j} = x_{iI}^{a}T_a\frac{d N^I}{d \xi_j}$$

MODULE PURE SUBROUTINE stsd_set_jacobian( Obj, Val, dNdXi, T )
  CLASS( STElemshapeData_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
    !! Space time nodal values of coordinates
  REAL( DFP ), INTENT( IN ) :: dNdXi(:,:,:)
    !! Local derivative of shape function for geometry
  REAL( DFP ), INTENT( IN ) :: T(:)
    !! Shape function for time element
END SUBROUTINE stsd_set_jacobian
END INTERFACE

INTERFACE setJacobian
  MODULE PROCEDURE set_jacobian, stsd_set_jacobian
END INTERFACE setJacobian

PUBLIC :: setJacobian

!----------------------------------------------------------------------------
!                                                         setdNTdt@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set `dNTdt` by using the space-time nodal values

!> authors: Dr. Vikas Sharma
!
! - This subroutine set `dNTdt` by using space-time nodal values
! - It is important to note that `dNTdXt` should be allocated before calling
! - This subroutine uses following formula
!
! $$\frac{\partial N^{I\  }T_{a}}{\partial t} =N^{I}\frac{\partial T_{a}}
! {\partial \theta } J^{-1}_{t}-\frac{\partial N^{I}T_{a}}{\partial x_{k}}
! \hat{v}_{k} $$

MODULE PURE SUBROUTINE stsd_set_dNTdt( Obj, Val )
  CLASS( STElemShapeData_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
    !! Space-time nodal values
END SUBROUTINE stsd_set_dNTdt
END INTERFACE

INTERFACE setdNTdt
  MODULE PROCEDURE stsd_set_dNTdt
END INTERFACE setdNTdt

PUBLIC :: setdNTdt

!----------------------------------------------------------------------------
!                                                        setdNTdXt@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set `dNTdXt` by using internal data

!> authors: Dr. Vikas Sharma
!
! This subroutine set `dNTdXt` by using internal data
! This subroutine uses inverse of Jacobian, therefore, before calling
! this subroutine make sure to set jacobian
!
! $$\frac{\partial N^{I\  }T_{a}}{\partial x_{i\  }}
! =\frac{\partial N^{I}T_{a}}{\partial \xi_{j} } \frac{\partial \xi_{j} }
! {\partial x_{i}} $$

MODULE PURE SUBROUTINE stsd_set_dNTdXt_internally( Obj )
  CLASS( STElemShapeData_ ), INTENT( INOUT) :: Obj
  !! Space-time nodal values
END SUBROUTINE stsd_set_dNTdXt_internally
END INTERFACE

INTERFACE setdNTdXt
  MODULE PROCEDURE stsd_set_dNTdXt_internally
END INTERFACE setdNTdXt

PUBLIC :: setdNTdXt

!----------------------------------------------------------------------------
!                                                         setValue@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set parameters defined on physical element

!> authors: Dr. Vikas Sharma
!
! This subroutine set parameters defined on physical element
!
! - `Val` denotes nodal coordinates of element in `xiJ` format
! - This subroutine will call
!     - `setJacobian`
!     - `setJs`
!     - `setdNdXt`
!     - `setBarycentricCoord`
! - The facility of `N` and `dNdXi` allow us to handle non-isoparametric
! elements
!
! @note
! In case `Obj` is instance of [[stelemshapedata_]] then `val` will denotes
! coordinates of spatial nodes at some time in [tn, tn+1]
! @endnote

MODULE PURE SUBROUTINE set_value( Obj, Val, N, dNdXi )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
    !! Spatial nodal coordinates
  REAL( DFP ), INTENT( IN ) :: N(:, :)
    !! Shape function for geometry
  REAL( DFP ), INTENT( IN ) :: dNdXi( :, :, : )
    !! Local derivative of shape functions for geometry
END SUBROUTINE set_value
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setValue@setMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set parameters defined on physical element

!> authors: Dr. Vikas Sharma
!
! This subroutine set parameters defined on physical element
!
! - `Val` denotes coordinates of the space-time element in `xiJa` format
! - The facility of supplying `N`, `T`, and `dNdXi` allows us to handle
! non-isoparametric element
! - This subroutine will call
!     - `setJacobian` uses `dNdXi`
!     - `setJs`
!     - `setdNdXt`
!     - `setBarycentricCoord` uses `N` and `T`
!     - `setdNTdXt`
!     - `setdNTdt`
!
! @note
! In case of [[stelemshapedata_]] `val` denotes nodal coordinate at
! some intermediate space-time slab
! @endnote

MODULE PURE SUBROUTINE stsd_set_value( Obj, Val, N, T, dNdXi )
  CLASS( STElemshapeData_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
    !! Spatial nodal coordinates
  REAL( DFP ), INTENT( IN ) :: N( :, : )
  REAL( DFP ), INTENT( IN ) :: T( : )
  REAL( DFP ), INTENT( IN ) :: dNdXi( :, :, : )
END SUBROUTINE stsd_set_value
END INTERFACE

INTERFACE setValue
  MODULE PROCEDURE set_value, stsd_set_value
END INTERFACE setValue

PUBLIC :: setValue

!----------------------------------------------------------------------------
!                                                                 setNormal
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE set_normal( Obj )
  CLASS( ElemShapeData_ ), INTENT( INOUT) :: Obj
END SUBROUTINE set_normal
END INTERFACE

INTERFACE setNormal
  MODULE PROCEDURE set_normal
END INTERFACE setNormal

PUBLIC :: setNormal

!----------------------------------------------------------------------------
!                                                 getInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine performs interpolations

!> authors: Dr. Vikas Sharma
!
! This subroutine performs interpolation of a scalar from its spatial nodal
! values.
! $$u=u_{I}N^{I}$$
MODULE PURE SUBROUTINE get_interpol_scalar( Obj, Interpol, Val )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( INOUT ), ALLOCATABLE :: Interpol( : )
    !! Interpolation value of `val` at integration points
  REAL( DFP ), INTENT( IN ) :: Val( : )
    !! spatial nodal values of scalar
END SUBROUTINE get_interpol_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                getInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE get_interpol_fevar_scalar( Obj, Interpol, Val )
  CLASS(ElemShapeData_), INTENT( IN ) :: Obj
  TYPE( FEVariable_ ), INTENT( IN ) :: Val
  REAL( DFP ), INTENT(INOUT) :: Interpol(:)
END SUBROUTINE get_interpol_fevar_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine performs interpolation of a vector

!> authors: Dr. Vikas Sharma
!
! This subroutine performs interpolation of a vector from its spatial
! nodal values
!
! $$u_{i}=u_{iI}N^{I}$$
MODULE PURE SUBROUTINE get_interpol_vector( Obj, Interpol, Val )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( INOUT ), ALLOCATABLE :: Interpol( :, : )
    !! Interpol(:,ips) => interpolation value at integration points
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
    !! nodal values of vector in `xiJ` format
END SUBROUTINE get_interpol_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine performs interpolation of matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine performs interpolation of matrix
MODULE PURE SUBROUTINE get_interpol_matrix( Obj, Interpol, Val )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( INOUT ), ALLOCATABLE :: Interpol( :, :, : )
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
    !! nodal value of matrix
END SUBROUTINE get_interpol_matrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine performs interpolation of matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine performs interpolation of matrix
MODULE PURE SUBROUTINE get_interpol_fevar_matrix( Obj, Interpol, Val )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( INOUT ) :: Interpol( :, :, : )
  TYPE( FEVariable_), INTENT( IN ) :: Val
END SUBROUTINE get_interpol_fevar_matrix
END INTERFACE

INTERFACE getInterpolation
  MODULE PROCEDURE get_interpol_scalar, get_interpol_vector, &
    & get_interpol_matrix, get_interpol_fevar_scalar, &
    & get_interpol_fevar_matrix
END INTERFACE getInterpolation

PUBLIC :: getInterpolation

!----------------------------------------------------------------------------
!                                                    Interpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the interpolation of a scalar

!> authors: Dr. Vikas Sharma
!
! This function returns interpolation of scalar
MODULE PURE FUNCTION interpol_scalar( Obj, Val ) RESULT( Interpol )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  REAL( DFP ), ALLOCATABLE :: Interpol( : )
END FUNCTION interpol_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Interpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the interpolation of vector

!> authors: Dr. Vikas Sharma
!
! This function returns the interpolation of vectors
MODULE PURE FUNCTION interpol_vector( Obj, Val ) RESULT( Interpol )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  REAL( DFP ), ALLOCATABLE :: Interpol( :, : )
END FUNCTION interpol_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Interpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the interpolation of matrix

!> authors: Dr. Vikas Sharma
!
! This function returns the interpolation of matrix
MODULE PURE FUNCTION interpol_matrix( Obj, Val ) RESULT( Interpol )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
  REAL( DFP ), ALLOCATABLE :: Interpol( :, :, : )
END FUNCTION interpol_matrix
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE interpol_scalar, interpol_vector, interpol_matrix
END INTERFACE Interpolation

PUBLIC :: Interpolation

!----------------------------------------------------------------------------
!                                               getSTInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine performs interpolations of scalar

!> authors: Dr. Vikas Sharma
!
! This subroutine performs interpolation of a scalar from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$
MODULE PURE SUBROUTINE stsd_get_interpol_scalar( Obj, Interpol, Val )
  CLASS( STElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( INOUT ), ALLOCATABLE :: Interpol( : )
    !! Interpolation value of `val` at integration points
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
    !! spatial nodal values of scalar
END SUBROUTINE stsd_get_interpol_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                getInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE stsd_get_interpol_fevar_scalar( Obj, Interpol, Val )
  CLASS(STElemShapeData_), INTENT( IN ) :: Obj( : )
  TYPE( FEVariable_ ), INTENT( IN ) :: Val
  REAL( DFP ), INTENT(INOUT) :: Interpol(:, :)
END SUBROUTINE stsd_get_interpol_fevar_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                               getSTInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine performs interpolation of a vector

!> authors: Dr. Vikas Sharma
!
! This subroutine performs interpolation of a vector from its space-time
! nodal values
!
! $$u_{i}=u^{a}_{iI}N^{I}T_{a}$$
MODULE PURE SUBROUTINE stsd_get_interpol_vector( Obj, Interpol, Val )
  CLASS( STElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( INOUT ), ALLOCATABLE :: Interpol( :, : )
    !! Interpol(:,ips) => interpolation value at integration points
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
    !! space-time nodal values of vector in `xiJa` format
END SUBROUTINE stsd_get_interpol_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                               getSTInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine performs interpolation of matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine performs interpolation of matrix from its space-time
! nodal values
MODULE PURE SUBROUTINE stsd_get_interpol_matrix( Obj, Interpol, Val )
  CLASS( STElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( INOUT ), ALLOCATABLE :: Interpol( :, :, : )
  REAL( DFP ), INTENT( IN ) :: Val( :, :, :, : )
    !! nodal value of matrix
END SUBROUTINE stsd_get_interpol_matrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                getInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE stsd_get_interpol_fevar_matrix( Obj, Interpol, Val )
  CLASS(STElemShapeData_), INTENT( IN ) :: Obj( : )
  TYPE( FEVariable_ ), INTENT( IN ) :: Val
  REAL( DFP ), INTENT(INOUT) :: Interpol(:, :, :, :)
END SUBROUTINE stsd_get_interpol_fevar_matrix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE getInterpolation
  MODULE PROCEDURE stsd_get_interpol_scalar, stsd_get_interpol_vector, &
    & stsd_get_interpol_matrix, stsd_get_interpol_fevar_scalar, &
    & stsd_get_interpol_fevar_matrix
END INTERFACE getInterpolation

!----------------------------------------------------------------------------
!                                                 STInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This function performs interpolations of scalar

!> authors: Dr. Vikas Sharma
!
! This function performs interpolation of a scalar from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$
MODULE PURE FUNCTION stsd_interpol_scalar( Obj, Val ) RESULT(interpol)
  CLASS( STElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
    !! space-time nodal values of scalar
  REAL( DFP ), ALLOCATABLE :: Interpol( : )
    !! Interpolation value of `val` at integration points
END FUNCTION stsd_interpol_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                 STInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This function performs interpolations of vector

!> authors: Dr. Vikas Sharma
!
! This function performs interpolation of a vector from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$
MODULE PURE FUNCTION stsd_interpol_vector( Obj, Val ) RESULT(interpol)
  CLASS( STElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
    !! spatial nodal values of vector
  REAL( DFP ), ALLOCATABLE :: Interpol( :, : )
    !! Interpolation value of `val` at integration points
END FUNCTION stsd_interpol_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                 STInterpolation@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This function performs interpolations of matrix

!> authors: Dr. Vikas Sharma
!
! This function performs interpolation of a matrix from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$
MODULE PURE FUNCTION stsd_interpol_matrix( Obj, Val ) RESULT(interpol)
  CLASS( STElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, :, :, : )
    !! spatial nodal values of matrix
  REAL( DFP ), ALLOCATABLE :: Interpol( :, :, : )
    !! Interpolation value of `val` at integration points
END FUNCTION stsd_interpol_matrix
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE stsd_interpol_scalar, stsd_interpol_vector, &
    & stsd_interpol_matrix
END INTERFACE STInterpolation

!----------------------------------------------------------------------------
!                                                 getLocalGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the local gradient of a scalar

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the local gradient of a scalar from space
! nodal values
!
! $$\frac{\partial \phi }{\partial \xi_{i} } =\phi_{I} \frac{\partial N^{I}}
! {\partial \xi_{i} }$$

MODULE PURE SUBROUTINE getLocalGradient_scalar( Obj, dPhidXi, Val )
  CLASS( ElemShapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: dPhidXi( :, : )
  REAL( DFP ), INTENT( IN ) :: Val( : )
    !! Space nodal values of scalar
END SUBROUTINE getLocalGradient_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getLocalGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the local gradient of a vector

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the local gradient of a vector
MODULE PURE SUBROUTINE getLocalGradient_vector( Obj, dVdXi, Val )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: dVdXi( :, :, : )
    !! local gradient at integration points
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
    !! nodal values of vector in `xiJ` format
END SUBROUTINE getLocalGradient_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getLocalGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the local gradient of a scalar

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the local gradient of a scalar from space
! time nodal values
!
! $$\frac{\partial \phi }{\partial \xi_{i} } =\phi^{a}_{I} T_{a}\frac
!{\partial N^{I}}{\partial \xi_{i} }$$
MODULE PURE SUBROUTINE stsd_getLocalGradient_scalar( Obj, dPhidXi, Val )
  CLASS( STElemShapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: dPhidXi( :, : )
    !! local gradient of scalar
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
    !! space-time nodal values
END SUBROUTINE stsd_getLocalGradient_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getLocalGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the local gradient of a vector

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the local gradient of a vector using space-time
! nodal coordinates
MODULE PURE SUBROUTINE stsd_getLocalGradient_vector( Obj, dVdXi, Val )
  CLASS( STElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: dVdXi( :, :, : )
    !! local gradient at integration points
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
    !! space-time nodal values of vector in `xiJa` format
END SUBROUTINE stsd_getLocalGradient_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getLocalGradient
!----------------------------------------------------------------------------
INTERFACE getLocalGradient
  MODULE PROCEDURE getLocalGradient_scalar, getLocalGradient_vector, &
    & stsd_getLocalGradient_scalar, stsd_getLocalGradient_vector
END INTERFACE getLocalGradient

! PUBLIC :: getLocalGradient/

!----------------------------------------------------------------------------
!                                               getSpatialGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the spatial gradient of vector

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the spatial gradient of a vector
MODULE PURE SUBROUTINE getSpatialGradient_vector( Obj, dVdXt, Val )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: dVdXt( :, :, : )
    !! spatial gradient of `val` at integration points
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
    !! nodal values of vector in `xiJ` format
END SUBROUTINE getSpatialGradient_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                               getSpatialGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the spatial gradient of scalar

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the spatial gradient of scalar
MODULE PURE SUBROUTINE getSpatialGradient_scalar( Obj, dPhidXt, Val )
  CLASS( ElemShapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: dPhidXt( :, : )
    !! Spatial gradient of scalar
  REAL( DFP ), INTENT( IN ) :: Val( : )
    !! Nodal values of scalar
END SUBROUTINE getSpatialGradient_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                               getSpatialGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the spatial gradient of scalar

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the spatial gradient of scalar
MODULE PURE SUBROUTINE stsd_getSpatialGradient_scalar( Obj, dPhidXt, Val )
  CLASS( STElemShapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: dPhidXt( :, : )
    !! Spatial gradient of scalar
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
    !! space-time Nodal values of scalar
END SUBROUTINE stsd_getSpatialGradient_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                               getSpatialGradient@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the spatial gradient of vector

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the spatial gradient of a vector from its
! space-time nodal values
MODULE PURE SUBROUTINE stsd_getSpatialGradient_vector( Obj, dVdXt, Val )
  CLASS( STElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: dVdXt( :, :, : )
    !! spatial gradient of `val` at integration points
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
    !! space-time nodal values of vector in `xiJa` format
END SUBROUTINE stsd_getSpatialGradient_vector
END INTERFACE

INTERFACE getSpatialGradient
  MODULE PROCEDURE getSpatialGradient_scalar, getSpatialGradient_vector, &
    & stsd_getSpatialGradient_scalar, &
      & stsd_getSpatialGradient_vector
END INTERFACE getSpatialGradient

PUBLIC :: getSpatialGradient

!----------------------------------------------------------------------------
!                                            getProjectionOfdNdXt@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine computes the projection of dNdXt on a vector

!> authors: Dr. Vikas Sharma
!
! This subroutine computes the projcetion cdNdXt on the vector `Val`
! Here the vector `Val` is constant in space and time
!
! $$P^{I}=c_{i}\frac{\partial N^{I}}{\partial x_{i}} $$
!
MODULE PURE SUBROUTINE getProjectionOfdNdXt_spacevalues( Obj, cdNdXt, Val )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: cdNdXt( :, : )
    !! returned $c_{i}\frac{\partial N^{I}}{\partial x_{i}}$
  REAL( DFP ), INTENT( IN ) :: Val( : )
    !! constant value of vector
END SUBROUTINE getProjectionOfdNdXt_spacevalues
END INTERFACE

!----------------------------------------------------------------------------
!                                            getProjectionOfdNdXt@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine computes the projection of dNdXt on a vector

!> authors: Dr. Vikas Sharma
!
! This subroutine computes the projcetion cdNdXt on the vector `Val`
! Here the vector `Val` is constant in space and time
!
! $$P^{I}=c_{i}\frac{\partial N^{I}}{\partial x_{i}} $$
!
MODULE PURE SUBROUTINE getProjectionOfdNdXt_fevar( Obj, cdNdXt, Val )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: cdNdXt( :, : )
    !! returned $c_{i}\frac{\partial N^{I}}{\partial x_{i}}$
  CLASS( FEVariable_ ), INTENT( IN ) :: Val
    !! constant value of vector
END SUBROUTINE getProjectionOfdNdXt_fevar
END INTERFACE

INTERFACE getProjectionOfdNdXt
  MODULE PROCEDURE &
    & getProjectionOfdNdXt_spacevalues,&
    & getProjectionOfdNdXt_fevar
END INTERFACE getProjectionOfdNdXt

PUBLIC :: getProjectionOfdNdXt

!----------------------------------------------------------------------------
!                                            getProjectionOfdNTdXt@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine computes the projection of dNTdXt on a vector

!> authors: Dr. Vikas Sharma
!
! This subroutine computes the projcetion cdNTdXt on the vector `Val`
! Here the vector `Val` is constant in space and time
!
! $$P^{I,a}=c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$$
!
MODULE PURE SUBROUTINE getProjectionOfdNTdXt_constvector( Obj, cdNTdXt, Val )
  CLASS( STElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: cdNTdXt( :, :, : )
    !! returned $c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$
  REAL( DFP ), INTENT( IN ) :: Val( : )
    !! constant value of vector
END SUBROUTINE getProjectionOfdNTdXt_constvector
END INTERFACE

!----------------------------------------------------------------------------
!                                            getProjectionOfdNTdXt@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine computes the projection of dNTdXt on a vector

!> authors: Dr. Vikas Sharma
!
! This subroutine computes the projcetion cdNTdXt on the vector `Val`
! Here the vector `Val` is constant in space and time
!
! $$P^{I,a}=c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$$
!
MODULE PURE SUBROUTINE getProjectionOfdNTdXt_fevar( Obj, cdNTdXt, Val )
  CLASS( STElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: cdNTdXt( :, :, : )
    !! returned $c_{i}\frac{\partial N^{I} T_a}{\partial x_{i}}$
  TYPE( FEVariable_ ), INTENT( IN ) :: Val
    !! constant value of vector
END SUBROUTINE getProjectionOfdNTdXt_fevar
END INTERFACE

!----------------------------------------------------------------------------
!                                            getProjectionOfdNTdXt@getMethod
!----------------------------------------------------------------------------

INTERFACE getProjectionOfdNTdXt
  MODULE PROCEDURE &
    & getProjectionOfdNTdXt_constvector, &
    & getProjectionOfdNTdXt_fevar
END INTERFACE getProjectionOfdNTdXt

PUBLIC :: getProjectionOfdNTdXt

!----------------------------------------------------------------------------
!                                                    getUnitNormal@getMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine can be used in SUPG formulation

!> authors: Dr. Vikas Sharma
!
! This routine can be used in the SUPG formulation
!
!  $$\nabla \vert phi \vert / \vert (\nabla \vert phi \vert) \vert$$
!

MODULE PURE SUBROUTINE getUnitNormal_scalar( Obj, R, Val )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: R( :, : )
  REAL( DFP ), INTENT( IN ) :: Val( : )
END SUBROUTINE getUnitNormal_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getUnitNormal@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE getUnitNormal_vector( Obj, R, Val )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT  ) :: R( :, : )
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
END SUBROUTINE getUnitNormal_vector
END INTERFACE

INTERFACE getUnitNormal
  MODULE PROCEDURE getUnitNormal_vector, getUnitNormal_scalar
END INTERFACE getUnitNormal

PUBLIC :: getUnitNormal

!----------------------------------------------------------------------------
!                                                    Initiate@LineH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine computes the shape functions given by Lagrange polynomials !! over reference line element

!> authors: Dr. Vikas Sharma
!
! This subroutine computes shape functions, lagrange polynomials, over
! reference line element
! The interpolation functions are defined inside the reference element itself
! The order of shape functions are also included inside the refelem

MODULE PURE SUBROUTINE Line_H1_Lagrange( Obj, Quad, RefElem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
    !! Quadrature points where shapefunctions will be evaluated
  CLASS( ReferenceLine_ ), INTENT( IN ) :: RefElem
    !! Reference element where shape functions will be defined
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
    !! H1 (nodal) Continuity type
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
    !! Lagrange polynomial will be used for interpolation
END SUBROUTINE Line_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@TriangleH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine computes the shape functions given by Lagrange polynomials !! over reference triangle element

!> authors: Dr. Vikas Sharma
!
! This subroutine computes shape functions, lagrange polynomials, over
! reference triangle element

MODULE PURE SUBROUTINE Triangle_H1_Lagrange( Obj, Quad, RefElem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceTriangle_ ), INTENT( IN ) :: RefElem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Triangle_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@QuadrangleH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Quadrangle_H1_Lagrange( Obj, Quad, RefElem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceQuadrangle_ ), INTENT( IN ) :: RefElem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Quadrangle_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@TetrahedronH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Tetrahedron_H1_Lagrange( Obj, Quad, RefElem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceTetrahedron_ ), INTENT( IN ) :: RefElem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Tetrahedron_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@HexahedronH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Hexahedron_H1_Lagrange( Obj, Quad, RefElem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceHexahedron_ ), INTENT( IN ) :: RefElem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Hexahedron_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@PrismH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Prism_H1_Lagrange( Obj, Quad, RefElem, &
  & ContinuityType, InterpolType)
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferencePrism_ ), INTENT( IN ) :: RefElem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Prism_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                              Initiate@PyramidH1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Pyramid_H1_Lagrange( Obj, Quad, RefElem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferencePyramid_ ), INTENT( IN ) :: RefElem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE Pyramid_H1_Lagrange
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Initiate@H1Lagrange
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE H1_Lagrange( Obj, Quad, RefElem, &
  & ContinuityType, InterpolType )
  CLASS( ElemshapeData_ ), INTENT( INOUT ) :: Obj
  CLASS( QuadraturePoint_ ), INTENT( IN ) :: Quad
  CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
  CLASS( H1_ ), INTENT( IN ) :: ContinuityType
  CLASS( LagrangeInterpolation_ ), INTENT( IN ) :: InterpolType
END SUBROUTINE H1_Lagrange
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE H1_Lagrange
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                                   Contains
!----------------------------------------------------------------------------

END MODULE ElemshapeData_Method