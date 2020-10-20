MODULE Seepage_Class
USE EASIFEM
USE Kernel_Class
IMPLICIT NONE
PRIVATE

#include "../Kernel_Def.inc"

INTEGER( I4B ) :: i_Ss=4, i_kx=1, i_ky=2, i_kz=3, i_kd=5
CLASS( Mesh_ ), POINTER :: MeshObj
CLASS( Element_ ), POINTER :: Elem
TYPE( DOF_ ) :: rhs_dof, vn_dof
REAL( DFP ), ALLOCATABLE :: rhs(:)
TYPE(RealMatrix_) :: rmat(10)
TYPE(RealVector_) :: rvec(10)
REAL( DFP ) :: ks( 3, 3 ) = 0.0_DFP
TYPE(QuadraturePoint_) :: spaceQuad, timeQuad
TYPE(ElemShapeData_) :: spacesd, timesd
TYPE( ReferenceLine_ ) :: TimeElem
TYPE( STElemShapeData_ ), ALLOCATABLE :: stsd( : )
TYPE( fevariable_ ) :: diff_coeff
REAL( DFP ), PARAMETER :: ACC_GRAVITY=9.81
INTEGER( I4B ), ALLOCATABLE :: nptrs(:)

!----------------------------------------------------------------------------
!                                                                   Seepage_
!----------------------------------------------------------------------------

TYPE, EXTENDS( Kernel_ ) :: Seepage_

  INTEGER( I4B ) :: SPACETIME=ALGO_OPT_SPACETIME
    !! main option for algorithm
  INTEGER( I4B ) :: SEMIDISCRETE=ALGO_OPT_SEMIDISCRETE
    !! main option for algorithm
  INTEGER( I4B ) :: p, rhs, p0, v, dpdt, p_trial, v_rhs, pw
    !! ID for nodal variables
  INTEGER( I4B ) :: hydrostat=0

  TYPE( QuadratureVariables_ ) :: ks
    !! permeability
  TYPE( QuadratureVariables_ ) :: Ss
    !! Specific storage
  REAL( DFP ) :: rho_w=1000.0_DFP
  REAL( DFP ) :: mu_w = 0.001_DFP

  LOGICAL(LGT) :: gravity=.TRUE.
  TYPE( LagrangeInterpolation_ ) :: TimeInterpol
  TYPE( LagrangeInterpolation_ ) :: SpaceInterpol
  TYPE( H1_ ) :: TimeContinuity
  TYPE( H1_ ) :: SpaceContinuity

  PROCEDURE(hydrostat_userfunc), POINTER, NOPASS :: &
    & PressureFunc => NULL()
  PROCEDURE( seepage_apply_initcond_spacetime ), POINTER, PASS(Obj) :: &
    & applyInitCondition=>NULL()
  PROCEDURE( seepage_apply_dbc_spacetime ), POINTER, PASS(Obj) :: &
    & applyDBC => NULL()
  PROCEDURE( seepage_velocity_explicit_spacetime ), POINTER, PASS(Obj) :: &
    & getVelocity=>NULL()
  PROCEDURE( seepage_nbc_spacetime ), POINTER, PASS( Obj ) :: &
    & applyNBC => NULL( )

  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: setAlgorithm => seepage_setalgo
    !! Extension
  PROCEDURE, PUBLIC, PASS( Obj ) :: setMaterialProperties=>set_matprops
    !! Extension
  PROCEDURE, PUBLIC, PASS( Obj ) :: setPermeability => set_perm
    !! Original
  PROCEDURE, PUBLIC, PASS( Obj ) :: setSpecificStorage => set_ss
    !! Original
  PROCEDURE, PUBLIC, PASS( Obj ) :: setBoundary => seepage_setbc
    !! Original
  PROCEDURE, PUBLIC, PASS( Obj ) :: getNormalVelocity => seepage_norm_vel
    !! Original
END TYPE Seepage_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: Seepage_

TYPE :: SeepagePointer_
  CLASS( Seepage_ ), POINTER :: Ptr => NULL( )
END TYPE SeepagePointer_

PUBLIC :: SeepagePointer_

!----------------------------------------------------------------------------
!                                                   setAlgorithm@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine sets the algorithm type of seepage kernel

!> authors: Dr. Vikas Sharma
!
! This subroutine sets the algorithm type of seepage kernel
! `mainOption` can have following values
! - `obj%STATIC` in which case we solve a static problem
! - `obj%SPACETIME` in which case we solve a transient problem by using
! space-time fem
! - `obj%SEMIDISCRETE` in which case we solve a transient problem by using
! discrete FEM

MODULE SUBROUTINE seepage_setalgo( Obj, mainOption, extraOption )
  CLASS( Seepage_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: mainOption( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: extraOption( : )
END SUBROUTINE seepage_setalgo
END INTERFACE

!----------------------------------------------------------------------------
!                                          setMaterialProperties@Constructor
!----------------------------------------------------------------------------
INTERFACE
!! This subroutine set the material properties

!> authors: Dr. Vikas Sharma
!
! This subroutine set the material properties (permeability of the medium,
! ans specific storage coefficient)
! - `matprops( :, iprop )` denotes the material properties of `iprop` element
! - material properties are stored as follows; Ss, kx, ky, kz
MODULE SUBROUTINE set_matprops( Obj, matprops )
  CLASS( Seepage_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: matprops(:,:)
END SUBROUTINE set_matprops
END INTERFACE

!----------------------------------------------------------------------------
!                                                 setPermeability@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the permeability of the medium

!> authors: Dr. Vikas Sharma
!
!  This subroutine set the permeability of the medium
! - kx, ky, kz should be provided
! - `ValFromMatType(:,:)` denotes the permeability of each material type; in
!   this case material properties of each element is imposed by using the
!   the material type id of an element

MODULE SUBROUTINE set_perm( Obj, ValFromMatType )
  CLASS( Seepage_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: ValFromMatType( :, : )
    !! Row1, Row2, Row3 corresponds to kx, ky, kz
END SUBROUTINE set_perm
END INTERFACE

!----------------------------------------------------------------------------
!                                              setSpecificStorage@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the permeability of the medium

!> authors: Dr. Vikas Sharma
!
!  This subroutine set the specific storage of the medium
! - kx, ky, kz should be provided
! - `ValFromMatType(:,:)` denotes the permeability of each material type; in
!   this case material properties of each element is imposed by using the
!   the material type id of an element

MODULE SUBROUTINE set_ss( Obj, ValFromMatType )
  CLASS( Seepage_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: ValFromMatType( :, : )
    !! Row1 corresponds to ss
END SUBROUTINE set_ss
END INTERFACE

!----------------------------------------------------------------------------
!                                                    setBoundary@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine sets the boundary condition

!> authors: Dr. Vikas Sharma
!
! This subroutine sets the boundary conditions to the seepage kernel
! Information of Dirichlet boundary and  Robin boundary is necessary during
! setting up the problem; Robine type boundary condition is useful incase of ! drainage layer.

MODULE SUBROUTINE seepage_setbc( Obj, DB, robinB )
  CLASS( Seepage_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: DB( : )
    !! Dirichlet boundary condition
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: robinB( : )
    !! Robin boundary condition
END SUBROUTINE seepage_setbc
END INTERFACE

!----------------------------------------------------------------------------
!                                              getNormalVelocity@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine computes the normal velocity at the nodal points

!> authors: Dr. Vikas Sharma
! 	This subroutine computes the normal velocity at the nodal points. The !
!   process is described below
!   - First we solve following equation to get normal at the nodal points
!  $$\left[ \int_{\Gamma } N^{I}N^{J}ds\right]  \left\{ \mathbf{n}_{J}
!     \right\} =\int_{\Gamma } N^{I}\mathbf{n} ds$$
!   - Then we use
! $\mathbf{v}_n = \mathbf{v} \cdot \mathbf{n} \mathbf{n}$
MODULE SUBROUTINE seepage_norm_vel( Obj, tag, Vn, local_nptrs, Angle, &
  & SaveData )
  CLASS( Seepage_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tag(:)
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT) :: Vn( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT) :: local_nptrs( : )
  REAL( DFP ), INTENT (OUT) :: Angle
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: SaveData
END SUBROUTINE seepage_norm_vel
END INTERFACE

!----------------------------------------------------------------------------
!                                                    setDOF@SpaceTimeMethod
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the degrees of freedom in the kernel

!> authors: Dr. Vikas Sharma
!
! This subroutine sets the degrees of freedom in the kernel
! This subroutine is internally called by the kernel when we call
! `setAlgorithm()` subroutine

MODULE SUBROUTINE seepage_setdof_spaceTime( Obj )
  CLASS( Seepage_ ), INTENT( INOUT) :: Obj
END SUBROUTINE seepage_setdof_spaceTime
END INTERFACE

!----------------------------------------------------------------------------
!                                              applyInitCondition@SpaceTime
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  PURE FUNCTION hydrostat_userfunc( x ) RESULT( Ans )
    IMPORT :: DFP
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: x( : )
    REAL( DFP ) :: Ans
  END FUNCTION hydrostat_userfunc
END INTERFACE

INTERFACE
!! This subroutine applies initial condition to the [[seepage_]] kernel

!> authors: Dr. Vikas Sharma
!
! This subroutine apply initial conditions to the seepage kernel
! If `Val` is not present then function pointer `PressureFunc` is used
! If `Val` is present then initial values are taken from `Val`
! If `local_nptrs` are present then we use following mapping
! Obj%p0(i) = initVal(local_nptrs(nptrs(i)))
! If `filename` is present then initial conditions are read from the file
MODULE SUBROUTINE seepage_apply_initcond_spacetime( Obj, Val, local_nptrs,&
  & path, filename, extension)
  CLASS( Seepage_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Val( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: local_nptrs(:)
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: path
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: filename
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: extension
END SUBROUTINE seepage_apply_initcond_spacetime
END INTERFACE

!----------------------------------------------------------------------------
!                                                  applyDBC@SpaceTimeMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine applies boundary condition to the seepage kernel

!> authors: Dr. Vikas Sharma
!
! This subroutine apply boundary condition to `obj%p` in the seepage kernel
! If `Val` is not present then `local_nptrs` is not requried; in that case,
! procedure pointer `PressureFunc` is used
! If `Val` is present then `local_nptrs` may or maynot be present
MODULE SUBROUTINE seepage_apply_dbc_spacetime( Obj, tag, Val, local_nptrs )
  CLASS( Seepage_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tag
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Val(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: local_nptrs(:)
END SUBROUTINE seepage_apply_dbc_spacetime
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ApplyNBC@SpaceTimeMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine apply neuman boundary condition in the seepage kernel

!> authors: Dr. Vikas Sharma
!
! This subroutine apply Neuman boundary condition in the seepage kernel
!
MODULE SUBROUTINE seepage_nbc_spacetime( Obj, tag, UniformSteady, &
  & UniformTransient, NonUniformSteady, NonUniformTransient )
  CLASS( Seepage_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tag
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: UniformSteady
    !! Uniform Value
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: UniformTransient( : )
    !! Time varying value, in space it is constant
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: NonUniformSteady( : )
    !! Varying only in space, remains constant in time
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: NonUniformTransient( :, : )
    !! Varying in space and time
END SUBROUTINE seepage_nbc_spacetime
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Assemble@SpacetimeMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine assemble the system of linear equation for seepage kernel

!> authors: Dr. Vikas Sharma
!
! This subroutine assemble the system of linear equations for seepage kernel
! If robin boundary conditions are specified then the element
! matrices corresponding to robin boundary will be added to the tangent matrix

MODULE SUBROUTINE seepage_assemble_spacetime( Obj )
  CLASS( Kernel_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE seepage_assemble_spacetime
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Solve@SpacetimeMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE seepage_solve_spacetime( Obj )
  CLASS( Kernel_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE seepage_solve_spacetime
END INTERFACE

!----------------------------------------------------------------------------
!                                                               isConverged
!----------------------------------------------------------------------------
INTERFACE
MODULE FUNCTION seepage_isconverg_spacetime( Obj, relTol, abstol, &
  & convergeInRes, convergeInSol ) RESULT( Ans )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: relTol
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: absTol
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: convergeInRes
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: convergeInSol
  LOGICAL( LGT ) :: Ans
END FUNCTION seepage_isconverg_spacetime
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Update@SpaceTimeMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE seepage_update_spacetime( Obj, reset )
  CLASS( Kernel_ ), INTENT( INOUT ) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: reset
END SUBROUTINE seepage_update_spacetime
END INTERFACE

!----------------------------------------------------------------------------
!                                           SeepageVelocity@SpaceTimeMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE seepage_velocity_explicit_spacetime( Obj )
  CLASS( Seepage_ ), INTENT( INOUT) :: Obj
END SUBROUTINE seepage_velocity_explicit_spacetime
END INTERFACE

!----------------------------------------------------------------------------
!                                                 WriteData@SpaceTimeMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE seepage_writedata_spacetime(Obj, gmsh, path, filename, &
  & extension, indx)
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  TYPE(gmsh_), OPTIONAL, INTENT( INOUT) :: gmsh
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: path, filename, extension
  INTEGER( I4B ), INTENT( IN ) :: indx(:)
END SUBROUTINE seepage_writedata_spacetime
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SaveState@SpaceTimeMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE savestate_spacetime( Obj, Path, Filename, Extension, &
  & Timestep )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Path
  CHARACTER( LEN = * ), INTENT( IN ) :: Filename
  CHARACTER( LEN = * ), INTENT( IN ) :: Extension
  INTEGER( I4B ), INTENT( IN ) :: Timestep
END SUBROUTINE savestate_spacetime
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setDOF@StaticMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the degrees of freedom

!> authors: Dr. Vikas Sharma
!
! This subroutine sets the degrees of freedom in seepage kernel
! In case of a static problem there are only two degrees of freedom;
! Total pressure P and RHS
MODULE SUBROUTINE seepage_setdof_static( Obj )
  CLASS( Seepage_ ), INTENT( INOUT) :: Obj
END SUBROUTINE seepage_setdof_static
END INTERFACE

!----------------------------------------------------------------------------
!                                  applyDBC@StaticMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine apply Dirichlet boundary condition

!> authors: Dr. Vikas Sharma
!
! This subroutine applies Dirichlet boundary condition to the seepage kernel
! - tag is id of boundary
! - Val is prescribed values at the nodes;
! - If SIZE(Val) == 1, then it represents homogenenous values, in this case
!   local_nptrs are not needed
! - If SIZE(Val) .NE. 1, then if local_nptrs are present then it is used to
!   extract values from Val.
! - If Val is not present then `PressureFunc` is used

MODULE SUBROUTINE seepage_apply_dbc_static( Obj, tag, Val, local_nptrs )
  CLASS( Seepage_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tag
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Val(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: local_nptrs(:)
END SUBROUTINE seepage_apply_dbc_static
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Assemble@StaticMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine assembles the tangent matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine assembles the tangent matrix

MODULE SUBROUTINE seepage_assemble_static( Obj )
  CLASS( Kernel_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE seepage_assemble_static
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Solve@StaticMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE seepage_solve_static( Obj )
  CLASS( Kernel_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE seepage_solve_static
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isConverged@StaticMethods
!----------------------------------------------------------------------------
INTERFACE
MODULE FUNCTION seepage_isconverg_static( Obj, relTol, abstol, &
  & convergeInRes, convergeInSol ) RESULT( Ans )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: relTol
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: absTol
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: convergeInRes
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: convergeInSol
  LOGICAL( LGT ) :: Ans
END FUNCTION seepage_isconverg_static
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Update@StaticMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE seepage_update_static( Obj, reset )
  CLASS( Kernel_ ), INTENT( INOUT ) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: reset
END SUBROUTINE seepage_update_static
END INTERFACE

!----------------------------------------------------------------------------
!                                                   WriteData@StaticMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE seepage_writedata_static(Obj, gmsh, path, filename, &
  & extension, indx)
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  TYPE(gmsh_), OPTIONAL, INTENT( INOUT) :: gmsh
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: path, filename, extension
  INTEGER( I4B ), INTENT( IN ) :: indx(:)
END SUBROUTINE seepage_writedata_static
END INTERFACE

END MODULE Seepage_Class