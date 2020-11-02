MODULE Kernel_Class
USE EASIFEM
IMPLICIT NONE
PRIVATE

#include "./Kernel_Def.inc"

!----------------------------------------------------------------------------
!                                                                   Kernel_
!----------------------------------------------------------------------------

TYPE :: Kernel_
  INTEGER( I4B ) :: STATIC= ALGO_OPT_STATIC
  INTEGER( I4B ) :: TRANSIENT = ALGO_OPT_TRANSIENT

  INTEGER( I4B ) :: NSD=2
    !! Spatial dimension of problem
  INTEGER( I4B ) :: NNT=2
    !! number of nodes in time finite element method
  INTEGER( I4B ) :: tNODES=0
    !! total number of nodes in problem
  INTEGER( I4B ) :: tELEMENTS=0
    !! total elements
  INTEGER( I4B ) :: tDOF=0
    !! total number of degrees of freedom per node
  INTEGER( I4B ) :: SpatialCoordType=0
    !! Spatial coordinate type
  INTEGER( I4B ) :: OneD_Horizontal=NSD_1_HORIZONTAL
  INTEGER( I4B ) :: OneD_Vertical=NSD_1_VERTICAL
  INTEGER( I4B ) :: TwoD_Cartesian=NSD_2_CARTESIAN
  INTEGER( I4B ) :: TwoD_AxiSymmetric=NSD_2_AXISYMMETRIC
  INTEGER( I4B ) :: ThreeD=NSD_3

  INTEGER( I4B ), ALLOCATABLE :: OmegaNoToMaterials( : )
    !! Mesh region to material mapping
  INTEGER( I4B ) :: tMaterials=1
    !! Total number of materials

  REAL( DFP ) :: dt = 0.01_DFP, tn = 0.0_DFP, err=0.0_DFP, err0=0.0_DFP
  REAL( DFP ) :: err_res = 0.0_DFP, err0_res = 0.0_DFP
  REAL( DFP ) :: tol_res = 1.0E-5, tol_sol = 1.0E-5
    !! Time step for solving dynamic/ transient problems

  REAL( DFP ), ALLOCATABLE :: matprops( :, : )
    !! material properties

  INTEGER( I4B ) :: ITER = 0
  INTEGER( I4B ) :: MAXITER=100

  INTEGER( I4B ) :: ITS = 0
  INTEGER( I4B ) :: NTS = 0

    !! maximum iteration number for linear iterative solver
  INTEGER( I4B ) :: solverName = LIS_CG
    !! linear iterative solver name
  REAL( DFP ) :: TOL=1.0E-5
    !! tolerance for linear iterative solver
  INTEGER( I4B ) :: LIS_IPAR(LIS_IPAR_LEN)=0
    !! integer parameters for linear iterative solver
  REAL( DFP ) :: LIS_FPAR(LIS_FPAR_LEN)=0.0
    !! real parameters for linear iterative solver
  INTEGER( I4B ) :: precondType = P_ILUD
    !! linear iterative solver preconditioning
  INTEGER( I4B ) :: PRECOND_IPAR(LIS_PRECOND_IPAR_LEN)=0
    !! integer parameters for linear iterative solver
  REAL( DFP ) :: PRECOND_FPAR(LIS_PRECOND_FPAR_LEN)=0.0
    !! real parameter for linear iterative solver

  INTEGER( I4B ), ALLOCATABLE :: OmegaNo(:)
    !! Id of cell mesh in domain
  INTEGER( I4B ), ALLOCATABLE :: DBCinfo(:)
    !! Boundary info
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
    !! Global Nptrs
  INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )
    !! Local Nptrs

  TYPE( IntVector_ ), ALLOCATABLE :: DB(:)
    !! DB(1) uniform pressure DB
    !! DB(2) hydrostate pressure DB
    !! DB(3) uniform discharge/flux NB
    !! DB(4) drainage/ filter / robin/ boundary NB
  TYPE( RealVector_ ), ALLOCATABLE :: nodalVar( : )
    !! nodalVar( 1 ) :: pressure
    !! nodalVar( 2 ) :: right hand side (residual)
    !! nodalVar( 3 ) :: velocity
  TYPE( DOF_ ), ALLOCATABLE :: dof( : )
    !! dof( 1 ) :: pressure
    !! dof( 2 ) :: right hand side (residual)
    !! dof( 3 ) :: velocity

  TYPE( IntVector_ ), ALLOCATABLE :: intvec( : )

  CLASS( Domain_ ), POINTER :: Dom => NULL( )
  CLASS( SparseMatrix_ ), POINTER :: tanmat => NULL( )
  CLASS( LinSolver_ ), POINTER :: linsol => NULL( )

  REAL( DFP ), ALLOCATABLE :: meshq(:)
  ! TYPE( QuadratureVariables_ ) :: meshq
  !! Mesh quality

  PROCEDURE( kernel_set_kernel ), POINTER, PASS( Obj ) :: setKernel => NULL()
  PROCEDURE( kernel_assemble ), POINTER, PASS(Obj) :: Assemble=>NULL()
  PROCEDURE( kernel_assemble ), POINTER, PASS(Obj) :: AssembleTanMat=>NULL()
  PROCEDURE( kernel_assemble ), POINTER, PASS(Obj) :: AssembleRHS=>NULL()
  PROCEDURE( kernel_solve ), POINTER, PASS(Obj) :: Solve=>NULL()
  PROCEDURE( kernel_update ), POINTER, PASS(Obj) :: Update=>NULL()
  PROCEDURE( kernel_isconverg ), POINTER, PASS(Obj) :: isConverged=>NULL()
  PROCEDURE( kernel_writedata ), POINTER, PASS(Obj) :: &
    & WriteData => NULL()
  PROCEDURE( kernel_savestate), POINTER, PASS( Obj ) :: saveState => NULL()

  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: Initiate => kernel_init
  PROCEDURE, PUBLIC, PASS( Obj ) :: SetDomain => kernel_setdomain
  PROCEDURE, PUBLIC, PASS( Obj ) :: setAlgorithm=>kernel_setalgo
  PROCEDURE, PUBLIC, PASS( Obj ) :: setLinearSolver=>kernel_setlinsol
  PROCEDURE, PUBLIC, PASS( Obj ) :: setPrecondition=>kernel_setprecond
  PROCEDURE, PUBLIC, PASS( Obj ) :: setTanMat=>kernel_settanmat
  PROCEDURE, PUBLIC, PASS( Obj ) :: Finalize => kernel_dealloc
  PROCEDURE, PUBLIC, PASS( Obj ) :: getMeshQuality => kernel_mesh_quality
  PROCEDURE, PUBLIC, PASS( Obj ) :: setTotalMaterials => set_total_materials
  PROCEDURE, PUBLIC, PASS( Obj ) :: setMaterial => set_material
END TYPE Kernel_

PUBLIC :: Kernel_

TYPE :: KernelPointer_
  CLASS( Kernel_ ), POINTER :: Ptr => NULL( )
END TYPE KernelPointer_

PUBLIC :: KernelPointer_

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine initiate the Kernel

!> authors: Dr. Vikas Sharma
!
! This subroutine initiate the kernel
! - If `NNT` (num of nodes in time domain) is not given then `NNT=1`
! - `NSD` is the spatial dimension of the problem
! - `dt` is the time step size; default value is '1.0'
MODULE SUBROUTINE kernel_init( Obj, nsd, nnt, dt, SpatialCoordType, tn, &
  & NTS, tol_res, tol_sol )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: nsd
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nnt
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: dt
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: SpatialCoordType
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: NTS
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tn
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol_res
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol_sol
END SUBROUTINE kernel_init
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SetDomain
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the domain to the kernel

!> authors: Dr. Vikas Sharma
!
! This subroutine set the domain to the kernel
! - Here `dom` is the `domain_` datatype
! - `omegaNo` is the IDs of regions which is required in the analysis
! - This subroutine will store information of `omegano`
! - It also prepares the mapping between local and global node numbers
! - Information such as, `obj%tnodes` and `obj%telements` are also stored
! inside the kernel
!
! Make sure that
! - `dom%omega` is allocated
! - `dom%nodes` is associated
! - `dom%omegano` contains valid pointer to elements of `dom%omega(:)`
! - `dom%omegano` contains

MODULE SUBROUTINE kernel_setdomain( Obj, dom, omegaNo )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  CLASS( Domain_ ), TARGET, INTENT( INOUT) :: dom
  INTEGER( I4B ), INTENT( IN ) :: omegaNo(:)
END SUBROUTINE kernel_setdomain
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setMaterialProperties
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the material propeties in the kernel

!> authors: Dr. Vikas Sharma
!
! This subroutine set the material properties in the kernel
! `matprops` contains information about different materials
! Each column of `matprops` denote a material type
! This method can be extended by the other kernels

MODULE SUBROUTINE kernel_setmatprops( Obj, matprops )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: matprops(:,:)
END SUBROUTINE kernel_setmatprops
END INTERFACE

!----------------------------------------------------------------------------
!                                                              setAlgorithm
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine set the algorithm for the kernel

!> authors: Dr. Vikas Sharma
!
! This subroutine set the algorithm of the Kernel
! This subroutine should be defined by specific kernel

MODULE SUBROUTINE kernel_setalgo( Obj, mainOption, extraOption  )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: mainOption( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: extraOption( : )
END SUBROUTINE kernel_setalgo
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 isConverge
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
FUNCTION kernel_isconverg(Obj, relTol, abstol, convergeInRes, convergeInSol) &
  & RESULT(Ans)
  IMPORT :: Kernel_, DFP, LGT
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: relTol
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: absTol
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: convergeInRes
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: convergeInSol
  LOGICAL( LGT ) :: Ans
END FUNCTION kernel_isconverg
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 setKernel
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE kernel_set_kernel( Obj )
  IMPORT :: Kernel_
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
END SUBROUTINE kernel_set_kernel
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Assemble
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE kernel_assemble( Obj )
  IMPORT :: Kernel_
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
END SUBROUTINE kernel_assemble
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Solve
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE kernel_solve( Obj )
  IMPORT :: Kernel_
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
END SUBROUTINE kernel_solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Update
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE kernel_update( Obj, reset )
  IMPORT :: Kernel_, LGT
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: reset
END SUBROUTINE kernel_update
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE kernel_writedata(Obj, gmsh, path,filename,extension, indx)
  IMPORT :: Kernel_, I4B, gmsh_
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  TYPE(gmsh_), OPTIONAL, INTENT( INOUT) :: gmsh
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: path, filename, extension
  INTEGER( I4B ), INTENT( IN ) :: indx(:)
END SUBROUTINE kernel_writedata
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SaveState
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE kernel_savestate(Obj, Path, Filename, Extension, timestep)
  IMPORT :: Kernel_, I4B
  CLASS( Kernel_ ), INTENT( INOUT ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Path
  CHARACTER( LEN = * ), INTENT( IN ) :: Filename
  CHARACTER( LEN = * ), INTENT( IN ) :: Extension
  INTEGER( I4B ), INTENT( IN ) :: timestep
END SUBROUTINE kernel_savestate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            setLinearSolver
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE kernel_setlinsol( Obj, name, maxiter, tol, fpar, ipar )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: name
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: maxiter
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ipar( : )
  REAL( DFP ) , OPTIONAL, INTENT( IN ) :: tol
  REAL( DFP ) , OPTIONAL, INTENT( IN ) :: fpar( : )
END SUBROUTINE kernel_setlinsol
END INTERFACE

!----------------------------------------------------------------------------
!                                                            setLinearSolver
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE kernel_setprecond( Obj, name, fpar, ipar )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: name
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ipar( : )
  REAL( DFP ) , OPTIONAL, INTENT( IN ) :: fpar( : )
END SUBROUTINE kernel_setprecond
END INTERFACE

!----------------------------------------------------------------------------
!                                                           setTangentMatrix
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE kernel_settanmat( Obj )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
END SUBROUTINE kernel_settanmat
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Finalize
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE kernel_dealloc( Obj )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
END SUBROUTINE kernel_dealloc
END INTERFACE

!----------------------------------------------------------------------------
!                                                            getMeshQuality
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine computes the mesh quality

!> authors: Dr. Vikas Sharma
!
! This subroutine computes the mesh quality
! - if `q` is present then mesh-quality of each element will be returned in it
!   otherwise meshquality will be stored in `obj%meshq`
! - `qmin` `qmax` and `qavg` are statistical parameters
! -`measure` is quality measure which can be
!     - meshquality%area
!     - meshquality%minAngle
!     - meshquality%maxAngle
!     - meshquality%angleRatio
!     - meshquality%radiusRatio
!     - meshquality%edgeRatio
!     - meshquality%aspectRatio
!     - meshquality%scaledJacobian
! - if `nodes` are not present then `dom%nodes` are used

MODULE SUBROUTINE kernel_mesh_quality( Obj, qmin, qmax, qavg, measure, q, &
  & nodes )
  CLASS( Kernel_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: measure
  REAL( DFP ), INTENT( INOUT ) :: qmin
  REAL( DFP ), INTENT( INOUT ) :: qmax
  REAL( DFP ), INTENT( INOUT ) :: qavg
  REAL( DFP ), ALLOCATABLE, OPTIONAL, INTENT( INOUT ) :: q( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: nodes( :, : )
END SUBROUTINE kernel_mesh_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                              setTotalMaterials@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE set_total_materials( Obj, tMaterials )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tMaterials
END SUBROUTINE set_total_materials
END INTERFACE

!----------------------------------------------------------------------------
!                                                    setMaterial@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE set_material( Obj, materialNo )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ),INTENT( IN ) :: materialNo(:)
END SUBROUTINE set_material
END INTERFACE


END MODULE Kernel_Class