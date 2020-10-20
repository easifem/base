!> authors: Dr. Vikas Sharma
!
! This module define a kernel for automatically moving the mesh and smoothing
! it, the mesh mover is based upon the elasticity method.
!
! ```fortran
! call meshmove%initiate( NSD=NSD, NNT=NNT, dt=dt )
! call meshmove%setdomain(dom=dom, omegano=[1])
! call meshmove%setMaterialProperties(matprops=meshprops)
! call meshmove%setAlgorithm(mainOption=[meshmove%elasticity])
! call meshmove%setBoundary( &
!   & xdb = [BOTTOM,RIGHT1,RIGHT2,TOP,LEFT], &
!   & ydb = [BOTTOM,RIGHT1,TOP,LEFT], &
!   & mb = [], noslip=.false. )
! call meshmove%assemble()
! call meshmove%applyDBC()
! call meshmove%solve()
! call meshmove%update(reset=.true.)
!```
MODULE MovingMesh_Class
USE EASIFEM
USE Kernel_Class
IMPLICIT NONE
PRIVATE

INTEGER( I4B ), PARAMETER :: opt_elasticity=1
INTEGER( I4B ), PARAMETER :: elasticity_lam=1
INTEGER( I4B ), PARAMETER :: elasticity_mu=2
INTEGER( I4B ), PARAMETER :: elasticity_xi=3



!----------------------------------------------------------------------------
!                                                           MovingMesh_Class
!----------------------------------------------------------------------------

TYPE, EXTENDS( Kernel_ ) :: MovingMesh_
  INTEGER(I4B) :: y=1
  INTEGER(I4B) :: rhs=2
  INTEGER( I4B ) :: Elasticity=opt_elasticity
  LOGICAL( LGT ) :: isNoSlip=.FALSE.
  REAL( DFP ) :: Alpha = 100.0

  REAL( DFP ), ALLOCATABLE :: SmoothNodes(:,:)
  LOGICAL( LGT ) :: Smoothing = .FALSE.
  INTEGER( I4B ) :: Laplace = 1

  TYPE( QuadratureVariables_ ) :: mesh_quality
  REAL( DFP ) :: fA=0.0_DFP, fAR=0.0_DFP

    !! Penalty parameter for nitsche method
  TYPE( LagrangeInterpolation_ ) :: InterpolType
    !! Interpolation type of shape functions
  TYPE( H1_ ) :: ContinuityType
    !! Continuity type of shape functions

  PROCEDURE( mmt_apply_initcond ), POINTER, PASS(Obj) :: &
    & applyInitCondition=>NULL()
  PROCEDURE( mmt_applyuniformdisp ), POINTER, PASS(Obj) :: &
    & applyUniformDisplacement=>NULL()
  PROCEDURE(apply_nitsche_elasticity), POINTER :: applyWeakDBC=>NULL()
  PROCEDURE(apply_dbc_elasticity), POINTER :: applyDBC=>NULL()

  CONTAINS
    PROCEDURE, PUBLIC, PASS( Obj ) :: setMaterialProperties => set_matprops
    PROCEDURE, PUBLIC, PASS( Obj ) :: setBoundary => mmt_setbc
    PROCEDURE, PUBLIC, PASS( Obj ) :: setAlgorithm => mmt_setalgo
    PROCEDURE, PUBLIC, PASS( Obj ) :: smooth => mmt_smooth
    PROCEDURE, PUBLIC, PASS( Obj ) :: getMeshQuality => mmt_getMeshQuality
END TYPE MovingMesh_

PUBLIC :: MovingMesh_

TYPE :: MovingMeshPointer_
  CLASS( MovingMesh_ ), POINTER :: Ptr => NULL( )
END TYPE MovingMeshPointer_

PUBLIC :: MovingMeshPointer_

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

MODULE SUBROUTINE set_matprops( Obj, matprops )
  CLASS( MovingMesh_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: matprops(:,:)
END SUBROUTINE set_matprops
END INTERFACE

!----------------------------------------------------------------------------
!                                                   setBoundary@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mmt_setbc( obj, xdb, ydb, zdb, mb, isNoSlip )
  CLASS( MovingMesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: xdb( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ydb( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: zdb( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: mb( : )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isNoSlip
END SUBROUTINE mmt_setbc
END INTERFACE

!----------------------------------------------------------------------------
!                                                   setAlgorithm@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mmt_setalgo( Obj, mainOption, extraOption )
  CLASS( MovingMesh_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: mainOption( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: extraOption( : )
END SUBROUTINE mmt_setalgo
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getMeshQuality@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mmt_getMeshQuality( Obj, qmin, qmax, qavg, measure, q, &
  & nodes )
  CLASS( MovingMesh_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: measure
  REAL( DFP ), INTENT( INOUT ) :: qmin
  REAL( DFP ), INTENT( INOUT ) :: qmax
  REAL( DFP ), INTENT( INOUT ) :: qavg
  REAL( DFP ), ALLOCATABLE, OPTIONAL, INTENT( INOUT ) :: q( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: nodes( :, : )
END SUBROUTINE mmt_getMeshQuality
END INTERFACE

!----------------------------------------------------------------------------
!                                                    setDOF@ElasticityMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mmt_setdof_elasticity( Obj )
  CLASS( MovingMesh_ ), INTENT( INOUT) :: Obj
END SUBROUTINE mmt_setdof_elasticity
END INTERFACE

!----------------------------------------------------------------------------
!                                        applyInitCondition@ElasticityMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE mmt_apply_initcond( Obj, initVal )
  IMPORT :: MovingMesh_, DFP
  CLASS( MovingMesh_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: initVal( : )
END SUBROUTINE mmt_apply_initcond
END INTERFACE

INTERFACE
MODULE SUBROUTINE mmt_apply_initcond_elasticity( Obj, initVal )
  CLASS( MovingMesh_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: initVal( : )
END SUBROUTINE mmt_apply_initcond_elasticity
END INTERFACE

!----------------------------------------------------------------------------
!                                          applyDBCondition@ElasticityMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE mmt_applyuniformdisp( Obj, tag, Val, dim )
  IMPORT :: MovingMesh_, I4B, DFP
  CLASS(MovingMesh_), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tag
  REAL(DFP), INTENT( IN ) :: Val
  INTEGER(I4B), INTENT( IN ) :: dim
END SUBROUTINE mmt_applyuniformdisp
END INTERFACE

INTERFACE
MODULE SUBROUTINE mmt_applyuniformdisp_elasticity( Obj, tag, Val, dim )
  CLASS( MovingMesh_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tag
  REAL( DFP ), INTENT( IN ) :: Val
  INTEGER( I4B ), INTENT( IN ) :: dim
END SUBROUTINE mmt_applyuniformdisp_elasticity
END INTERFACE

!----------------------------------------------------------------------------
!                                              applyNitscheBoundaryCondition
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE apply_nitsche_elasticity( Obj, tag, y, dt)
  CLASS( MovingMesh_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: y(:), dt
  INTEGER(I4B), INTENT( IN ) :: tag( : )
END SUBROUTINE apply_nitsche_elasticity
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   applyDBC
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE apply_dbc_elasticity( Obj, tag, y,dt, dim, lb, ub, &
  & local_nptrs, magnitude )
  CLASS( MovingMesh_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: y(:), dt
  INTEGER(I4B), INTENT( IN ) :: tag( : ), dim, lb, ub
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: local_nptrs(lb:ub)
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: magnitude
END SUBROUTINE apply_dbc_elasticity
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    Assemble
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mmt_assemble_elasticity( Obj )
  CLASS( Kernel_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE mmt_assemble_elasticity
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Solve
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mmt_solve_elasticity( Obj )
  CLASS( Kernel_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE mmt_solve_elasticity
END INTERFACE

!----------------------------------------------------------------------------
!                                                               isConverged
!----------------------------------------------------------------------------
INTERFACE
MODULE FUNCTION mmt_isconverg_elasticity( Obj, relTol, abstol, &
  & convergeInRes, convergeInSol ) RESULT( Ans )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: relTol
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: absTol
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: convergeInRes
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: convergeInSol
  LOGICAL( LGT ) :: Ans
END FUNCTION mmt_isconverg_elasticity
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Update@ElasticityMethod
!----------------------------------------------------------------------------

INTERFACE
!! This method update the kernel

!> authors: Dr. Vikas Sharma
!
! This method update the kernel.
! This subroutine will update the nodal coordinates and nodal velocity
! If `reset` option is true then the solution `y` are set to zero after
! updating the nodal velocity and nodal coordinates of mesh

MODULE SUBROUTINE mmt_update_elasticity( Obj, reset )
  CLASS( Kernel_ ), INTENT( INOUT ) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: reset
END SUBROUTINE mmt_update_elasticity
END INTERFACE

!----------------------------------------------------------------------------
!                                                 WriteData@ElasticityMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mmt_writedata_elasticity(Obj,gmsh, path,filename,extension,&
  & indx)
  CLASS( Kernel_ ), INTENT(INOUT) :: Obj
  TYPE(Gmsh_), OPTIONAL, INTENT(INOUT) :: gmsh
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: path, filename, extension
  INTEGER( I4B ), INTENT( IN ) :: indx(:)
END SUBROUTINE mmt_writedata_elasticity
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Smooth@Smooth
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mmt_smooth( Obj,maxIter,omegano, boundary,mainOption,&
  & extraOption )
  CLASS( MovingMesh_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: maxiter
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: omegano(:), boundary(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: mainOption(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: extraOption(:)
END SUBROUTINE mmt_smooth
END INTERFACE

END MODULE MovingMesh_Class
