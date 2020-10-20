MODULE HeatTransferPM_Class
USE EASIFEM
USE Kernel_Class
IMPLICIT NONE
PRIVATE

#include "../Kernel_Def.inc"

CLASS( Mesh_ ), POINTER :: MeshObj
CLASS( Element_ ), POINTER :: Elem
TYPE(QuadraturePoint_) :: spaceQuad
TYPE(QuadraturePoint_) :: spaceQuad_A
TYPE(QuadraturePoint_) :: spaceQuad_B
TYPE(QuadraturePoint_) :: spaceQuad_C
TYPE(QuadraturePoint_) :: spaceQuad_D
TYPE( ElemShapeData_ ) :: spacesd
TYPE( ElemShapeData_ ) :: spacesd_A
TYPE( ElemShapeData_ ) :: spacesd_B
TYPE( ElemShapeData_ ) :: spacesd_C
TYPE( ElemShapeData_ ) :: spacesd_D
TYPE( FEVariable_ ) :: thermCond_fevar
TYPE( FEVariable_ ) :: volHeatCap_fevar
TYPE( FEVariable_ ) :: HDelta_fevar

INTEGER( I4B ), ALLOCATABLE :: local_nptrs(:)
INTEGER( I4B ), ALLOCATABLE :: nptrs(:)
REAL( DFP ), ALLOCATABLE :: xij( :, : )
REAL( DFP ), ALLOCATABLE :: Ke( :, : )
REAL( DFP ), ALLOCATABLE :: Me( :, : )
REAL( DFP ), ALLOCATABLE :: Be( :, : )
REAL( DFP ), ALLOCATABLE :: Cr( :, : )
REAL( DFP ), ALLOCATABLE :: temp_nodes( : )
REAL( DFP ), ALLOCATABLE :: temp0_nodes(:)
REAL( DFP ), ALLOCATABLE :: temp_gp( : )
REAL( DFP ), ALLOCATABLE :: temp0_gp( : )
REAL( DFP ), ALLOCATABLE :: thermCond(:,:,:)
REAL( DFP ), ALLOCATABLE :: volheatCap(:)
REAL( DFP ), ALLOCATABLE :: HDelta_tanmat( : )
REAL( DFP ), ALLOCATABLE :: HDelta_rhs( : )
CHARACTER(LEN=1) :: PhaseInfo( 50 )

REAL( DFP ), ALLOCATABLE :: tanmat( :, :)
REAL( DFP ), ALLOCATABLE :: rhs_e(:)
REAL( DFP ), ALLOCATABLE :: PC_Fe(:)

!----------------------------------------------------------------------------
!                                                            HeatTransferPM_
!----------------------------------------------------------------------------

TYPE, EXTENDS( Kernel_ ) :: HeatTransferPM_

  INTEGER( I4B ) :: Temp0, DTemp, Temp, RHS, RHS_FIXED
  INTEGER( I4B ) :: Flux = ALGO_OPT_FLUX
  INTEGER( I4B ) :: noFlux = ALGO_OPT_NO_FLUX
  INTEGER( I4B ) :: phaseChange = ALGO_OPT_PHASE_CHANGE
  INTEGER( I4B ) :: nophaseChange = ALGO_OPT_NO_PHASE_CHANGE
  INTEGER( I4B ) :: waterFlux = ALGO_OPT_FLUX_WATER
  INTEGER( I4B ) :: airFlux = ALGO_OPT_FLUX_AIR
  INTEGER( I4B ) :: waterAirFlux = ALGO_OPT_FLUX_WATER_AIR

  TYPE( LagrangeInterpolation_ ) :: SpaceInterpol
  TYPE( H1_ ) :: SpaceContinuity

  TYPE( Soil_ ) :: Soils( MAX_MATERIAL_TYPE )

  TYPE( QuadratureVariables_ ) :: thermCond, thermCond0
    !! thermal properties (therm conductivity) of mixture
  TYPE( QuadratureVariables_ ) :: volHeatCap, volHeatCap0
    !! Volumetric heat capacity of mixture
  TYPE( QuadratureVariables_ ) :: volContent, volContent0
    !! volumetric fraction of solid, liquid, ice
  TYPE( QuadratureVariables_ ) :: enthalapy, enthalapy0
    !! Enthalapy of solid, liquid, ice, and mixture
  LOGICAL( LGT ), ALLOCATABLE :: PCFlag( :, : )

  PROCEDURE( applyInitCondition_nsd_2_c_tnqx ), POINTER, PASS :: &
    & applyInitCondition => NULL()
  PROCEDURE( applyDBC_nsd_2_c_tnqx ), POINTER, PASS :: &
    & applyDBC => NULL()
  PROCEDURE( applyNBC_nsd_2_c_tnqx ), POINTER, PASS :: &
    & applyNBC => NULL()
  PROCEDURE( getHeatFlux_nsd_2_c_tnqx ), POINTER, PASS :: &
    & getHeatFlux => NULL()
  PROCEDURE (setMaterialProperties_nsd_2_c_tnqx), POINTER, PASS :: &
    & setMaterialProperties => NULL()
  PROCEDURE (setVolumetricContent_nsd_2_c_tnqx ), POINTER, PASS :: &
    & setVolumetricContent => NULL()

  CONTAINS
  PROCEDURE, PUBLIC, PASS( Obj ) :: setAlgorithm => setalgo
    !! Extension
  PROCEDURE, PUBLIC, PASS( Obj ) :: setBoundary => setbc
    !! This subroutine set the boundary condition
END TYPE HeatTransferPM_

PUBLIC :: HeatTransferPM_

!----------------------------------------------------------------------------
!                                                     HeatTransferPMPointer_
!----------------------------------------------------------------------------

TYPE :: HeatTransferPMPointer_
  CLASS( HeatTransferPM_ ), POINTER :: Ptr => NULL( )
END TYPE HeatTransferPMPointer_

PUBLIC :: HeatTransferPMPointer_

!----------------------------------------------------------------------------
!                                                             TemperatureFunc
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
!! This is tempelate to specify temperature
  PURE FUNCTION temp_userfunc( x, t ) RESULT( Ans )
    IMPORT :: DFP
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: x( : )
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: t
    REAL( DFP ) :: Ans
  END FUNCTION temp_userfunc
END INTERFACE

!----------------------------------------------------------------------------
!                                                   setAlgorithm@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine sets the algorithm type of heatTransfer kernel

!> authors: Dr. Vikas Sharma
!
! This subroutine sets the algorithm type of heat transfer kernel
! `mainOption` can have following values
! - `obj%STATIC` in which case we solve a static problem
! - `obj%SEMIDISCRETE` in which case we solve a transient problem by using
! discrete FEM

MODULE SUBROUTINE setalgo( Obj, mainOption, extraOption )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: mainOption( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: extraOption( : )
END SUBROUTINE setalgo
END INTERFACE

!----------------------------------------------------------------------------
!                                                    setBoundary@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine sets the boundary condition

!> authors: Dr. Vikas Sharma
!
! - This subroutine sets the boundary conditions to the heat transfer kernel
! - Information of Dirichlet boundary and  Robin boundary is necessary during
! setting up the problem; Robine type boundary condition is useful for
! modeling heat exchange processes

MODULE SUBROUTINE setbc( Obj, DB, NB, robinB, ClimateB, DB_Point )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: DB( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: NB( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: robinB( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ClimateB( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: DB_point( : )
END SUBROUTINE setbc
END INTERFACE

!----------------------------------------------------------------------------
!                                                       methods@NSD_2_C_TNQX
!----------------------------------------------------------------------------

#include "./nsd_2_c_tnqx.inc"
#include "./nsd_2_c_tnpx.inc"


END MODULE HeatTransferPM_Class