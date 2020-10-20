!----------------------------------------------------------------------------
!                                                       setDOF@nsd_2_c_tnpx
!----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine set the degrees of freedom for HeatTransfer in porous media

! !> authors: Dr. Vikas Sharma
! !
! ! This subroutine sets the degrees of freedom for heat transfer in porous
! ! media kernel. Also, this subroutine initiate the variable necessary to
! ! run the kernel
! !
! ! - Currently, we have four nodal of degrees of freedom; T0, T, dT, RHS

! MODULE SUBROUTINE setDof_nsd_2_c_tnpx( Obj )
!   CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
! END SUBROUTINE setDof_nsd_2_c_tnpx
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                           applyInitCondition@nsd_2_c_tnpx
! !----------------------------------------------------------------------------

! INTERFACE
! !! This subroutine apply initial condition to HeatTransferPM_ kernel

! !> authors: Dr. Vikas Sharma
! !
! ! This subroutine apply initial conditions to the `HeatTransferPM_` kernel
! ! If `Val` is not present then function pointer `TempertureFunc` is used
! ! If `Val` is present then initial values are taken from `Val`
! ! If `local_nptrs` are present then we use following mapping
! ! Obj%Temp0(i) = initVal(local_nptrs(nptrs(i)))
! ! If `filename` is present then initial conditions are read from the file

! MODULE SUBROUTINE applyInitCondition_nsd_2_c_tnpx( Obj, Val, local_nptrs, &
!   & path, filename, extension )
!   CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
!   REAL( DFP ), OPTIONAL, INTENT( IN ) :: Val( : )
!   INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: local_nptrs(:)
!   CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: path
!   CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: filename
!   CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: extension
! END SUBROUTINE applyInitCondition_nsd_2_c_tnpx
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                      applyDBC@nsd_2_c_tnpx
! !----------------------------------------------------------------------------

! INTERFACE
!   !! This subroutine applies boundary condition to `HeatTransferPM_` kernel

! !> authors: Dr. Vikas Sharma
! !
! ! This subroutine applies boundary condition to `HeatTransferPM_` kernel
! ! - Boundary conditions will be applied to`obj%Temp` and `obj%dTemp`
! ! - If `Val` is not present then `local_nptrs` is not requried; in that case,
! ! procedure pointer `TemperatureFunc` is used
! ! If `Val` is present then `local_nptrs` may or maynot be present

! MODULE SUBROUTINE applyDBC_nsd_2_c_tnpx( Obj, tag, Nptrs, &
!   & TemperatureFunc, Val, local_nptrs )
!   CLASS( HeatTransferPM_ ), INTENT( INOUT ) :: Obj
!   INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
!   REAL( DFP ), OPTIONAL, INTENT( IN ) :: Val(:)
!   INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: local_nptrs(:)
!   INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: Nptrs(:)
!   PROCEDURE( temp_userfunc ), POINTER, OPTIONAL, INTENT( IN ):: &
!     & TemperatureFunc
! END SUBROUTINE applyDBC_nsd_2_c_tnpx
! END INTERFACE

! !----------------------------------------------------------------------------
! !                                                      applyNBC@nsd_2_c_tnpx
! !----------------------------------------------------------------------------

! INTERFACE
! MODULE SUBROUTINE applyNBC_nsd_2_c_tnpx( Obj, tag, UniformSteady, &
!   & UniformTransient, NonUniformSteady, NonUniformTransient )
!   CLASS( HeatTransferPM_ ), INTENT( INOUT ) :: Obj
!   INTEGER( I4B ), INTENT( IN ) :: tag
!   REAL( DFP ), OPTIONAL, INTENT( IN ) :: UniformSteady
!     !! Uniform Value
!   REAL( DFP ), OPTIONAL, INTENT( IN ) :: UniformTransient( : )
!     !! Time varying value, in space it is constant
!   REAL( DFP ), OPTIONAL, INTENT( IN ) :: NonUniformSteady( : )
!     !! Varying only in space, remains constant in time
!   REAL( DFP ), OPTIONAL, INTENT( IN ) :: NonUniformTransient( :, : )
!     !! Varying in space and time
! END SUBROUTINE applyNBC_nsd_2_c_tnpx
! END INTERFACE

!----------------------------------------------------------------------------
!                                           setVolumetricContent@nsd_2_c_tnpx
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE setVolumetricContent_nsd_2_c_tnpx( Obj, Temp, S, W, I, A, &
  & volContent, nodes )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp( : )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: W
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: S
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: A
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: I
  TYPE( QuadratureVariables_ ), INTENT( INOUT ) :: volContent
  REAL( DFP ), INTENT( IN ) :: nodes(:,:)
END SUBROUTINE setVolumetricContent_nsd_2_c_tnpx
END INTERFACE

!----------------------------------------------------------------------------
!                                        setMaterialProperties@nsd_2_c_tnpx
!----------------------------------------------------------------------------

INTERFACE
!! Set thermal conductivity and specific heat capacity

!> authors: Dr. Vikas Sharma
!
! This subroutine sets thermal conductivity and heat capacity of the kernel

MODULE SUBROUTINE setMaterialProperties_nsd_2_c_tnpx( Obj, Temp, Nodes, &
  & thermCond, volHeatCap, enthalapy )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), INTENT( IN ) :: Temp( : )
  REAL( DFP ), INTENT( IN ) :: Nodes( :, : )
  TYPE( QuadratureVariables_ ), INTENT( INOUT) :: thermCond
  TYPE( QuadratureVariables_ ), INTENT( INOUT) :: volHeatCap
  TYPE( QuadratureVariables_ ), INTENT( INOUT) :: enthalapy
END SUBROUTINE setMaterialProperties_nsd_2_c_tnpx
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setKernel@nsd_2_c_tnpx
!----------------------------------------------------------------------------

INTERFACE
!! Final setup of the kernel

!> authors: Dr. Vikas Sharma
!
! This subroutine performs final setup for the kernel
! This subroutine set the volContent, thermCond, volHeatCap

MODULE SUBROUTINE setKernel_nsd_2_c_tnpx( Obj )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
END SUBROUTINE setKernel_nsd_2_c_tnpx
END INTERFACE

!----------------------------------------------------------------------------
!                                                      assemble@nsd_2_c_tnpx
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine assemble the system of linear equation for heatTransferPM_

!> authors: Dr. Vikas Sharma
!
! This subroutine assemble the system of linear equations for `heatTransferPM`

MODULE SUBROUTINE assemble_nsd_2_c_tnpx( Obj )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
END SUBROUTINE assemble_nsd_2_c_tnpx
END INTERFACE

!----------------------------------------------------------------------------
!                                               assembleTanMat@nsd_2_c_tnpx
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine assemble the system of linear equation for heatTransferPM_

!> authors: Dr. Vikas Sharma
!
! This subroutine assemble the system of linear equations for `heatTransferPM`

MODULE SUBROUTINE assembleTanMat_nsd_2_c_tnpx( Obj )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
END SUBROUTINE assembleTanMat_nsd_2_c_tnpx
END INTERFACE

!----------------------------------------------------------------------------
!                                               assembleRHS@nsd_2_c_tnpx
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine assemble the system of linear equation for heatTransferPM_

!> authors: Dr. Vikas Sharma
!
! This subroutine assemble the system of linear equations for `heatTransferPM`

MODULE SUBROUTINE assembleRHS_nsd_2_c_tnpx( Obj )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
END SUBROUTINE assembleRHS_nsd_2_c_tnpx
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Solve@nsd_2_c_tnpx
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE solve_nsd_2_c_tnpx( Obj )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
END SUBROUTINE solve_nsd_2_c_tnpx
END INTERFACE

!----------------------------------------------------------------------------
!                                                   isConverged@nsd_2_c_tnpx
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION isConverged_nsd_2_c_tnpx( Obj,  relTol, abstol, &
  & convergeInRes, convergeInSol ) RESULT( Ans )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: relTol
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: absTol
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: convergeInRes
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: convergeInSol
  LOGICAL( LGT ) :: Ans
END FUNCTION isConverged_nsd_2_c_tnpx
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Update@nsd_2_c_tnpx
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE Update_nsd_2_c_tnpx( Obj, reset )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: reset
END SUBROUTINE Update_nsd_2_c_tnpx
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getHeatFlux@nsd_2_c_tnpx
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE getHeatFlux_nsd_2_c_tnpx( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE getHeatFlux_nsd_2_c_tnpx
END INTERFACE

!----------------------------------------------------------------------------
!                                                     WriteData@nsd_2_c_tnpx
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE WriteData_nsd_2_c_tnpx( Obj, gmsh, path, filename, &
  & extension, indx )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  TYPE(gmsh_), OPTIONAL, INTENT( INOUT) :: gmsh
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: path, filename, extension
  INTEGER( I4B ), INTENT( IN ) :: indx(:)
END SUBROUTINE WriteData_nsd_2_c_tnpx
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SaveState@nsd_2_c_tnpx
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE saveState_nsd_2_c_tnpx( Obj, Path, FileName, Extension, &
  & Timestep )
  CLASS( Kernel_ ), INTENT( INOUT) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Path
  CHARACTER( LEN = * ), INTENT( IN ) :: Filename
  CHARACTER( LEN = * ), INTENT( IN ) :: Extension
  INTEGER( I4B ), INTENT( IN ) :: Timestep
END SUBROUTINE saveState_nsd_2_c_tnpx
END INTERFACE
