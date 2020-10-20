!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_sfpw( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_sfpa( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_sfpb( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_sfqw( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_sfqa( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_sfqb( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_snpx( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_snqx( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_tfpw( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_tfpa( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_tfpb( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_tfqw( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_tfqa( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_tfqb( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_tnpx( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
  Obj%applyInitCondition => applyInitCondition_nsd_2_c_tnqx
  Obj%applyDBC => applyDBC_nsd_2_c_tnqx
  Obj%applyNBC => applyNBC_nsd_2_c_tnqx
  Obj%setVolumetricContent => setVolumetricContent_nsd_2_c_tnpx
  Obj%setMaterialProperties => setMaterialProperties_nsd_2_c_tnpx
  Obj%setKernel => setKernel_nsd_2_c_tnpx
  Obj%assemble => assemble_nsd_2_c_tnpx
  Obj%assembleTanmat => assembleTanmat_nsd_2_c_tnpx
  Obj%assembleRHS => assembleRHS_nsd_2_c_tnpx
  Obj%Solve => solve_nsd_2_c_tnpx
  Obj%isConverged => isConverged_nsd_2_c_tnpx
  Obj%update => update_nsd_2_c_tnpx
  Obj%getHeatFlux => getHeatFlux_nsd_2_c_tnpx
  Obj%WriteData => writeData_nsd_2_c_tnpx
  Obj%SaveState => saveState_nsd_2_c_tnpx
  CALL setDOF_nsd_2_c_tnqx(Obj)
  CALL Obj%setLinearSolver(name=lis_cg, tol=1.0d-8)
  CALL Obj%setPrecondition()
  CALL Obj%setTanmat()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_algo_nsd_2_c_tnqx( Obj )
  CLASS( HeatTransferPM_ ), INTENT( INOUT) :: Obj
  Obj%applyInitCondition => applyInitCondition_nsd_2_c_tnqx
  Obj%applyDBC => applyDBC_nsd_2_c_tnqx
  Obj%applyNBC => applyNBC_nsd_2_c_tnqx
  Obj%setVolumetricContent => setVolumetricContent_nsd_2_c_tnqx
  Obj%setMaterialProperties => setMaterialProperties_nsd_2_c_tnqx
  Obj%setKernel => setKernel_nsd_2_c_tnqx
  Obj%assemble => assemble_nsd_2_c_tnqx
  Obj%assembleTanmat => assembleTanmat_nsd_2_c_tnqx
  Obj%assembleRHS => assembleRHS_nsd_2_c_tnqx
  Obj%Solve => solve_nsd_2_c_tnqx
  Obj%isConverged => isConverged_nsd_2_c_tnqx
  Obj%update => update_nsd_2_c_tnqx
  Obj%getHeatFlux => getHeatFlux_nsd_2_c_tnqx
  Obj%WriteData => writeData_nsd_2_c_tnqx
  Obj%SaveState => saveState_nsd_2_c_tnqx
  CALL setDOF_nsd_2_c_tnqx(Obj)
  CALL Obj%setLinearSolver(name=lis_cg, tol=1.0d-8)
  CALL Obj%setPrecondition()
  CALL Obj%setTanmat()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------