SUBMODULE( HeatTransferPM_Class ) nsd_2_c_tnpx
  !! Submodule for transient,no flux, no phase change case of
  !! heat transfer in porous media
USE h5fortran
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                       setVolumetricContent
!----------------------------------------------------------------------------

MODULE PROCEDURE setVolumetricContent_nsd_2_c_tnpx
  CHARACTER( LEN = 4 ) :: NameCase(4)
  REAL( DFP ) :: vol_frac(4)
  INTEGER( I4B ) :: aa, ii, NUM_GP, matID
  INTEGER( I4B ) :: telem, kk, iel
  !
  NUM_GP=SIZE(volContent%val,2)
  !
  IF( PRESENT( S ) ) THEN
    aa = 0
    DO ii=1, SIZE(obj%omegano)
      meshobj=>obj%dom%omega(obj%omegano(ii))%ptr
      telem = meshobj%size()
      matID = obj%OmegaNoToMaterials(ii)
      vol_frac(1) = obj%soils(matID)%volFrac_solid
      DO iel=1,telem
        aa=aa+1
        DO kk=1,NUM_GP
          volContent%val(1, kk, aa) = vol_frac(1)
        END DO
      END DO
    END DO
  END IF
  !
  IF( PRESENT( W ) ) THEN
    IF (PRESENT( Temp ) ) THEN
      aa = 0
      meshobj => obj%dom%omega(obj%omegano(1))%ptr
      elem => meshobj%elem(1)%ptr
      spaceQuad = GaussLegendreQuadrature( refelem=elem%refelem, &
        & order=2*elem%refelem%order )
      NUM_GP = SIZE(spaceQuad, 2)
      CALL initiate( obj = spacesd, quad = spacequad, &
        & refelem = elem%refelem, &
        & continuityType= obj%spaceContinuity, &
        & interpolType = obj%spaceInterpol )
      !
      DO ii=1, SIZE(obj%omegano)
        meshobj=>obj%dom%omega(obj%omegano(ii))%ptr
        matID = obj%OmegaNoToMaterials(ii)
        telem = meshobj%size()
        !
        DO iel=1,telem
          elem => meshobj%elem(iel)%ptr
          local_nptrs = obj%local_nptrs(elem%nptrs)
          xij = nodes(1:obj%nsd, elem%nptrs)
          CALL setValue(obj=spacesd, val=xij, N=spacesd%N, &
            & dNdXi=spacesd%dNdXi)
          temp_nodes = Temp(local_nptrs)
          temp_gp = Interpolation( obj=spacesd, val=temp_nodes)
          aa=aa+1
          DO kk=1,NUM_GP
            vol_frac(2)=obj%soils(matID)%SFCCModel%getValue(Temp=temp_gp(kk))
            volContent%val(2, kk, aa) = vol_frac(2)
          END DO
        END DO
      END DO
    ELSE
      aa = 0
      DO ii=1, SIZE(obj%omegano)
        meshobj=>obj%dom%omega(obj%omegano(ii))%ptr
        telem = meshobj%size()
        matID = obj%OmegaNoToMaterials(ii)
        vol_frac(2) = obj%soils(matID)%volFrac_water
        DO iel=1,telem
          aa=aa+1
          DO kk=1,NUM_GP
            volContent%val(2, kk, aa) = vol_frac(2)
          END DO
        END DO
      END DO
    END IF
  END IF
  !
  IF( PRESENT( I ) ) THEN
    IF( PRESENT( Temp ) ) THEN
      aa = 0
      DO ii=1, SIZE(obj%omegano)
        meshobj=>obj%dom%omega(obj%omegano(ii))%ptr
        telem = meshobj%size()
        matID = obj%OmegaNoToMaterials(ii)
        vol_frac(2) = obj%soils(matID)%volFrac_water
        DO iel=1,telem
          aa=aa+1
          DO kk=1,NUM_GP
            volContent%val(3, kk, aa) = &
              & (vol_frac(2)-volContent%val(2,kk,aa)) &
              & * Water%density / Ice%density
          END DO
        END DO
      END DO
    ELSE
      aa = 0
      DO ii=1, SIZE(obj%omegano)
        meshobj=>obj%dom%omega(obj%omegano(ii))%ptr
        telem = meshobj%size()
        matID = obj%OmegaNoToMaterials(ii)
        vol_frac(3) = obj%soils(matID)%volFrac_ice
        DO iel=1,telem
          aa=aa+1
          DO kk=1,NUM_GP
            volContent%val(3, kk, aa) = vol_frac(3)
          END DO
        END DO
      END DO
    END IF
  END IF
  !
  IF( PRESENT( A ) ) THEN
    aa = 0
    DO ii=1, SIZE(obj%omegano)
      meshobj=>obj%dom%omega(obj%omegano(ii))%ptr
      telem = meshobj%size()
      matID = obj%OmegaNoToMaterials(ii)
      vol_frac(4) = obj%soils(matID)%volFrac_air
      DO iel=1,telem
        aa=aa+1
        DO kk=1,NUM_GP
          volContent%val(4, kk, aa) = vol_frac(4)
        END DO
      END DO
    END DO
  END IF

END PROCEDURE setVolumetricContent_nsd_2_c_tnpx

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE setMaterialProperties_nsd_2_c_tnpx
  INTEGER( I4B ) :: ii, matID, a, kk, telem, iel, tnptrs
  REAL( DFP ) :: realVal(10), vol_Frac(4)
  INTEGER( I4B ) :: NUM_GP

  a = 0
  meshobj => obj%dom%omega(obj%omegano(1))%ptr
  elem => meshobj%elem(1)%ptr
  spaceQuad = GaussLegendreQuadrature( refelem=elem%refelem, &
    & order=2*elem%refelem%order )
  NUM_GP = SIZE(spaceQuad, 2)

  CALL initiate( obj = spacesd, quad = spacequad, &
    & refelem = elem%refelem, &
    & continuityType= obj%spaceContinuity, &
    & interpolType = obj%spaceInterpol )
  !
  DO ii = 1, SIZE( obj%OmegaNo )
    meshobj => obj%dom%omega(obj%omegano(ii))%ptr
    matID = obj%OmegaNoToMaterials(ii)
    telem = meshobj%size()
    !
    DO iel = 1, telem
      !
      elem => meshobj%elem(iel)%ptr
      local_nptrs = obj%local_nptrs(elem%nptrs)
      xij = nodes(1:obj%nsd, elem%nptrs)
      CALL setValue(obj=spacesd, val=xij, N=spacesd%N, &
        & dNdXi=spacesd%dNdXi)
      !
      temp_nodes = Temp(local_nptrs)
      temp0_nodes = obj%nodalVar(obj%temp0)%val(local_nptrs)
      temp_gp = Interpolation( obj=spacesd, val=temp_nodes)
      temp0_gp = Interpolation( obj=spacesd, val=temp0_nodes)
      !
      a=a+1
      !
      ! check phase change
      tnptrs = SIZE(local_nptrs)
      DO kk=1,tnptrs
        PhaseInfo(kk) = obj%soils(matID)%SFCCModel%PhaseInfo( temp_nodes(kk) )
      END DO
      !
      IF( ALL( PhaseInfo(1:tnptrs) .EQ. 'S' ) ) THEN
        obj%PCFlag(2,a) = .FALSE.
      ELSE IF( ALL( PhaseInfo(1:tnptrs) .EQ. 'L' ) ) THEN
        obj%PCFlag(2,a) = .FALSE.
      ELSE
        obj%PCFlag(2,a) = .TRUE.
      END IF
      !
      IF( obj%PCFlag(1,a) .OR. obj%PCFlag(2,a) ) THEN
        obj%PCFlag(3,a) = .TRUE.
        obj%PCFlag(4,a) = .FALSE.
      ELSE
        obj%PCFlag(3,a) = .FALSE.
        obj%PCFlag(4,a) = .TRUE.
      END IF
      !
      DO kk = 1, NUM_GP
        vol_Frac = obj%volContent%val(:, kk, a)
        realVal(1:3) = obj%soils(matID)%thermCondModel%getValue( &
          & volFrac_solid = vol_Frac(1), &
          & volFrac_water = vol_Frac(2), &
          & volFrac_ice = vol_Frac(3), &
          & volFrac_air = vol_Frac(4), &
          & Temp = temp_gp(kk), &
          & x = spacesd%coord(1,kk), &
          & y = spacesd%coord(2,kk), &
          & z = spacesd%coord(3,kk) )

        CALL obj%thermCond%setValue( ipoint=kk, elemnum=a, &
          & Val=realVal(1:3))

        realVal(4) = obj%soils(matID)%volHeatCapModel%getValue( &
          & volFrac_solid = vol_Frac(1), &
          & volFrac_ice = vol_Frac(3), &
          & volFrac_water = vol_Frac(2), &
          & volFrac_air = vol_Frac(4), &
          & Temp = temp_gp(kk), &
          & x = spacesd%coord(1,kk), &
          & y = spacesd%coord(2,kk), &
          & z = spacesd%coord(3,kk) )

        CALL obj%volHeatCap%setValue( ipoint=kk, elemnum=a, &
          & Val=realVal(4:4))

        realVal(5) = obj%soils(matID)%volHeatCapModel%volHeatCap_solid * &
          & temp_gp(kk)
        ! realVal(5) = obj%soils(matID)%volHeatCapModel%volHeatCap_solid * &
        !   & ( temp_gp(kk) - temp0_gp(kk) )

        realVal(6) = temp_gp(kk) * volHeatCap_water()

        ! realVal(6) = &
        !   (temp_gp(kk)-temp0_gp(kk))/6.0_DFP * &
        !   & ( &
        !   &   volHeatCap_water(temp=temp0_gp(kk)) &
        !   & + 4.0*volHeatCap_water(temp=0.5_DFP*(temp0_gp(kk)+temp_gp(kk))) &
        !   & + volHeatCap_water( temp=temp_gp(kk) ) &
        !   & )

        realVal(7) = temp_gp(kk)*volHeatCap_ice()

        ! realVal(7) = &
        !   (temp_gp(kk)-temp0_gp(kk))/6.0_DFP * &
        !   & ( &
        !   &   volHeatCap_ice(temp=temp0_gp(kk)) &
        !   & + 4.0*volHeatCap_ice(temp=0.5_DFP*(temp0_gp(kk)+temp_gp(kk))) &
        !   & + volHeatCap_ice( temp=temp_gp(kk) ) &
        !   & )

          obj%enthalapy%val(1:3, kk, a) = realVal(5:7)
          ! obj%enthalapy%val(1:3, kk, a) = obj%enthalapy0%val(1:3, kk, a) &
          !   & + realVal(5:7)

      END DO
    END DO
  END DO

  NULLIFY( meshobj, elem )
END PROCEDURE setMaterialProperties_nsd_2_c_tnpx

!----------------------------------------------------------------------------
!                                                                 setKernel
!----------------------------------------------------------------------------

MODULE PROCEDURE setKernel_nsd_2_c_tnpx
  INTEGER( I4B ) :: NUM_GP, iel, a, telem, kk, ii, matID
  REAL( DFP ) :: volFrac(4)

  SELECT TYPE( obj )
  CLASS IS (HeatTransferPM_)

    meshobj => obj%dom%omega(obj%omegano(1))%ptr
    elem => meshobj%elem(1)%ptr
    spaceQuad = GaussLegendreQuadrature( refelem=elem%refelem, &
      & order=2*elem%refelem%order )
    NUM_GP = SIZE(spaceQuad, 2)

    CALL obj%thermCond%initiate(tprop=3, tpoint=NUM_GP, telem=obj%telements)
    CALL obj%thermCond0%initiate(tprop=3, tpoint=NUM_GP, telem=obj%telements)

    CALL obj%volHeatCap%initiate(tprop=1, tpoint=NUM_GP, telem=obj%telements)
    CALL obj%volHeatCap0%initiate(tprop=1, tpoint=NUM_GP, telem=obj%telements)

    CALL obj%volContent0%initiate(tprop=4, tpoint=NUM_GP, telem=obj%telements)
    CALL obj%volContent%initiate(tprop=4, tpoint=NUM_GP, telem=obj%telements)

    CALL obj%enthalapy%initiate(tprop=4, tpoint=NUM_GP, telem=obj%telements)
    CALL obj%enthalapy0%initiate(tprop=4, tpoint=NUM_GP, telem=obj%telements)

    ALLOCATE( obj%PCFlag( 4, obj%telements ) )
    obj%PCFlag = .FALSE.

    CALL Obj%setVolumetricContent( S=.TRUE., W=.TRUE., I=.TRUE., A=.TRUE., &
      & volContent=obj%volContent0, Temp=obj%nodalVar(obj%temp0)%val, &
      & nodes=obj%dom%nodes )

    CALL Obj%setVolumetricContent(S=.TRUE., W=.TRUE., I=.TRUE., A=.TRUE., &
      & volContent=obj%volContent, Temp=obj%nodalVar(obj%temp0)%val, &
      & nodes=obj%dom%nodes )

    CALL obj%setMaterialProperties( &
      & Temp=obj%nodalVar(obj%temp0)%val, &
      & Nodes=obj%dom%nodes, thermCond=obj%thermCond0, &
      & volHeatCap=obj%volHeatCap0, &
      & enthalapy=obj%enthalapy0 )

    CALL obj%setMaterialProperties( &
      & Temp=obj%nodalVar(obj%temp0)%val, &
      & Nodes=obj%dom%nodes, thermCond=obj%thermCond, &
      & volHeatCap=obj%volHeatCap, &
      & enthalapy=obj%enthalapy )

  END SELECT

  NULLIFY( meshobj, elem )

END PROCEDURE setKernel_nsd_2_c_tnpx

!----------------------------------------------------------------------------
!                                                                   assemble
!----------------------------------------------------------------------------

MODULE PROCEDURE assemble_nsd_2_c_tnpx
  INTEGER( I4B ) :: ii, kk, iel, telem, NUM_GP, a, matID
  REAL( DFP ) :: volFrac(4), dummyReal
  LOGICAL( LGT ) :: noPCflag

  SELECT TYPE ( obj )
  CLASS IS (HeatTransferPM_)
    !
    ! resetting of tangent material and rhs
    CALL setValue(obj=obj%tanmat, val=0.0_DFP)
    obj%nodalVar(obj%rhs)%val = 0.0_DFP
    !
    a = 0
    DO ii=1,SIZE(obj%omegano)
      meshobj => obj%dom%omega(obj%omegano(ii))%ptr
      matID = obj%OmegaNoToMaterials(ii)
      elem => meshobj%elem(1)%ptr
      telem = meshobj%size()

      spaceQuad = GaussLegendreQuadrature( refelem=elem%refelem, &
        & order=2*elem%refelem%order )
      NUM_GP = SIZE(spaceQuad, 2)

      CALL initiate( &
        & obj=spacesd, quad=spacequad, &
        & refelem=elem%refelem, &
        & continuityType=obj%spaceContinuity, &
        & interpolType=obj%spaceInterpol )

      CALL Reallocate( thermCond, obj%nsd, obj%nsd, NUM_GP )
      CALL Reallocate( volHeatCap, NUM_GP )
      CALL Reallocate( HDelta_tanmat, NUM_GP )
      CALL Reallocate( HDelta_rhs, NUM_GP )

      DO iel=1,telem
        a = a+1
        elem => meshobj%elem(iel)%ptr
        local_nptrs = obj%local_nptrs(elem%nptrs)
        xij = obj%dom%nodes(1:obj%nsd, elem%nptrs)

        CALL setValue(obj=spacesd, val=xij, N=spacesd%N, &
          & dNdXi=spacesd%dNdXi)

        ! computing temperature dependent thermal properties
        temp_nodes = obj%nodalVar(obj%temp)%val(local_nptrs)
        temp0_nodes = obj%nodalVar(obj%temp0)%val(local_nptrs)

        ! loop over gauss points
        DO kk = 1, NUM_GP
          volHeatCap(kk) = obj%volHeatCap%val(1, kk, a )
          thermCond(1,1,kk) = obj%thermCond%val(1, kk, a )
          thermCond(2,2,kk) = obj%thermCond%val(2, kk, a )
        END DO

        volHeatCap_fevar = QuadratureVariable( volHeatCap, &
          & typefevariableScalar, typeFeVariableSpace )

        Me = MassMatrix( Test=spacesd, Trial=spacesd, Rho=volHeatCap_fevar )

        thermCond_fevar = QuadratureVariable( thermCond, &
          & typefevariableMatrix, typeFeVariableSpace )

        Ke = DiffusionMatrix( Test=spacesd, Trial=spacesd, K=thermCond_fevar )

        tanmat = Me / obj%dt + Ke

        rhs_e = &
          & obj%nodalVar(obj%rhs_fixed)%val(local_nptrs) &
          & + MATMUL( Me, (temp0_nodes-temp_nodes)/obj%dt ) &
          & - MATMUL( Ke, temp_nodes )

        !! phase change related end
        CALL ComputePhaseChangeQuantity_3(Obj)
        CALL addcontribution( obj=obj%tanmat, nptrs=local_nptrs, &
          & val=tanmat, scale=1.0_dfp, &
          & storageFMT=DOF_FMT )

        CALL addcontribution( Vec=obj%nodalVar(obj%rhs)%val, &
          & Obj=obj%dof(obj%rhs), Nptrs=local_nptrs, Val=rhs_e, &
          & Scale=1.0_dfp, conversion=[NONE] )
      END DO
    END DO
  END SELECT
  NULLIFY( MeshObj, Elem )


  CONTAINS

#include "./nsd_2_c_tnpx_PhaseChange.inc"

END PROCEDURE assemble_nsd_2_c_tnpx

!----------------------------------------------------------------------------
!                                                             assembleTanMat
!----------------------------------------------------------------------------

MODULE PROCEDURE assembleTanmat_nsd_2_c_tnpx
END PROCEDURE assembleTanmat_nsd_2_c_tnpx

!----------------------------------------------------------------------------
!                                                                 assembleRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE assembleRHS_nsd_2_c_tnpx
END PROCEDURE assembleRHS_nsd_2_c_tnpx

!----------------------------------------------------------------------------
!                                                                     solve
!----------------------------------------------------------------------------

MODULE PROCEDURE solve_nsd_2_c_tnpx
  INTEGER( I4B ) :: ii
  SELECT TYPE( Obj )
  CLASS IS (HeatTransferPM_)
    CALL obj%linsol%setMatrix(obj%tanmat)
    CALL obj%linsol%solve( rhs=obj%nodalVar(obj%rhs)%val, &
      & sol=obj%nodalVar(obj%dTemp)%val )
    ! update temp
    obj%nodalVar(obj%temp)%val = obj%nodalVar(obj%temp)%val &
      & + obj%nodalVar(obj%dTemp)%val

    CALL Obj%setVolumetricContent( &
      & Temp=Obj%nodalVar(obj%temp)%val, &
      & S=.TRUE., W=.TRUE., A=.TRUE., I=.TRUE., &
      & volContent=obj%volContent, &
      & nodes=obj%dom%nodes )
    !
    CALL Obj%setMaterialProperties( &
      & Temp=Obj%nodalVar(obj%temp)%val, &
      & nodes=Obj%dom%nodes, &
      & thermCond=obj%thermcond, &
      & volHeatCap=obj%volHeatCap, &
      & enthalapy=obj%enthalapy )

  END SELECT
END PROCEDURE solve_nsd_2_c_tnpx

!----------------------------------------------------------------------------
!                                                                 isConverged
!----------------------------------------------------------------------------

MODULE PROCEDURE isConverged_nsd_2_c_tnpx
  INTEGER( I4B ) :: ii
  LOGICAL(LGT) :: l1, l2

  l1=.TRUE.
  l2=.TRUE.

  SELECT TYPE(obj)
  CLASS IS(HeatTransferPM_)

    IF( obj%iter .EQ. 0 ) THEN
      obj%err0 = 0.0_DFP
      obj%err0_res = 0.0_DFP
      DO ii = 1, obj%tnodes
        obj%err0 = obj%err0 + ( obj%nodalVar(obj%Temp0)%val(ii))**2
        obj%err0_res = obj%err0_res + ( obj%nodalVar(obj%rhs)%val(ii))**2
      END DO
      obj%err0 = SQRT(obj%err0/obj%tnodes)
      obj%err0_res = SQRT(obj%err0_res/obj%tnodes)
    END IF

    obj%iter = obj%iter + 1
    obj%err = 0.0_DFP
    obj%err_res = 0.0_DFP

    DO ii = 1, obj%tnodes
      obj%err = obj%err &
        & + ( obj%nodalVar(obj%dTemp)%val( ii ) )**2
      obj%err_res = obj%err_res &
        & + ( obj%nodalVar(obj%rhs)%val( ii ) )**2
    END DO

    obj%err = SQRT(obj%err/obj%tnodes)
    obj%err_res = SQRT(obj%err_res/obj%tnodes)

    IF( PRESENT( convergeInSol ) ) THEN
      IF( PRESENT(abstol) ) THEN
        IF( obj%err .LE. abstol ) THEN
          l1 = .TRUE.
        ELSE
          l1 = .FALSE.
        END IF
      END IF
      !
      IF( PRESENT(reltol) ) THEN
        IF( obj%err .LE. obj%err0 * reltol ) THEN
          l2 = .TRUE.
        ELSE
          l2 = .FALSE.
        END IF
      END IF
      !
    ELSE
      !
      IF( PRESENT(abstol) ) THEN
        IF( obj%err_res .LE. abstol ) THEN
          l1 = .TRUE.
        ELSE
          l1 = .FALSE.
        END IF
      END IF
      !
      IF( PRESENT(reltol) ) THEN
        IF( obj%err_res .LE. obj%err0_res * reltol ) THEN
          l2 = .TRUE.
        ELSE
          l2 = .FALSE.
        END IF
      END IF
    END IF
    !
    Ans = l1 .AND. l2

  END SELECT

END PROCEDURE isConverged_nsd_2_c_tnpx

!----------------------------------------------------------------------------
!                                                                     update
!----------------------------------------------------------------------------

MODULE PROCEDURE update_nsd_2_c_tnpx
  INTEGER( I4B ) :: ii

  obj%iter = 0
  obj%its=obj%its+1
  SELECT TYPE( Obj )
  CLASS IS ( HeatTransferPM_ )
    obj%tn = obj%tn + obj%dt
    DO ii = 1, obj%tnodes
      obj%nodalVar(obj%temp0)%val(ii) = obj%nodalVar(obj%temp)%val(ii)
      obj%nodalVar(obj%rhs_fixed)%val(ii) = 0.0_DFP
      !! update material properties also
    END DO

    DO ii=1, SIZE(obj%volContent%val, 3)
      obj%volContent0%val(:, :, ii) = obj%volContent%val(:, :, ii )
      obj%volHeatCap0%val(1:3, :, ii) = obj%volHeatCap%val(1:3, :, ii)
      obj%thermCond0%val(1:3, :, ii) = obj%thermCond%val(1:3, :, ii)
      obj%enthalapy0%val(1:3, :, ii) = obj%enthalapy%val(1:3, :, ii)
      obj%PCFlag(1, ii) = obj%PCFlag(2, ii)
    END DO

  END SELECT

END PROCEDURE update_nsd_2_c_tnpx

!----------------------------------------------------------------------------
!                                                                 getHeatFlux
!----------------------------------------------------------------------------

MODULE PROCEDURE getHeatFlux_nsd_2_c_tnpx
END PROCEDURE getHeatFlux_nsd_2_c_tnpx

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE writeData_nsd_2_c_tnpx
  type( vtk_ ) :: vtu
  TYPE(string) :: pfe(3)

  IF(PRESENT(path)) THEN
    pfe(1)=string(trim(path))
  ELSE
    pfe(1)=string('./')
  END IF

  IF(PRESENT(filename)) THEN
    pfe(2)=string(trim(filename))
  ELSE
    pfe(2)=string('HeatTransferPM')
  END IF

  IF(PRESENT(extension)) THEN
    pfe(3)=string(trim(extension))
  ELSE
    pfe(3)=string('.vtu')
  END IF

  ! step -1
  SELECT TYPE(obj)
  CLASS IS(HeatTransferPM_)
    call vtu % initiate( &
      & path = trim(pfe(1)%chars()), &
      & filename = trim(pfe(2)%chars()), &
      & extension = trim(pfe(3)%chars()), &
      & fmt = "ascii", &
      & meshTopology = "UnstructuredGrid", &
      & indx = indx )
    ! step-2
    call vtu%openPiece( )
    call vtu%writeGeometry( MeshObj=obj%dom%omega, &
        & nodes = obj%dom%nodes, tag=obj%omegano, &
        & local2global = obj%nptrs, map=obj%local_nptrs, &
        & lb = LBOUND(obj%local_nptrs,1), ub=UBOUND(obj%local_nptrs,1) )
    ! step-3
    call vtu%openNodeData( )
    call vtu%writeNodeData( x=obj%nodalVar(obj%temp0)%val,&
      & dofobj=obj%dof(obj%temp0), prefix="_TempConverged" )
    call vtu%writeNodeData( x=obj%nodalVar(obj%rhs)%val,&
      & dofobj=obj%dof(obj%rhs), prefix="_Res" )
    call vtu%closeNodeData()
    !
    ! call vtu % startWriteElementData( )
    ! call vtu % stopWriteElementData( )
    call vtu % closePiece( )
    call vtu % Finalize( )
    !
    ! gmsh post process
    IF( PRESENT(gmsh) ) THEN
    call gmsh%model%mesh%writenodedata(x=obj%nodalVar(obj%Temp0)%val,&
      & Indx=indx, dofObj=obj%dof(obj%temp0), Name='Temp0', &
      & nodes=obj%dom%nodes, local_nptrs=obj%local_nptrs)
    END IF
  END SELECT
END PROCEDURE writeData_nsd_2_c_tnpx

!----------------------------------------------------------------------------
!                                                                 saveState
!----------------------------------------------------------------------------

MODULE PROCEDURE saveState_nsd_2_c_tnpx
END PROCEDURE saveState_nsd_2_c_tnpx

END SUBMODULE nsd_2_c_tnpx