SUBMODULE( HeatTransferPM_Class ) nsd_2_c_tnqx
  !! Submodule for transient,no flux, no phase change case of
  !! heat transfer in porous media
USE h5fortran
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    setDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE setDof_nsd_2_c_tnqx
  INTEGER( I4B ) :: ii

  SELECT TYPE(Obj)
  CLASS IS (HeatTransferPM_)
    Obj%Temp0=1
    Obj%Temp=2
    Obj%dTemp=3
    Obj%rhs=4
    Obj%rhs_fixed=5

    obj%tdof = 1
    ALLOCATE( obj%dof( 5 ), obj%nodalVar( 5 ) )

    obj%dof(obj%Temp0 ) = dof( tNodes=[obj%tNodes], Names=['T'], &
      & SpaceCompo=[-1], TimeCompo=[1], StorageFMT = Nodes_FMT )

    obj%dof(obj%Temp ) = dof( tNodes=[obj%tNodes], Names=['T'], &
      & SpaceCompo=[-1], TimeCompo=[1], StorageFMT = Nodes_FMT )

    obj%dof(obj%DTemp ) = dof( tNodes=[obj%tNodes], Names=['T'], &
      & SpaceCompo=[-1], TimeCompo=[1], StorageFMT = Nodes_FMT )

    obj%dof(obj%rhs_fixed) = dof( tNodes=[obj%tNodes], Names=['R'], &
      & SpaceCompo=[-1], TimeCompo=[1], StorageFMT = Nodes_FMT )

    obj%dof(obj%RHS ) = dof( tNodes=[obj%tNodes], Names=['R'], &
      & SpaceCompo=[-1], TimeCompo=[1], StorageFMT = Nodes_FMT )

    DO ii=1, SIZE(obj%dof)
      CALL initiate( obj % nodalVar(ii), obj % dof(ii) )
    END DO
  END SELECT
END PROCEDURE setDof_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                         applyInitCondition
!----------------------------------------------------------------------------

MODULE PROCEDURE applyInitCondition_nsd_2_c_tnqx
  INTEGER( I4B ) :: ii
  type(hdf5_file) :: h5f
  type(file_) :: afile

  IF( PRESENT( Val ) ) THEN
    IF( SIZE(Val) .EQ. 1 ) THEN
      obj%nodalVar(obj%Temp0)%val = Val(1)
      obj%nodalVar(obj%Temp)%val = Val(1)
      RETURN
    END IF
    !
    IF( PRESENT( local_nptrs ) ) THEN
      DO ii=1,obj%tnodes
        obj%nodalVar(obj%Temp0)%val(ii) = Val(local_nptrs(obj%nptrs(ii)))
        obj%nodalVar(obj%Temp)%val(ii) = Val(local_nptrs(obj%nptrs(ii)))
      END DO
    ELSE
      IF( SIZE(Val) .EQ. obj%tnodes ) THEN
        obj%nodalVar(obj%Temp0)%val = Val
        obj%nodalVar(obj%Temp)%val = Val
      ELSE
        DO ii =1, obj%tnodes
          obj%nodalVar(obj%Temp0)%val(ii)=Val(obj%nptrs(ii))
          obj%nodalVar(obj%Temp)%val(ii)=Val(obj%nptrs(ii))
        END DO
      END IF
    END IF
    RETURN
  END IF
  !
  IF( PRESENT( filename ) ) THEN
    SELECT CASE( trim(extension ) )
    CASE( ".hdf5" )
      CALL h5f%initialize( &
        & filename= trim(path)//trim(filename)//trim(Extension), &
        & status='old', action='r')
      CALL h5f%read( '/Temp0', obj%nodalVar(obj%Temp0)%val)
      CALL  h5f%finalize()
    CASE( ".txt")
      CALL openFileToRead(obj=afile, Path=path, FileName=filename, &
        & extension=extension )
      DO ii=1, obj%tnodes
        READ(afile%unitno, *) obj%nodalVar(obj%Temp0)%val(ii)
        obj%nodalVar(obj%Temp)%val(ii) = obj%nodalVar(obj%Temp0)%val(ii)
      END DO
      CALL closeFile(obj=afile)
    END SELECT
    RETURN
  END IF

END PROCEDURE applyInitCondition_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                                 applyDBC
!----------------------------------------------------------------------------

MODULE PROCEDURE applyDBC_nsd_2_c_tnqx
  INTEGER( I4B ) :: ii, jj, kk, case_num, case_num_minor
  REAL( DFP ) :: x(3)

  IF( PRESENT( tag ) ) case_num = 1
  IF( PRESENT( Nptrs ) ) case_num = 2
  IF( PRESENT( Val ) ) case_num_minor = 1
  IF( PRESENT( TemperatureFunc ) ) case_num_minor = 2

  SELECT CASE( case_num )
  CASE( 1 )
    SELECT CASE( case_num_minor )
    CASE( 1 )
      IF( SIZE( Val ) .EQ. 1 ) THEN
        DO ii=1, obj%dom%mdboundary(tag)%ptr%tNodes
          kk = obj%dom%mdboundary(tag)%ptr%nptrs(ii)
          jj = obj%local_nptrs(kk)
          obj%nodalVar(obj%Temp)%val( jj ) = Val(1)
          obj%nodalVar(obj%dTemp)%val( jj ) = 0.0_DFP
          IF( Obj%its .EQ. 0 ) obj%nodalVar(obj%Temp0)%val( jj ) = Val(1)
        END DO
        RETURN
      END IF

      IF ( PRESENT(local_nptrs) ) THEN
        DO ii=1, obj%dom%mdboundary(tag)%ptr%tNodes
          kk = obj%dom%mdboundary(tag)%ptr%nptrs(ii)
          jj = obj%local_nptrs(kk)
          kk = local_nptrs(kk)
          obj%nodalVar(obj%Temp)%val( jj ) = Val(kk)
          obj%nodalVar(obj%dTemp)%val( jj ) = 0.0_DFP
          IF( Obj%its .EQ. 0 ) obj%nodalVar(obj%Temp0)%val( jj ) = Val(kk)
        END DO
        RETURN
      END IF

      DO ii=1,obj%dom%mdboundary(tag)%ptr%tNodes
          kk = obj%dom%mdboundary(tag)%ptr%nptrs(ii)
          jj = obj%local_nptrs(kk)
          obj%nodalVar(obj%Temp)%val( jj ) = Val(kk)
          obj%nodalVar(obj%dTemp)%val( jj ) = 0.0_DFP
          IF( Obj%its .EQ. 0 ) obj%nodalVar(obj%Temp0)%val( jj ) = Val(kk)
      END DO
      RETURN

    CASE (2)
      DO ii=1,obj%dom%mdboundary(tag)%ptr%tNodes
        kk = obj%dom%mdboundary(tag)%ptr%nptrs(ii)
        jj = obj%local_nptrs(kk)
        x(1:obj%nsd) = obj%dom%nodes(1:obj%nsd, kk)
        obj%nodalVar(obj%Temp)%val( jj ) = TemperatureFunc(x, obj%tn+obj%dt)
        obj%nodalVar(obj%dTemp)%val( jj ) = 0.0_DFP
        IF( Obj%its .EQ. 0 ) &
          & obj%nodalVar(obj%Temp0)%val( jj )=TemperatureFunc(x, obj%tn)
      END DO
      RETURN
    END SELECT
  CASE( 2 )
    SELECT CASE( case_num_minor )
    CASE( 1 )
      IF( SIZE( Val ) .EQ. 1 ) THEN
        DO ii=1, SIZE(Nptrs)
          jj = obj%local_nptrs(Nptrs(ii))
          obj%nodalVar(obj%Temp)%val( jj ) = Val(1)
          obj%nodalVar(obj%dTemp)%val( jj ) = 0.0_DFP
          IF( Obj%its .EQ. 0 ) obj%nodalVar(obj%Temp0)%val( jj ) = Val(1)
        END DO
        RETURN
      END IF

      IF( SIZE( Val ) .EQ. SIZE(Nptrs) ) THEN
        DO ii=1, SIZE(Nptrs)
          jj = obj%local_nptrs(Nptrs(ii))
          obj%nodalVar(obj%Temp)%val(jj) = Val(ii)
          obj%nodalVar(obj%dTemp)%val( jj ) = 0.0_DFP
          IF( Obj%its .EQ. 0 ) obj%nodalVar(obj%Temp0)%val( jj ) = Val(ii)
        END DO
        RETURN
      END IF

      IF( PRESENT( local_nptrs ) ) THEN
        DO ii=1, SIZE(Nptrs)
          jj = obj%local_nptrs(Nptrs(ii))
          kk = local_nptrs(Nptrs(ii))
          obj%nodalVar(obj%Temp)%val( jj ) = Val(kk)
          obj%nodalVar(obj%dTemp)%val( jj ) = 0.0_DFP
          IF( Obj%its .EQ. 0 ) obj%nodalVar(obj%Temp0)%val( jj ) = Val(kk)
        END DO
        RETURN
      END IF

      DO ii=1, SIZE(Nptrs)
        jj = obj%local_nptrs(Nptrs(ii))
        obj%nodalVar(obj%Temp)%val( jj ) = Val(Nptrs(ii))
        obj%nodalVar(obj%dTemp)%val( jj ) = 0.0_DFP
        IF( Obj%its .EQ. 0 ) obj%nodalVar(obj%Temp0)%val( jj )=Val(Nptrs(ii))
      END DO

    CASE( 2 )
      DO ii=1,SIZE(Nptrs)
        kk = Nptrs(ii)
        jj = obj%local_nptrs(kk)
        x(1:obj%nsd) = obj%dom%nodes(1:obj%nsd, kk)
        obj%nodalVar(obj%Temp)%val( jj ) = TemperatureFunc(x, obj%tn+obj%dt)
        obj%nodalVar(obj%dTemp)%val( jj ) = 0.0_DFP
        IF( Obj%its .EQ. 0 ) &
          & obj%nodalVar(obj%Temp0)%val( jj ) = TemperatureFunc(x, obj%tn)
      END DO
    END SELECT
  END SELECT

END PROCEDURE applyDBC_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                                  applyNBC
!----------------------------------------------------------------------------

MODULE PROCEDURE applyNBC_nsd_2_c_tnqx
  !! TO DO
END PROCEDURE applyNBC_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                       setVolumetricContent
!----------------------------------------------------------------------------

MODULE PROCEDURE setVolumetricContent_nsd_2_c_tnqx
  CHARACTER( LEN = 4 ) :: NameCase(4)
  REAL( DFP ) :: vol_frac(4)
  INTEGER( I4B ) :: aa, ii, NUM_GP, matID, telem, kk, iel

  NUM_GP=SIZE(volContent%val,2)

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

  IF( PRESENT( W ) ) THEN
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

  IF( PRESENT( I ) ) THEN
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

END PROCEDURE setVolumetricContent_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE setMaterialProperties_nsd_2_c_tnqx
  INTEGER( I4B ) :: ii, matID, a, kk, telem, iel
  REAL( DFP ) :: therm_Cond(3), vol_Frac(4), vol_HeatCap
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

  DO ii = 1, SIZE( obj%OmegaNo )
    meshobj => obj%dom%omega(obj%omegano(ii))%ptr
    matID = obj%OmegaNoToMaterials(ii)
    telem = meshobj%size()

    DO iel = 1, telem
      elem => meshobj%elem(iel)%ptr
      local_nptrs = obj%local_nptrs(elem%nptrs)
      xij = nodes(1:obj%nsd, elem%nptrs)
      CALL setValue(obj=spacesd, val=xij, N=spacesd%N, &
        & dNdXi=spacesd%dNdXi)

      temp_nodes = Temp(local_nptrs)
      temp_gp = Interpolation( obj=spacesd, val=temp_nodes)

      a=a+1
      DO kk = 1, NUM_GP
        vol_Frac = obj%volContent%val(:, kk, a)
        therm_cond(1) = obj%soils(matID)%thermCondModel%getValue( &
          & volFrac_solid = vol_Frac(1), &
          & volFrac_water = vol_Frac(2), &
          & volFrac_ice = vol_Frac(3), &
          & volFrac_air = vol_Frac(4), &
          & Temp = temp_gp(kk), &
          & x = spacesd%coord(1,kk), &
          & y = spacesd%coord(2,kk), &
          & z = spacesd%coord(3,kk) )

        therm_Cond(2:3) = therm_Cond(1)
        CALL obj%thermCond%setValue( ipoint=kk, elemnum=a, &
          & Val=therm_Cond)

        vol_HeatCap = obj%soils(matID)%volHeatCapModel%getValue( &
          & volFrac_solid = vol_Frac(1), &
          & volFrac_ice = vol_Frac(3), &
          & volFrac_water = vol_Frac(2), &
          & volFrac_air = vol_Frac(4), &
          & Temp = temp_gp(kk), &
          & x = spacesd%coord(1,kk), &
          & y = spacesd%coord(2,kk), &
          & z = spacesd%coord(3,kk) )

        CALL obj%volHeatCap%setValue( ipoint=kk, elemnum=a, &
          & Val=[vol_HeatCap])
      END DO
    END DO

  END DO

  NULLIFY( meshobj, elem )
END PROCEDURE setMaterialProperties_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                                 setKernel
!----------------------------------------------------------------------------

MODULE PROCEDURE setKernel_nsd_2_c_tnqx
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
    CALL obj%volHeatCap%initiate(tprop=1, tpoint=NUM_GP, telem=obj%telements)
    CALL obj%volContent%initiate(tprop=4, tpoint=NUM_GP, telem=obj%telements)

    CALL Obj%setVolumetricContent(S=.TRUE., W=.TRUE., I=.TRUE., A=.TRUE., &
      & volContent=obj%volContent, Temp=obj%nodalVar(obj%temp0)%val, &
      & nodes = obj%dom%nodes )

    CALL obj%setMaterialProperties( &
      & Temp=obj%nodalVar(obj%temp0)%val, &
      & Nodes=obj%dom%nodes, thermCond=obj%thermCond, &
      & volHeatCap=obj%volHeatCap, &
      & enthalapy=obj%enthalapy )

  END SELECT

  NULLIFY( meshobj, elem )

END PROCEDURE setKernel_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                                 assemble
!----------------------------------------------------------------------------

MODULE PROCEDURE assemble_nsd_2_c_tnqx
  INTEGER( I4B ) :: ii, kk, iel, telem, NUM_GP, a
  REAL( DFP ) :: volFrac(4)

  SELECT TYPE ( obj )
  CLASS IS (HeatTransferPM_)

    CALL Obj%setMaterialProperties( &
      & Temp=Obj%nodalVar(obj%temp)%val, &
      & nodes=Obj%dom%nodes, &
      & thermCond=obj%thermcond, &
      & volHeatCap=obj%volHeatCap, &
      & enthalapy=obj%enthalapy )

    ! resetting of tangent material and rhs
    CALL setValue(obj=obj%tanmat, val=0.0_DFP)
    obj%nodalVar(obj%rhs)%val = 0.0_DFP

    a = 0
    DO ii=1,SIZE(obj%omegano)
      meshobj => obj%dom%omega(obj%omegano(ii))%ptr
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

      a=a+1
      DO iel=1,telem
        elem => meshobj%elem(iel)%ptr
        local_nptrs = obj%local_nptrs(elem%nptrs)
        xij = obj%dom%nodes(1:obj%nsd, elem%nptrs)

        CALL setValue(obj=spacesd, val=xij, N=spacesd%N, &
          & dNdXi=spacesd%dNdXi)

        ! computing temperature dependent thermal properties
        temp_nodes = obj%nodalVar(obj%temp)%val(local_nptrs)
        temp0_nodes = obj%nodalVar(obj%temp0)%val(local_nptrs)
        temp_gp = Interpolation( obj=spacesd, val=temp_nodes )

        DO kk = 1, NUM_GP
          volHeatCap(kk) = obj%volHeatCap%val(1, kk, iel+a-1 )
          thermCond(1,1,kk) = obj%thermCond%val(1, kk, iel+a-1 )
          thermCond(2,2,kk) = obj%thermCond%val(2, kk, iel+a-1 )
        END DO

        volHeatCap_fevar = QuadratureVariable( volHeatCap, &
          & typefevariableScalar, typeFeVariableSpace )

        Me = MassMatrix( Test=spacesd, Trial=spacesd, Rho=volHeatCap_fevar )

        thermCond_fevar = QuadratureVariable( thermCond, &
          & typefevariableMatrix, typeFeVariableSpace )

        Ke = DiffusionMatrix( Test=spacesd, Trial=spacesd, K=thermCond_fevar )

        tanmat = Me / obj%dt + Ke

        CALL addcontribution( obj=obj%tanmat, nptrs=local_nptrs, &
          & val=tanmat, scale=1.0_dfp, &
          & storageFMT=DOF_FMT )

        rhs_e = &
          & obj%nodalVar(obj%rhs_fixed)%val(local_nptrs) &
          & + MATMUL( Me, (temp0_nodes-temp_nodes)/obj%dt ) &
          & - MATMUL( Ke, temp_nodes )

        CALL addcontribution( Vec=obj%nodalVar(obj%rhs)%val, &
          & Obj=obj%dof(obj%rhs), Nptrs=local_nptrs, Val=rhs_e, &
          & Scale=1.0_dfp, conversion=[NONE] )
      END DO
    END DO
  END SELECT
  NULLIFY( MeshObj, Elem )

END PROCEDURE assemble_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                             assembleTanMat
!----------------------------------------------------------------------------

MODULE PROCEDURE assembleTanmat_nsd_2_c_tnqx
END PROCEDURE assembleTanmat_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                                 assembleRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE assembleRHS_nsd_2_c_tnqx
END PROCEDURE assembleRHS_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                                     solve
!----------------------------------------------------------------------------

MODULE PROCEDURE solve_nsd_2_c_tnqx
  INTEGER( I4B ) :: ii

  SELECT TYPE( Obj )
  CLASS IS (HeatTransferPM_)
    CALL obj%linsol%setMatrix(obj%tanmat)
    CALL obj%linsol%solve( rhs=obj%nodalVar(obj%rhs)%val, &
      & sol=obj%nodalVar(obj%dTemp)%val )
    ! update temp
    obj%nodalVar(obj%temp)%val = obj%nodalVar(obj%temp)%val &
      & + obj%nodalVar(obj%dTemp)%val
  END SELECT
END PROCEDURE solve_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                                 isConverged
!----------------------------------------------------------------------------

MODULE PROCEDURE isConverged_nsd_2_c_tnqx
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

END PROCEDURE isConverged_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                                     update
!----------------------------------------------------------------------------

MODULE PROCEDURE update_nsd_2_c_tnqx
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
  END SELECT

END PROCEDURE update_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                                 getHeatFlux
!----------------------------------------------------------------------------

MODULE PROCEDURE getHeatFlux_nsd_2_c_tnqx
END PROCEDURE getHeatFlux_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE writeData_nsd_2_c_tnqx
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
    call vtu%closeNodeData()

    ! call vtu % startWriteElementData( )
    ! call vtu % stopWriteElementData( )
    call vtu % closePiece( )
    call vtu % Finalize( )

    ! gmsh post process
    IF( PRESENT(gmsh) ) THEN
    call gmsh%model%mesh%writenodedata(x=obj%nodalVar(obj%Temp0)%val,&
      & Indx=indx, dofObj=obj%dof(obj%temp0), Name='Temp0', &
      & nodes=obj%dom%nodes, local_nptrs=obj%local_nptrs)
    END IF
  END SELECT
END PROCEDURE writeData_nsd_2_c_tnqx

!----------------------------------------------------------------------------
!                                                                 saveState
!----------------------------------------------------------------------------

MODULE PROCEDURE saveState_nsd_2_c_tnqx
END PROCEDURE saveState_nsd_2_c_tnqx

END SUBMODULE nsd_2_c_tnqx