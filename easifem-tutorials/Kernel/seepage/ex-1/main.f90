!! we USE no penetration boundary condition

PROGRAM Main
USE easifem
USE seepage_class
USE movingmesh_class
IMPLICIT NONE

#include "./var.inc"

  !---------------------------------------------------------------------------

  !---------------------------------------------------------------------------
  ! Gamma_m ---> Omega
  CALL display('INITIATING GMSH OBJECT')
  ii = gmsh%initialize( NSD )
  ii = gmsh%open( "./", "mesh", ".msh" )
  tnodes = gmsh % model % mesh % totalnodes( )

  CALL display('INITIATING DOMAIN')
  allocate( facetmesh( 2, 2 ) )
  facetmesh(1,1) = "Gamma_4"
  facetmesh(1,2) = "Omega_2"
  facetmesh(2,1) = "Gamma_5"
  facetmesh(2,2) = "Omega_1"
  CALL gmsh%model%mesh%getElements( dom, facetmesh )

  !---------------------------------------------------------------------------

  !---------------------------------------------------------------------------

  CALL display( "INITIATING MESH MOVER" )
  CALL meshmove%initiate( NSD=NSD, NNT=NNT, dt=dt )
  CALL meshmove%setdomain(dom=dom, omegano=[1,2])
  CALL meshmove%setMaterialProperties(matprops=meshprops)
  CALL meshmove%setAlgorithm(mainOption=[meshmove%elasticity])
  CALL meshmove%setBoundary( &
    & xdb = [BOTTOM, RIGHT1, RIGHT2, LEFT, MID], &
    & ydb = [BOTTOM, LEFT], &
    & mb=[TOP_L, TOP_R])

  CALL display( "INITIATING SEEPAGE KERNEL" )
  CALL seep%initiate( NSD=NSD, NNT=NNT, dt=dt)
  CALL seep%setdomain(dom=dom, omegano=[1,2])
  CALL seep%setMaterialProperties(matprops=damprops)
  CALL seep%setAlgorithm( mainOption=[seep%spacetime], &
    & extraoption=[seep%explicit_velocity])
  CALL seep%setBoundary(DB = [RIGHT1, RIGHT2, TOP_L, TOP_R, LEFT])

  CALL display( "APPLYING INITIAL CONDITION FOR SEEPAGE" )
  CALL seep%applyInitCondition(path=path, filename=filename, &
    & extension=extension)
  CALL Display( 'enter its0 :: ' )
  READ(*,*) its0

  seep%PressureFunc=>upstreamBC
  CALL seep%applyDBC(tag=LEFT)

  seep%PressureFunc=>downstreamBC
  CALL seep%applyDBC(tag=RIGHT1)

  CALL seep%writedata( gmsh=gmsh, path="./Seepage/", filename="seepage", &
    & extension=".vtu", indx=[its0] )

  CALL display( "STARTING TIME STEP LOOP" )
  seep%PressureFunc=>freeSurfaceBC

  DO its = its0+1, NTS

    CALL meshmove%getMeshQuality(qmin=qmin(its-1), &
      & qmax=qmax(its-1),&
      & qavg=qavg(its-1) )

    CALL Display( meshmove%fA, 'meshmove%fA :: ' )
    CALL Display( meshmove%fAR, 'meshmove%fAR :: ' )
    IF( meshmove%fA .GE. MESH_QUALITY_FA &
      & .OR. meshmove%fAR .GT. MESH_QUALITY_FAR ) THEN
      CALL display( 'Saving the sate' )
      call seep % saveState("./state_old/", "seepage", ".hdf5", its-1)
      CALL display( 'Saving the mesh' )
      CALL gmsh%model%mesh%writemesh("./state_old/", "mesh", ".msh",&
        & dom%nodes)
      CALL display( 'Bad MESH encounter!!!' )
      CALL display( 'REMESHING...' )
      ii = newgmsh % initiate( nsd = nsd )
      ii = newgmsh%model%add("remesh")
      ii = gmsh%remesh( gmsh=newgmsh, Nodes=dom%Nodes )
      ii = newgmsh%write("./state_new/","mesh",".geo" )
      ii = newgmsh%finalize()
      STOP
    END IF

    DO ii = 1, MAXITER
      CALL seep%assemble()
      CALL seep%applyDBC(tag=RIGHT1)
      CALL seep%applyDBC(tag=RIGHT2)
      CALL seep%applyDBC(tag=TOP_L)
      CALL seep%applyDBC(tag=TOP_R)
      CALL seep%solve()
      CALL seep%getVelocity()

      isconverge=seep%isConverged(relTol=reltol)

      CALL display([its, ii], "its, ii :: ")
      CALL display([seep%err0, seep%err], "err0, err :: ")

      IF(isconverge) THEN
        CALL display(ii, "converged in total steps :: ")
        CALL seep%update( reset=.false.)
        CALL meshmove%update( reset=.true. )
        EXIT

      ELSE

        ! CALL meshmove%smooth(maxiter=1)
        CALL meshmove%assemble()
        CALL seep%getNormalVelocity(tag=[TOP_L, TOP_R], Vn=Vn, &
          & local_nptrs=local_nptrs, Angle=Angle(its))

        CALL reallocate( v, tnodes*nsd )
        !>> top right boundary
        DO jj = 1, dom%mdboundary(TOP_R)%ptr%tnodes
          kk = dom%mdboundary(TOP_R)%ptr%nptrs(jj)
          ll = meshmove%local_nptrs(kk)
          v((ll-1)*nsd+1) = vn((local_nptrs(kk)-1)*nsd+1)
          v((ll-1)*nsd+2) = vn((local_nptrs(kk)-1)*nsd+2)
        END DO

        !>> top left boundary
        DO jj = 1, dom%mdboundary(TOP_L)%ptr%tnodes
          kk = dom%mdboundary(TOP_L)%ptr%nptrs(jj)
          ll = meshmove%local_nptrs(kk)
          v((ll-1)*nsd+1) = vn((local_nptrs(kk)-1)*nsd+1)
          v((ll-1)*nsd+2) = vn((local_nptrs(kk)-1)*nsd+2)
        END DO

        CALL meshmove%applyWeakDBC(tag=[TOP_L, TOP_R], y=v, dt=dt)
        CALL meshmove%applyUniformDisplacement(tag=RIGHT1,val=0.0_DFP, &
          & dim=1)
        CALL meshmove%applyUniformDisplacement(tag=RIGHT2,val=0.0_DFP, &
          & dim=1)
        CALL meshmove%applyUniformDisplacement(tag=LEFT,val=0.0_DFP, dim=0)
        CALL meshmove%applyUniformDisplacement(tag=MID,val=0.0_DFP, dim=1)
        CALL meshmove%solve()
      end IF
    end DO

    IF(.not. isconverge) THEN
      CALL display(its, "no convergence in its :: ")
      CALL display(seep%err, "err :: ")
      ! CALL seep%update( reset=.false.)
      ! CALL meshmove%update( reset=.true. )
      ! !> post processing
      ! CALL seep%writedata(gmsh=gmsh, path="./Seepage/", filename="seepage",&
      !   & extension=".vtu", indx=[its])
      stop
    end IF

    !> post processing
    CALL seep%writedata(gmsh=gmsh, path="./Seepage/", filename="seepage",&
      & extension=".vtu", indx=[its])

  end DO

#include "./contains.inc"
END PROGRAM main