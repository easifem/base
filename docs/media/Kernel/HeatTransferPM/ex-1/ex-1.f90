program main
use easifem
use HeatTransferPM_Class
implicit none

INTEGER( I4B ) :: ii
INTEGER( I4B ) :: tNodes
INTEGER( I4B ) :: its
INTEGER( I4B ) :: iter
  !! Total nodes in the problem
INTEGER( I4B ), PARAMETER :: NSD = 2, BOTTOM=1, RIGHT=2, TOP=3, LEFT=4
  !! Spatial domain of the problem
REAL( DFP ), PARAMETER :: dt = 15 !! in sec
REAL( DFP ), PARAMETER :: tn = 0.0_DFP !! in sec
INTEGER( I4B ), PARAMETER :: NTS = 100
INTEGER( I4B ), PARAMETER :: MaxIter = 10
REAL( DFP ), PARAMETER :: tol_res=1.0E-5
REAL( DFP ), PARAMETER :: tol_sol=1.0E-5
LOGICAL( LGT ) :: convgInSol
LOGICAL( LGT ) :: convgInRes
PROCEDURE( UserVolHeatCap ), POINTER :: User_VolHeatCap

TYPE( HeatTransferPM_ ) :: HeatTransPM
TYPE( Gmsh_ ) :: gmsh
TYPE( Domain_ ) :: dom

CALL Display( 'INITIATING GMSH OBJECT')
!--------------------------------------
ii = gmsh%initialize(NSD)
ii = gmsh%open("./", "mesh", ".msh")
tNodes = gmsh%model%mesh%totalnodes()

CALL display( 'INITIATING DOMAIN' )
!--------------------------------------
CALL gmsh%model%mesh%getElements( dom )

CALL Display( "HEAT TRANSFER THROUGH POROUS MEDIA")
CALL display( '    initiate')
CALL HeatTransPM%initiate( NSD=NSD, dt=dt, tn=tn, NTS=NTS, tol_res=tol_res, &
  & tol_sol=tol_sol, SpatialCoordType=HeatTransPM%TwoD_Cartesian)
CALL display( '    set domain')
CALL HeatTransPM%setDomain( dom=dom, omegano=[1] )
CALL display( '    set algorithm')
CALL HeatTransPM%setAlgorithm(mainOption=[HeatTransPM%Transient, &
  & HeatTransPM%noFlux, HeatTransPM%noPhaseChange])
CALL display( '    set materials')

! User_VolHeatCap => UserVolHeatCap
! HeatTransPM%Soils(1) = Soil( &
!   & ThermCondModel=TypeSoil%ConstantThermCond, &
!   & ThermCondVal = 2.0_DFP, &
!   & volHeatCapModel=TypeSoil%UserVolHeatCap, &
!   & UserVolHeatCap = User_VolHeatCap, &
!   & volFrac_solid= 0.6_DFP, &
!   & volFrac_water=0.40_DFP )

HeatTransPM%Soils(1) = Soil( &
  & ThermCondModel=TypeSoil%ConstantThermCond, &
  & ThermCondVal = 2.0_DFP, &
  & volHeatCapModel=TypeSoil%ConstantVolHeatCap, &
  & volHeatCapVal = 1.0D+6, &
  & volFrac_solid= 0.6_DFP, &
  & volFrac_water=0.40_DFP )

CALL HeatTransPM%setMaterial(materialNo = [1])

CALL display( '    apply initial condition')
CALL HeatTransPM%applyInitCondition(Val=[293.15_DFP])
CALL display( '    final setup of kernel')
CALL HeatTransPM%setBoundary(DB=[BOTTOM], DB_Point=[52])
CALL HeatTransPM%setKernel()

DO its = 1, NTS
  DO iter = 1, MaxIter
    CALL HeatTransPM%applyDBC(tag=BOTTOM, Val=[293.15_DFP])
    CALL HeatTransPM%applyDBC(Nptrs=[52], Val=[373.15_DFP])
    CALL HeatTransPM%assemble()
    CALL HeatTransPM%solve()
    convgInSol = HeatTransPM%isConverged(relTol=tol_sol, convergeInSol=.TRUE.)
    convgInRes = HeatTransPM%isConverged(relTol=tol_res, convergeInRes=.TRUE.)
    CALL blankLines()
    CALL blankLines()
    CALL display([its, iter], "time step, iteration :: ")
    CALL Display( [HeatTransPM%err, HeatTransPM%err0], " err, err0 :: " )
    CALL Display( [HeatTransPM%err_res, HeatTransPM%err0_res], &
      & " err_res, err0_res :: " )
    IF( convgInSol .OR. convgInRes ) THEN
      CALL display(" Convergence achieved ")
      CALL display(iter, "converged in total steps :: ")
      CALL HeatTransPM%update( reset=.false.)
      EXIT
    END IF
  END DO

  IF( MOD(its, 5) .EQ. 0 ) THEN
    CALL HeatTransPM%writedata( &
      & gmsh=gmsh, &
      & path="./HeatTransPM/", &
      & filename="heatTransPM",&
      & extension=".vtu", indx=[its] )
  END IF

END DO

CONTAINS
PURE FUNCTION UserVolHeatCap( Obj, volFrac_solid, volFrac_water, &
  & volFrac_ice, volFrac_air, Temp, x, y, z ) RESULT( Ans )

  CLASS( volHeatCapModel_ ), INTENT( IN ) :: Obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_solid
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_water
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_ice
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_air
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: x
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: y
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: z
  REAL( DFP ) :: Ans

  IF( Temp .GE. 273.15_DFP ) THEN
    Ans = 2.0E+6
  ELSE
    Ans = 2.0E+6 + (334000.0_DFP * 1.0E+3 * 1.6_DFP * 0.1 * 0.5) * ABS(Temp-273.15_DFP)**(-1.5_DFP)
  END IF
END FUNCTION
end program main