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
REAL( DFP ), PARAMETER :: dt = 0.5 !! in sec
REAL( DFP ), PARAMETER :: tn = 0.0_DFP !! in sec
INTEGER( I4B ), PARAMETER :: NTS = 2000
INTEGER( I4B ), PARAMETER :: MaxIter = 20
INTEGER( I4B ), PARAMETER :: DATA_PRINT_FREQ = 10
REAL( DFP ), PARAMETER :: tol_res=1.0E-5
REAL( DFP ), PARAMETER :: tol_sol=1.0E-5
LOGICAL( LGT ) :: convgInSol
LOGICAL( LGT ) :: convgInRes
LOGICAL( LGT ) :: isconverge
PROCEDURE( UserVolHeatCap ), POINTER :: User_VolHeatCap
PROCEDURE( UserthermCond ), POINTER :: User_thermCond

TYPE( HeatTransferPM_ ) :: HeatTransPM
TYPE( Gmsh_ ) :: gmsh
TYPE( Domain_ ) :: dom

CALL Display( 'INITIATING GMSH OBJECT')

ii = gmsh%initialize(NSD)
ii = gmsh%open("./", "mesh", ".msh")
tNodes = gmsh%model%mesh%totalnodes()

CALL Display( 'INITIATING DOMAIN' )
CALL gmsh%model%mesh%getElements( dom )
CALL Display( "HEAT TRANSFER THROUGH POROUS MEDIA")
CALL Display( '    initiate')
CALL HeatTransPM%initiate( NSD=NSD, dt=dt, tn=tn, NTS=NTS, tol_res=tol_res, &
  & tol_sol=tol_sol, SpatialCoordType=HeatTransPM%TwoD_Cartesian)
CALL Display( '    set domain')
CALL HeatTransPM%setDomain( dom=dom, omegano=[1] )
CALL Display( '    set algorithm')
CALL HeatTransPM%setAlgorithm(mainOption=[HeatTransPM%Transient, &
  & HeatTransPM%noFlux, HeatTransPM%PhaseChange])
CALL Display( '    set materials')

User_VolHeatCap => UserVolHeatCap
User_thermCond => UserthermCond

HeatTransPM%Soils(1) = Soil( &
  & ThermCondModel=TypeSoil%UserThermCond, &
  & UserthermCond = User_thermCond, &
  & volHeatCapModel=TypeSoil%UserVolHeatCap, &
  & UserVolHeatCap = User_VolHeatCap, &
  & volFrac_solid= 0.6_DFP, &
  & volFrac_water=0.40_DFP, &
  & Porosity = 0.40_DFP, &
  & SFCCModel = TypeSoil%ExpSFCC, &
  & SFCC_Theta_r = 0.1_DFP, &
  & SFCC_Temp_l = 273.15_DFP, &
  & SFCC_Temp_s = 270.15_DFP, &
  & SFCC_Coeff = 4.0_DFP )

CALL HeatTransPM%setMaterial(materialNo = [1])
CALL Display( '    apply initial condition')
CALL HeatTransPM%applyInitCondition(Val=[300.15_DFP])
CALL Display( '    final setup of kernel')
CALL HeatTransPM%setBoundary(DB=[TOP, BOTTOM])
CALL HeatTransPM%applyDBC(tag=TOP, Val=[263.15_DFP])
CALL HeatTransPM%applyDBC(tag=BOTTOM, Val=[263.15_DFP])
CALL HeatTransPM%setKernel()

DO its = 1, NTS
  DO iter = 1, MaxIter
    CALL HeatTransPM%applyDBC(tag=TOP, Val=[263.15_DFP])
    CALL HeatTransPM%applyDBC(tag=BOTTOM, Val=[263.15_DFP])
    CALL HeatTransPM%assemble()
    CALL HeatTransPM%solve()
    convgInSol = HeatTransPM%isConverged(relTol=tol_sol, convergeInSol=.TRUE.)
    convgInRes = HeatTransPM%isConverged(relTol=tol_res, convergeInRes=.TRUE.)
      CALL blankLines(nol=2)
      CALL Display( [its, iter], "time step, iteration :: " )
      CALL Display( [HeatTransPM%err, HeatTransPM%err0], " err, err0 :: " )
      CALL Display( [HeatTransPM%err_res, HeatTransPM%err0_res], &
        & " err_res, err0_res :: " )
    isconverge = convgInSol .OR. convgInRes
    IF( isconverge ) THEN
      CALL Display( " Convergence achieved " )
      CALL Display( iter, "converged in total steps :: " )
      CALL HeatTransPM%update( reset = .false. )
      EXIT
    END IF
  END DO
  IF( MOD(its, DATA_PRINT_FREQ) .EQ. 0 ) THEN
    CALL HeatTransPM%writedata( &
      & gmsh=gmsh, &
      & path="./HeatTransPM/", &
      & filename="heatTransPM",&
      & extension=".vtu", indx=[its] )
  END IF
  IF(.not. isconverge) THEN
      CALL Display(its, "no convergence in its :: ")
      STOP
  END IF
END DO

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS
PURE FUNCTION UserthermCond( Obj, volFrac_solid, volFrac_water, &
  & volFrac_ice, volFrac_air, Temp, x, y, z ) RESULT( Ans )
  CLASS( ThermCondModel_ ), INTENT( IN ) :: Obj
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
    Ans = 0.5_DFP
    RETURN
  END IF

  IF( Temp .LE. 270.15_DFP ) THEN
    Ans = 2.0_DFP
    RETURN
  END IF

  Ans = 2.0_DFP + (0.5-2.0)/(273.15_DFP - 270.15_DFP) * (Temp-270.15_DFP)
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION UservolHeatCap( Obj, volFrac_solid, volFrac_water, &
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
    RETURN
  END IF

  IF( Temp .LE. 270.15_DFP ) THEN
    Ans = 1.0E+6
    RETURN
  END IF

  Ans = 1.0 + (2.0-1.0)/(273.15_dfp - 270.15_DFP) * (Temp-270.15_DFP)
  Ans = Ans*1.0E+6
END FUNCTION

end program main