SUBMODULE( HeatTransferPM_Class ) Constructor
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               setAlgorithm
!----------------------------------------------------------------------------

MODULE PROCEDURE setalgo
  INTEGER( I4B ) :: ii
  CHARACTER( LEN = 4 ) :: CaseNo

  IF( .NOT. PRESENT( mainOption ) ) THEN
    CALL display( "ERROR:: HeatTransferPM_Class@Constructor.f90" )
    CALL display( "        setAlgorithm()" )
    CALL display( "          mainOption should be present" )
    stop
  END IF

  IF( SIZE( mainOption ) .LT. 3 ) THEN
    CALL display( "ERROR:: HeatTransferPM_Class@Constructor.f90" )
    CALL display( "        setAlgorithm()" )
    CALL display( "          mainOption(1:3) should be specified" )
    stop
  END IF

  !! S == Static; T = Transient
  IF( mainOption( 1 ) .EQ.  ALGO_OPT_STATIC ) THEN
    CaseNo( 1:1 ) = 'S'
  ELSE
    CASENo( 1:1 ) = 'T'
  END IF

  !! F = FLUX; N= NO FLUX; W= Water; A=Air; B=Both Air Water; X=No Flux
  IF( mainOption( 2 ) .EQ. ALGO_OPT_FLUX ) THEN
    CaseNo( 2:2 ) = 'F'

    IF( .NOT. PRESENT( extraOption ) ) THEN
      CALL display( "ERROR:: HeatTransferPM_Class@Constructor.f90" )
      CALL display( "        setAlgorithm()" )
      CALL display( "          extraOption should be present" )
      CALL display( "           as flux option is activated" )
      stop
    END IF

    IF( extraOption( 1 ) .EQ.  ALGO_OPT_FLUX_WATER ) THEN
      CaseNo( 4:4 ) = 'W'
    ELSE IF( extraOption( 1 ) .EQ.  ALGO_OPT_FLUX_AIR ) THEN
      CaseNo( 4:4 ) = 'A'
    ELSE IF( extraOption( 1 ) .EQ.  ALGO_OPT_FLUX_WATER_AIR ) THEN
      CaseNo( 4:4 ) = 'B'
    END IF

  ELSE
    CaseNo( 2:2 ) = 'N'
    CaseNo( 4:4 ) = 'X'
  END IF

  !! P = Phase Change; Q=No Phase Change
  IF( mainOption( 3 ) .EQ. ALGO_OPT_PHASE_CHANGE ) THEN
    CaseNo( 3:3 ) = 'P'
  ELSE
    CaseNo( 3:3 ) = 'Q'
  END IF

    SELECT CASE( Obj%SpatialCoordType )
    CASE( NSD_1_HORIZONTAL )
      SELECT CASE( CaseNo )
      CASE( 'SFPW')
        CALL set_algo_NSD_1_H_SFPW(Obj)
      CASE( 'SFPA')
        CALL set_algo_NSD_1_H_SFPA(Obj)
      CASE( 'SFPB')
        CALL set_algo_NSD_1_H_SFPB(Obj)
      CASE( 'SFQW')
        CALL set_algo_NSD_1_H_SFQW(Obj)
      CASE( 'SFQA')
        CALL set_algo_NSD_1_H_SFQA(Obj)
      CASE( 'SFQB')
        CALL set_algo_NSD_1_H_SFQB(Obj)
      CASE( 'SNPX')
        CALL set_algo_NSD_1_H_SNPX(Obj)
      CASE( 'SNQX')
        CALL set_algo_NSD_1_H_SNQX(Obj)
      CASE( 'TFPW')
        CALL set_algo_NSD_1_H_TFPW(Obj)
      CASE( 'TFPA')
        CALL set_algo_NSD_1_H_TFPA(Obj)
      CASE( 'TFPB')
        CALL set_algo_NSD_1_H_TFPB(Obj)
      CASE( 'TFQW')
        CALL set_algo_NSD_1_H_TFQW(Obj)
      CASE( 'TFQA')
        CALL set_algo_NSD_1_H_TFQA(Obj)
      CASE( 'TFQB')
        CALL set_algo_NSD_1_H_TFQB(Obj)
      CASE( 'TNPX')
        CALL set_algo_NSD_1_H_TNPX(Obj)
      CASE( 'TNQX')
        CALL set_algo_NSD_1_H_TNQX(Obj)
      END SELECT
    CASE( NSD_1_VERTICAL )
      SELECT CASE( CaseNo )
      CASE( 'SFPW')
        CALL set_algo_NSD_1_V_SFPW(Obj)
      CASE( 'SFPA')
        CALL set_algo_NSD_1_V_SFPA(Obj)
      CASE( 'SFPB')
        CALL set_algo_NSD_1_V_SFPB(Obj)
      CASE( 'SFQW')
        CALL set_algo_NSD_1_V_SFQW(Obj)
      CASE( 'SFQA')
        CALL set_algo_NSD_1_V_SFQA(Obj)
      CASE( 'SFQB')
        CALL set_algo_NSD_1_V_SFQB(Obj)
      CASE( 'SNPX')
        CALL set_algo_NSD_1_V_SNPX(Obj)
      CASE( 'SNQX')
        CALL set_algo_NSD_1_V_SNQX(Obj)
      CASE( 'TFPW')
        CALL set_algo_NSD_1_V_TFPW(Obj)
      CASE( 'TFPA')
        CALL set_algo_NSD_1_V_TFPA(Obj)
      CASE( 'TFPB')
        CALL set_algo_NSD_1_V_TFPB(Obj)
      CASE( 'TFQW')
        CALL set_algo_NSD_1_V_TFQW(Obj)
      CASE( 'TFQA')
        CALL set_algo_NSD_1_V_TFQA(Obj)
      CASE( 'TFQB')
        CALL set_algo_NSD_1_V_TFQB(Obj)
      CASE( 'TNPX')
        CALL set_algo_NSD_1_V_TNPX(Obj)
      CASE( 'TNQX')
        CALL set_algo_NSD_1_V_TNQX(Obj)
      END SELECT
    CASE( NSD_2_CARTESIAN )
      SELECT CASE( CaseNo )
      CASE( 'SFPW')
        CALL set_algo_NSD_2_C_SFPW(Obj)
      CASE( 'SFPA')
        CALL set_algo_NSD_2_C_SFPA(Obj)
      CASE( 'SFPB')
        CALL set_algo_NSD_2_C_SFPB(Obj)
      CASE( 'SFQW')
        CALL set_algo_NSD_2_C_SFQW(Obj)
      CASE( 'SFQA')
        CALL set_algo_NSD_2_C_SFQA(Obj)
      CASE( 'SFQB')
        CALL set_algo_NSD_2_C_SFQB(Obj)
      CASE( 'SNPX')
        CALL set_algo_NSD_2_C_SNPX(Obj)
      CASE( 'SNQX')
        CALL set_algo_NSD_2_C_SNQX(Obj)
      CASE( 'TFPW')
        CALL set_algo_NSD_2_C_TFPW(Obj)
      CASE( 'TFPA')
        CALL set_algo_NSD_2_C_TFPA(Obj)
      CASE( 'TFPB')
        CALL set_algo_NSD_2_C_TFPB(Obj)
      CASE( 'TFQW')
        CALL set_algo_NSD_2_C_TFQW(Obj)
      CASE( 'TFQA')
        CALL set_algo_NSD_2_C_TFQA(Obj)
      CASE( 'TFQB')
        CALL set_algo_NSD_2_C_TFQB(Obj)
      CASE( 'TNPX')
        CALL set_algo_NSD_2_C_TNPX(Obj)
      CASE( 'TNQX')
        CALL set_algo_NSD_2_C_TNQX(Obj)
      END SELECT
    CASE( NSD_2_AXISYMMETRIC )
      SELECT CASE( CaseNo )
      CASE( 'SFPW')
        CALL set_algo_NSD_2_A_SFPW(Obj)
      CASE( 'SFPA')
        CALL set_algo_NSD_2_A_SFPA(Obj)
      CASE( 'SFPB')
        CALL set_algo_NSD_2_A_SFPB(Obj)
      CASE( 'SFQW')
        CALL set_algo_NSD_2_A_SFQW(Obj)
      CASE( 'SFQA')
        CALL set_algo_NSD_2_A_SFQA(Obj)
      CASE( 'SFQB')
        CALL set_algo_NSD_2_A_SFQB(Obj)
      CASE( 'SNPX')
        CALL set_algo_NSD_2_A_SNPX(Obj)
      CASE( 'SNQX')
        CALL set_algo_NSD_2_A_SNQX(Obj)
      CASE( 'TFPW')
        CALL set_algo_NSD_2_A_TFPW(Obj)
      CASE( 'TFPA')
        CALL set_algo_NSD_2_A_TFPA(Obj)
      CASE( 'TFPB')
        CALL set_algo_NSD_2_A_TFPB(Obj)
      CASE( 'TFQW')
        CALL set_algo_NSD_2_A_TFQW(Obj)
      CASE( 'TFQA')
        CALL set_algo_NSD_2_A_TFQA(Obj)
      CASE( 'TFQB')
        CALL set_algo_NSD_2_A_TFQB(Obj)
      CASE( 'TNPX')
        CALL set_algo_NSD_2_A_TNPX(Obj)
      CASE( 'TNQX')
        CALL set_algo_NSD_2_A_TNQX(Obj)
      END SELECT
    CASE( NSD_3 )
      SELECT CASE( CaseNo )
      CASE( 'SFPW')
        CALL set_algo_NSD_3_SFPW(Obj)
      CASE( 'SFPA')
        CALL set_algo_NSD_3_SFPA(Obj)
      CASE( 'SFPB')
        CALL set_algo_NSD_3_SFPB(Obj)
      CASE( 'SFQW')
        CALL set_algo_NSD_3_SFQW(Obj)
      CASE( 'SFQA')
        CALL set_algo_NSD_3_SFQA(Obj)
      CASE( 'SFQB')
        CALL set_algo_NSD_3_SFQB(Obj)
      CASE( 'SNPX')
        CALL set_algo_NSD_3_SNPX(Obj)
      CASE( 'SNQX')
        CALL set_algo_NSD_3_SNQX(Obj)
      CASE( 'TFPW')
        CALL set_algo_NSD_3_TFPW(Obj)
      CASE( 'TFPA')
        CALL set_algo_NSD_3_TFPA(Obj)
      CASE( 'TFPB')
        CALL set_algo_NSD_3_TFPB(Obj)
      CASE( 'TFQW')
        CALL set_algo_NSD_3_TFQW(Obj)
      CASE( 'TFQA')
        CALL set_algo_NSD_3_TFQA(Obj)
      CASE( 'TFQB')
        CALL set_algo_NSD_3_TFQB(Obj)
      CASE( 'TNPX')
        CALL set_algo_NSD_3_TNPX(Obj)
      CASE( 'TNQX')
        CALL set_algo_NSD_3_TNQX(Obj)
      END SELECT
    END SELECT

END PROCEDURE setalgo

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./setalgo/nsd_1_h_options.inc"
#include "./setalgo/nsd_1_v_options.inc"
#include "./setalgo/nsd_2_c_options.inc"
#include "./setalgo/nsd_2_a_options.inc"
#include "./setalgo/nsd_3_options.inc"

!----------------------------------------------------------------------------
!                                                                 setBoundary
!----------------------------------------------------------------------------

MODULE PROCEDURE setbc
  INTEGER( I4B ) :: ii

  ALLOCATE( Obj%DB(5), Obj%DBCinfo(5) )
  !! 1--> DBC
  !! 2--> Dirichlet Boundary Point
  !! 3--> robinB
  !! 4--> climateB

  Obj%DBCinfo = 0

  IF( PRESENT( DB ) ) THEN

    IF( .NOT. ALLOCATED( obj%dom%boundary ) ) THEN
      CALL display( "ERROR:: In ", __FILE__ )
      CALL display( __LINE__, "        Line " )
      CALL display( "        seepage_setbc()" )
      CALL display( "          dom % boundary NOT ALLOCATED" )
      STOP
    END IF

    IF( ANY( DB .GT. SIZE( obj%dom%boundary ) ) ) THEN
      CALL display( "ERROR:: In ", __FILE__ )
      CALL display( __LINE__, "        Line " )
      CALL display( "        setbc()" )
      CALL display( "          bound error due to uniformDB" )
      STOP
    END IF

    DO ii = 1, SIZE( DB )
      IF( .NOT. ASSOCIATED( obj%dom%boundary( DB( ii ) ) % ptr)) THEN
        CALL display( "ERROR:: In ", __FILE__ )
        CALL display( __LINE__, "        Line " )
        CALL display( "        setbc()" )
        CALL display( "          some dom%omega are not associated" )
        STOP
      END IF
    END DO

    DO ii = 1, SIZE( DB )
      IF( .NOT. ASSOCIATED( obj%dom%boundary( DB( ii ) )%ptr)) THEN
        CALL display( "ERROR:: In ", __FILE__ )
        CALL display( __LINE__, "        Line " )
        CALL display( "        setbc()" )
        CALL display( "          some dom%omega are not associated" )
        STOP
      END IF
    END DO

    Obj%DB(1)=IntVector(DB)
    Obj%dbcinfo(1)=1

  END IF

  IF( PRESENT( NB ) ) THEN

    IF( .NOT. ALLOCATED( obj%dom%boundary ) ) THEN
      CALL display( "ERROR:: In ", __FILE__ )
      CALL display( __LINE__, "        Line " )
      CALL display( "        setbc()" )
      CALL display( "          dom%boundary NOT ALLOCATED" )
      STOP
    END IF

    IF( ANY( robinB .GT. SIZE( obj%dom%boundary ) ) ) THEN
      CALL display( "ERROR:: In ", __FILE__ )
      CALL display( __LINE__, "        Line " )
      CALL display( "        setbc()" )
      CALL display( "          bound error due to robinB" )
      STOP
    END IF

    DO ii = 1, SIZE( NB )
      IF( .NOT. ASSOCIATED( obj%dom%boundary( NB( ii ) )%ptr ) ) THEN
        CALL display( "ERROR:: In ", __FILE__ )
        CALL display( __LINE__, "        Line " )
        CALL display( "        setbc()" )
        CALL display( "          some obj%dom%boundary() are not associated" )
        STOP
      END IF
    END DO
    obj % DB(2) = IntVector(NB)
    obj % dbcinfo(2) = 2
  END IF

  IF( PRESENT( robinB ) ) THEN

    IF( .NOT. ALLOCATED( obj%dom%boundary ) ) THEN
      CALL display( "ERROR:: In ", __FILE__ )
      CALL display( __LINE__, "        Line " )
      CALL display( "        setbc()" )
      CALL display( "          dom%boundary NOT ALLOCATED" )
      STOP
    END IF

    IF( ANY( robinB .GT. SIZE( obj%dom%boundary ) ) ) THEN
      CALL display( "ERROR:: In ", __FILE__ )
      CALL display( __LINE__, "        Line " )
      CALL display( "        setbc()" )
      CALL display( "          bound error due to robinB" )
      STOP
    END IF

    DO ii = 1, SIZE( robinB )
      IF( .NOT. ASSOCIATED( obj%dom%boundary( robinB( ii ) )%ptr ) ) THEN
        CALL display( "ERROR:: In ", __FILE__ )
        CALL display( __LINE__, "        Line " )
        CALL display( "        setbc()" )
        CALL display( "          some obj%dom%boundary() are not associated" )
        STOP
      END IF
    END DO
    obj % DB(3) = IntVector(robinB)
    obj % dbcinfo(3) = 3
  END IF

  IF( PRESENT( climateB ) ) THEN

    IF( .NOT. ALLOCATED( obj%dom%boundary ) ) THEN
      CALL display( "ERROR:: In ", __FILE__ )
      CALL display( __LINE__, "        Line " )
      CALL display( "        setbc()" )
      CALL display( "          dom%boundary NOT ALLOCATED" )
      STOP
    END IF

    IF( ANY( climateB .GT. SIZE( obj%dom%boundary ) ) ) THEN
      CALL display( "ERROR:: In ", __FILE__ )
      CALL display( __LINE__, "        Line " )
      CALL display( "        setbc()" )
      CALL display( "          bound error due to climateB" )
      STOP
    END IF

    DO ii = 1, SIZE( climateB )
      IF( .NOT. ASSOCIATED( obj%dom%boundary( climateB( ii ) )%ptr ) ) THEN
        CALL display( "ERROR:: In ", __FILE__ )
        CALL display( __LINE__, "        Line " )
        CALL display( "        setbc()" )
        CALL display( "          some obj%dom%boundary() are not associated" )
        STOP
      END IF
    END DO
    obj % DB(4) = IntVector(climateB)
    obj % dbcinfo(4) = 4
  END IF

  IF( PRESENT( DB_Point ) ) THEN
    obj % DB(5) = IntVector(DB_Point)
    obj % dbcinfo(5) = 5
  END IF

  IF( ALLOCATED( obj%intvec ) ) DEALLOCATE( obj%intvec )
  ALLOCATE( obj%intvec( obj%tdof ) )

  IF( obj%dbcinfo( 1 ) .NE. 0 ) THEN
    DO ii = 1, SIZE( DB )
      nptrs = obj%dom%mdboundary(DB(ii))%ptr%Nptrs
      CALL append( obj%intvec( 1 ), obj%local_nptrs(nptrs) )
    END DO
    CALL RemoveDuplicates( obj%intvec( 1 ) )
  END IF

  IF( obj%dbcinfo( 5 ) .NE. 0 ) THEN
    CALL append( obj%intvec( 1 ), obj%local_nptrs(DB_Point) )
    CALL RemoveDuplicates( obj%intvec( 1 ) )
  END IF

  CALL obj%linsol%setDirichletBCNodes( &
    & nptrs = obj%intvec, dofs = arange(start=1,end=obj%tdof) )

  DEALLOCATE( obj%intvec )

END PROCEDURE setbc

END SUBMODULE Constructor