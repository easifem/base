!> authors: Dr. Vikas Sharma
!
! test Loc_NearestPoint from utility

program main
use easifem

REAL( DFP ) :: Nodes( 2, 10 )
INTEGER( I4B ) :: iloc

CALL RANDOM_NUMBER( Nodes )
Nodes = Nodes * 10.0_DFP
CALL Display( Nodes, "Nodes :: ")
Nodes( :, 5 ) = [5.0_DFP, 5.0_DFP]

iloc = LOC_NearestPoint( Nodes, [5.0_DFP, 5.0_DFP])
CALL Display( iloc, "iloc:: ")

end program main