program main
use basemethod
use basetype


!block
  ! type( elemshapedata_ ) :: test, trial
  ! type( quadraturepoint_ ) :: quad
  ! type( referenceline_ ) :: refelem
  ! type( fevariable_ ) :: rho
  ! real( dfp ), allocatable :: mat( :, : ), XiJ( :, : )

  ! XiJ = RESHAPE( [-1, 1], [1, 2])
  ! refelem = referenceline( 1 )
  ! quad = GaussLegendreQuadrature( refelem, 2 )
  ! call initiate( test, quad, refelem, typeH1, typeLagrangeInterpolation, XiJ )
  ! rho  = quadraturevariable( 1.0_DFP, typefevariablescalar, typefevariableconstant )
  ! mat = massmatrix( test = test, trial = test, rho = rho, ncopy = 3 )
  ! call display( mat, "mat" )
!end block

block
  type( elemshapedata_ ) :: test, trial
  type( quadraturepoint_ ) :: quad
  type( referenceTriangle_ ) :: refelem
  type( fevariable_ ) :: rho
  real( dfp ), allocatable :: mat( :, : ), XiJ( :, : )

  XiJ = RESHAPE( [ 1, 0, 0, 1, 0, 0 ], [2, 3])
  refelem = referenceTriangle( 2 )
  quad = GaussLegendreQuadrature( refelem, 2 )
  call initiate( test, quad, refelem, typeH1, typeLagrangeInterpolation, XiJ )
  rho  = quadraturevariable( 1.0_DFP, typefevariablescalar, typefevariableconstant )
  mat = massmatrix( test = test, trial = test, rho = rho, ncopy = 1 )
  call display( mat, "mat" )
end block

end program main