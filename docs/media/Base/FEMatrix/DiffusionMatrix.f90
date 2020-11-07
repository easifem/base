program main
use basemethod
use basetype


! block
!   type( elemshapedata_ ) :: test, trial
!   type( quadraturepoint_ ) :: quad
!   type( referenceline_ ) :: refelem
!   type( fevariable_ ) :: rho
!   real( dfp ), allocatable :: mat( :, : ), XiJ( :, : )

!   XiJ = RESHAPE( [-1, 1], [1, 2])
!   refelem = referenceline( 1 )
!   quad = GaussLegendreQuadrature( refelem, 1 )
!   call initiate( test, quad, refelem, typeH1, typeLagrangeInterpolation, XiJ )
!   mat = Diffusionmatrix( test = test, trial = test )
!   call display( mat, "mat" )
! end block

! block
!   type( elemshapedata_ ) :: test, trial
!   type( quadraturepoint_ ) :: quad
!   type( referenceline_ ) :: refelem
!   type( fevariable_ ) :: rho
!   real( dfp ), allocatable :: mat( :, : ), XiJ( :, : )

!   XiJ = RESHAPE( [-1, 1], [1, 2])
!   refelem = referenceline( 1 )
!   quad = GaussLegendreQuadrature( refelem, 1 )
!   call initiate( test, quad, refelem, typeH1, typeLagrangeInterpolation, XiJ )
!   mat = Diffusionmatrix( test = test, trial = test, ncopy = 2 )
!   call display( mat, "mat" )
! end block

! block
!   type( elemshapedata_ ) :: test, trial
!   type( quadraturepoint_ ) :: quad
!   type( referenceTriangle_ ) :: refelem
!   type( fevariable_ ) :: rho
!   real( dfp ), allocatable :: mat( :, : ), XiJ( :, : )

!   XiJ = RESHAPE( [ 1, 0, 0, 1, 0, 0 ], [2, 3])
!   refelem = referenceTriangle( 2 )
!   quad = GaussLegendreQuadrature( refelem, 1 )
!   call initiate( test, quad, refelem, typeH1, typeLagrangeInterpolation, XiJ )

!   ! rho  = quadraturevariable( 1.0_DFP, typefevariablescalar, typefevariableconstant )
!   mat = Diffusionmatrix( test = test, trial = test )
!   call display( mat, "mat" )

! end block

! block
!   type( elemshapedata_ ) :: test, trial
!   type( quadraturepoint_ ) :: quad
!   type( referenceTriangle_ ) :: refelem
!   type( fevariable_ ) :: K
!   real( dfp ), allocatable :: mat( :, : ), XiJ( :, : ), Kmat( :, : )

!   XiJ = RESHAPE( [ 1, 0, 0, 1, 0, 0 ], [2, 3])
!   refelem = referenceTriangle( 2 )
!   quad = GaussLegendreQuadrature( refelem, 1 )
!   call initiate( test, quad, refelem, typeH1, typeLagrangeInterpolation, XiJ )

!   KMat = RESHAPE( [1,0,0,1], [2,2] )
!   K  = NODALvariable( Kmat, typefevariableMATRIX, typefevariableconstant )
!   mat = Diffusionmatrix( test, test, K )
!   call display( mat, "mat" )

! end block

block
  type( elemshapedata_ ) :: test, trial
  type( quadraturepoint_ ) :: quad
  type( referenceTriangle_ ) :: refelem
  type( fevariable_ ) :: K
  real( dfp ), allocatable :: mat( :, : ), XiJ( :, : ), Kmat( :, :, : )

  XiJ = RESHAPE( [ 1, 0, 0, 1, 0, 0 ], [2, 3])
  refelem = referenceTriangle( 2 )
  quad = GaussLegendreQuadrature( refelem, 1 )
  call initiate( test, quad, refelem, typeH1, typeLagrangeInterpolation, XiJ )

  ALLOCATE( Kmat( 2, 2, 3 ) )

  KMat( :, :, 1 ) = RESHAPE( [1,0,0,1], [2,2] )
  KMat( :, :, 2 ) = RESHAPE( [1,0,0,1], [2,2] )
  KMat( :, :, 3 ) = RESHAPE( [1,0,0,1], [2,2] )

  K  = NODALvariable( Kmat, typefevariableMATRIX, typefevariableSpace )
  mat = Diffusionmatrix( test, test, K )
  call display( mat, "mat" )

end block

end program main