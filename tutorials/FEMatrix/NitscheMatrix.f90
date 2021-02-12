program main
use easifem
implicit none

  class( element_ ), pointer :: cell
  class( facetelement_ ), pointer :: obj
  class( referenceelement_ ), pointer :: refelem
  type( quadraturepoint_ ) :: quad
  type( elemshapedata_ ) :: elemsd
  real( dfp ), allocatable :: xij( :, : )

  ! creating cell
  xiJ = reshape( [1., 0., 3.0, 0.0, 2., 1.], [2,3] )
  refelem => ReferenceTriangle_Pointer( NSD = 2 )
  cell => element_pointer( NPTRS = [1,2,3], Mat_Type = 1, RefElem = refelem )
  refelem => ReferenceLine_Pointer( NSD = 2 )
  obj => facetelement_pointer( nptrs = [2,3], Mat_type = 1, refelem =  refelem )
  obj % cell => cell
  call obj % setFacetLocalID( 2 )

  quad = GaussLegendreQuadrature( refelem = obj % refelem, &
    & order = 2 )
  call display( quad, 'quad')

  call obj % getElemShapeData( elemsd = elemsd, quad = quad, xiJ = xiJ, &
    & ContinuityType = TypeH1, InterpolType = TypeLagrangeInterpolation )
  call display( elemsd, 'elemsd' )

  ! block
  !   real( dfp ), allocatable :: mat( :, : )
  !   type( fevariable_ ) :: alpha, evec

  !   alpha = NodalVariable( 1.0_dfp, typeFEVariableScalar, &
  !     & typeFEVariableConstant )

  !   evec = QuadratureVariable( [1.0_dfp/sqrt(2.0), 1.0_dfp/sqrt(2.0)], &
  !     & typeFEVariableVector, typeFEVariableConstant )

  !   call display( alpha, 'alpha')
  !   call display( evec, 'evec')
  !   mat = NitscheMatrix( elemsd, elemsd, Alpha, Evec )
  !   call display( mat, 'mat')
  ! end block

  ! block
  !   real( dfp ), allocatable :: mat( :, : )
  !   type( fevariable_ ) :: lambda, mu, evec

  !   lambda = NodalVariable( 1.0_dfp, typeFEVariableScalar, &
  !     & typeFEVariableConstant )

  !   mu = NodalVariable( 1.0_dfp, typeFEVariableScalar, &
  !     & typeFEVariableConstant )

  !   evec = QuadratureVariable( [1.0_dfp/sqrt(2.0), 1.0_dfp/sqrt(2.0)], &
  !     & typeFEVariableVector, typeFEVariableConstant )

  !   call display( lambda, 'lambda')
  !   call display( mu, 'mu')
  !   call display( evec, 'evec')
  !   mat = NitscheMatrix( elemsd, elemsd, lambda, mu, Evec )
  !   call display( mat, 'mat')
  ! end block

end program main