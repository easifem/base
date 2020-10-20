MODULE Projection_Class
USE EASIFEM
IMPLICIT NONE
PRIVATE

INTEGER( I4B ), PARAMETER :: SPACETIME_EXPLICIT_PARAM=100
INTEGER( I4B ), PARAMETER :: SPACETIME_IMPLICIT_PARAM=101
INTEGER( I4B ), PARAMETER :: SPACE_EXPLICIT_PARAM=200
INTEGER( I4B ), PARAMETER :: SPACE_IMPLICIT_PARAM=201

!----------------------------------------------------------------------------
!                                                               Projection_
!----------------------------------------------------------------------------

TYPE :: Projection_
  INTEGER( I4B ) :: SPACETIME_EXPLICIT=SPACETIME_EXPLICIT_PARAM
  INTEGER( I4B ) :: SPACETIME_IMPLICIT=SPACETIME_IMPLICIT_PARAM
  INTEGER( I4B ) :: SPACE_EXPLICIT=SPACE_EXPLICIT_PARAM
  INTEGER( I4B ) :: SPACE_IMPLICIT=SPACE_IMPLICIT_PARAM
  CONTAINS
    PROCEDURE, PUBLIC, PASS( Obj ) :: L2Projection => l2_project_quad_to_node
END TYPE Projection_

PUBLIC :: Projection_

!----------------------------------------------------------------------------
!                                                         ProjectionPointer_
!----------------------------------------------------------------------------

TYPE :: ProjectionPointer_
  CLASS(Projection_), POINTER :: Ptr=>NULL()
END TYPE ProjectionPointer_

PUBLIC :: ProjectionPointer_

!----------------------------------------------------------------------------
!                                                               L2Projection
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE l2_project_quad_to_node( Obj, omega, mdomega, omegano, &
  & nodes, quadVar, nodalVar, nodalDOF, rhs, rhsDOF, local_nptrs, algoType )
  CLASS( Projection_ ), INTENT( INOUT) :: Obj
  CLASS( MeshPointer_ ), INTENT( INOUT) :: omega(:)
  CLASS( MeshDataPointer_ ), INTENT( INOUT) :: mdomega(:)
  INTEGER( I4B ), INTENT( IN ) :: omegano(:)
  REAL( DFP ), INTENT( IN ) :: nodes(:,:)
  TYPE( QuadratureVariablesPointer_ ), INTENT( IN ) :: quadVar(:)
  REAL( DFP ), INTENT( INOUT ) :: nodalVar(:)
  TYPE( DOF_ ), INTENT( IN ) :: nodalDOF
  REAL( DFP ), INTENT( INOUT ) :: rhs(:)
  TYPE( DOF_ ), INTENT( IN ) :: rhsDOF
  INTEGER( I4B ), INTENT( IN ) :: local_nptrs(:)
  INTEGER( I4B ), INTENT( IN ) :: algoType
END SUBROUTINE l2_project_quad_to_node
END INTERFACE

END MODULE Projection_Class