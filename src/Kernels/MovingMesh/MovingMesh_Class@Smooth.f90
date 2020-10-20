SUBMODULE( MovingMesh_Class ) Smooth
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Smooth
!----------------------------------------------------------------------------

SUBROUTINE LaplaceSmoothing( Obj, maxiter, omegano, boundary, tol, measure )
  CLASS( MovingMesh_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: boundary(:), omegano(:)
  INTEGER( I4B ), INTENT( IN ) :: maxIter
  INTEGER( I4B ), INTENT( IN ) :: measure
  REAL( DFP ), INTENT( IN ) :: tol

  INTEGER( I4B ), ALLOCATABLE :: n2n( : )
  INTEGER( I4B ) :: ii, imesh, jj, kk, iter, ll
  REAL( DFP ), ALLOCATABLE :: dummy_nodes(:,:)
  REAL( DFP ) :: q(3)

  IF( PRESENT(omegano) ) THEN
    DO imesh = 1, SIZE(omegano)
      jj = omegano(imesh)
      CALL obj%dom%mdOmega(jj)%ptr%initiateBoundaryData(obj%dom%Omega(jj)%ptr)
      CALL obj%dom%mdOmega(jj)%ptr%initiateNodeToNodes(obj%dom%Omega(jj)%ptr)
    END DO
  END IF

  obj%SmoothNodes=obj%dom%nodes
  dummy_nodes = obj%dom%nodes
  obj%smoothing = .TRUE.

  DO iter = 1, maxiter

    IF( PRESENT( omegano ) ) THEN
      DO imesh = 1, SIZE(omegano)

        jj = omegano(imesh)

        DO ii = 1, obj%dom%mdOmega(jj)%ptr%tNodes
          kk = obj%dom%mdOmega(jj)%ptr%nptrs(ii)
          n2n = obj%dom%mdOmega(jj)%ptr%nodeToNodes( &
            & GlobalNode=kk, &
            & includeSelf = .false. )

          DO ll = 1, obj%nsd
            obj%SmoothNodes(ll, kk) = SUM(dummy_nodes(ll, n2n)) / SIZE(n2n)
          END DO

        END DO

        DO ii = 1, obj%dom%mdOmega(jj)%ptr%totalBoundaryNodes()
          kk = obj%dom%mdOmega(jj)%ptr%boundaryNptrs(ii)
          obj%SmoothNodes(:, kk) = obj%dom%nodes(:,kk)
        END DO
      END DO
    END IF

    IF( PRESENT( boundary ) ) THEN
      DO imesh = 1, SIZE(boundary)

        jj = boundary(imesh)
        call obj%dom%mdboundary(jj)%ptr%InitiateNodeToNodes(&
          & obj%dom%boundary(jj)%ptr)
        call obj%dom%mdboundary(jj)%ptr%initiateBoundaryData(&
          & obj%dom%boundary(jj)%ptr)

        DO ii = 1, obj%dom%mdboundary(jj)%ptr%tNodes
          kk = obj%dom%mdboundary(jj)%ptr%nptrs(ii)
          n2n = obj%dom%mdboundary(jj)%ptr%nodeToNodes( &
            & GlobalNode=kk, &
            & includeSelf = .false. )

          DO ll = 1, obj%nsd
            obj%SmoothNodes(ll, kk) = SUM(dummy_nodes(ll, n2n)) / SIZE(n2n)
          END DO

        END DO

        DO ii = 1, obj%dom%mdboundary(jj)%ptr%totalBoundaryNodes()
          kk = obj%dom%mdboundary(jj)%ptr%boundaryNptrs(ii)
          obj%SmoothNodes(:, kk) = obj%dom%nodes(:,kk)
        END DO

      END DO
    END IF

    ! calculate mesh quality
    CALL obj%getMeshQuality( qmin=q(1), qmax=q(2), qavg=q(3), &
      & measure=measure, nodes=obj%SmoothNodes )

    IF( q(2) .LE. tol ) THEN
      EXIT
    ELSE
      dummy_nodes = obj%SmoothNodes
    END IF

  END DO

  DEALLOCATE( dummy_nodes, n2n )

END SUBROUTINE LaplaceSmoothing

!----------------------------------------------------------------------------
!                                                                 Smooth
!----------------------------------------------------------------------------

MODULE PROCEDURE mmt_smooth
  INTEGER( I4B ) :: measure
  REAL( DFP ) :: tol
  LOGICAL( LGT ) :: isb, iso

  tol = 1.5
  measure = QualityMeasure%AspectRatio

  IF( PRESENT(boundary) ) THEN
    isb = .TRUE.
  ELSE
    isb = .FALSE.
  END IF

  IF( PRESENT(omegano) ) THEN
    iso = .TRUE.
  ELSE
    iso = .FALSE.
  END IF

  IF( PRESENT(mainOption) ) THEN
    IF( mainOption(1) .EQ. Obj%Laplace ) THEN

      IF( isb .AND. iso ) THEN
        CALL LaplaceSmoothing(obj=obj, maxiter=maxiter, boundary=boundary, &
          & omegano=omegano, tol=tol, measure=measure)
        RETURN
      END IF

      IF( isb ) THEN
        CALL LaplaceSmoothing(obj=obj, maxiter=maxiter, boundary=boundary, &
          & tol=tol, measure=measure)
        RETURN
      END IF

      IF( iso ) THEN
        CALL LaplaceSmoothing(obj=obj, maxiter=maxiter, omegano=omegano, &
          & tol=tol, measure=measure)
        RETURN
      END IF
    END IF

  ELSE

    IF( isb .AND. iso ) THEN
      CALL LaplaceSmoothing(obj=obj, maxiter=maxiter, boundary=boundary, &
        & omegano=omegano, tol=tol, measure=measure)
      RETURN
    END IF

    IF( isb ) THEN
      CALL LaplaceSmoothing(obj=obj, maxiter=maxiter, boundary=boundary, &
        & tol=tol, measure=measure)
      RETURN
    END IF

    IF( iso ) THEN
      CALL LaplaceSmoothing(obj=obj, maxiter=maxiter, omegano=omegano, &
        & tol=tol, measure=measure)
      RETURN
    END IF

  END IF

END PROCEDURE mmt_smooth

END SUBMODULE Smooth