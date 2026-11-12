!> author: Shion Shimizu
! date: 2026-11-02
! summary: Test of StiffnessMatrix_ with tangent at quadrature points and
! InternalForceVector_
!
! The element data of a box shaped linear element (Q4 in 2D, H8 in 3D) is
! constructed by hand, with 2 Gauss points in each direction.
!
! Checks:
! 1. StiffnessMatrix_(Cijkl(:,:,:)) equals StiffnessMatrix_(lambda, mu)
!    when Cijkl is the isotropic tangent at every quadrature point.
!    The 2D tangent is obtained by ReduceVoigtTangent_ (plane strain).
! 2. InternalForceVector_ with stress = C B u equals K u.

PROGRAM main
USE GlobalData, ONLY: DFP, I4B, LGT, VoigtFormPlaneStrain, VoigtForm3D
USE BaseType, ONLY: ElemShapeData_
USE StiffnessMatrix_Method, ONLY: StiffnessMatrix_
USE ForceVector_Method, ONLY: InternalForceVector_
USE MaterialJacobian_Method, ONLY: IsotropicTangent, ReduceVoigtTangent_
IMPLICIT NONE

CALL RunTest(nsd=2)
CALL RunTest(nsd=3)

CONTAINS

SUBROUTINE RunTest(nsd)
  INTEGER(I4B), INTENT(IN) :: nsd
  TYPE(ElemShapeData_) :: elemsd
  REAL(DFP), PARAMETER :: lambda = 1.3_DFP, mu = 0.7_DFP
  REAL(DFP), PARAMETER :: hbox(3) = [2.0_DFP, 1.0_DFP, 0.5_DFP]
  REAL(DFP) :: C6(6, 6), Cv(6, 6), err
  REAL(DFP), ALLOCATABLE :: Cq(:, :, :), K1(:, :), K2(:, :), &
                            u(:, :), uvec(:), stress(:, :), fint(:, :), &
                            grad(:, :), eps(:)
  INTEGER(I4B) :: nns, nips, nv, nrow, ncol, ips, ii, jj, ia, voigtForm

  CALL BoxElement(nsd, hbox(1:nsd), elemsd)
  nns = elemsd%nns
  nips = elemsd%nips
  nv = nsd * (nsd + 1) / 2

  voigtForm = VoigtForm3D
  IF (nsd .EQ. 2) voigtForm = VoigtFormPlaneStrain

  C6 = IsotropicTangent(lambda=lambda, mu=mu)
  CALL ReduceVoigtTangent_(C=C6, voigtForm=voigtForm, ans=Cv, nrow=nrow, &
                           ncol=ncol)
  IF (nrow .NE. nv) STOP "ReduceVoigtTangent_ wrong size"

  ALLOCATE (Cq(nv, nv, nips), K1(nns * nsd, nns * nsd), &
            K2(nns * nsd, nns * nsd))
  DO ips = 1, nips
    Cq(:, :, ips) = Cv(1:nv, 1:nv)
  END DO

  CALL StiffnessMatrix_(test=elemsd, trial=elemsd, Cijkl=Cq, ans=K1, &
                        nrow=nrow, ncol=ncol)
  CALL StiffnessMatrix_(test=elemsd, trial=elemsd, lambda=lambda, mu=mu, &
                        ans=K2, nrow=nrow, ncol=ncol)
  err = MAXVAL(ABS(K1 - K2)) / MAXVAL(ABS(K2))
  WRITE (*, "(A,I1,A,ES12.4)") "nsd=", nsd, &
    " | K(Cijkl qp) - K(lambda,mu) | / |K| = ", err
  IF (err .GT. 1.0E-12) STOP "FAILED: StiffnessMatrix_ with Cijkl(:,:,:)"

  ! internal force
  ALLOCATE (u(nns, nsd), uvec(nns * nsd), stress(nv, nips), &
            fint(nns, nsd), grad(nsd, nsd), eps(nv))
  DO jj = 1, nsd
    DO ia = 1, nns
      u(ia, jj) = SIN(REAL(ia + 3 * jj, DFP))
    END DO
  END DO
  uvec = RESHAPE(u, [nns * nsd])

  DO ips = 1, nips
    ! grad(i, j) = du_i / dx_j
    DO jj = 1, nsd
      DO ii = 1, nsd
        grad(ii, jj) = DOT_PRODUCT(u(:, ii), elemsd%dNdXt(:, jj, ips))
      END DO
    END DO
    IF (nsd .EQ. 2) THEN
      eps = [grad(1, 1), grad(2, 2), grad(1, 2) + grad(2, 1)]
    ELSE
      eps = [grad(1, 1), grad(2, 2), grad(3, 3), grad(1, 2) + grad(2, 1), &
             grad(2, 3) + grad(3, 2), grad(1, 3) + grad(3, 1)]
    END IF
    stress(:, ips) = MATMUL(Cv(1:nv, 1:nv), eps)
  END DO

  CALL InternalForceVector_(test=elemsd, stress=stress, ans=fint, &
                            nrow=nrow, ncol=ncol)
  IF (nrow .NE. nns .OR. ncol .NE. nsd) STOP "InternalForceVector_ size"

  err = MAXVAL(ABS(RESHAPE(fint, [nns * nsd]) - MATMUL(K2, uvec))) / &
        MAXVAL(ABS(MATMUL(K2, uvec)))
  WRITE (*, "(A,I1,A,ES12.4)") "nsd=", nsd, &
    " | fint - K u | / |K u| = ", err
  IF (err .GT. 1.0E-12) STOP "FAILED: InternalForceVector_"

  ! addContribution and scale
  CALL InternalForceVector_(test=elemsd, stress=stress, ans=fint, &
                            nrow=nrow, ncol=ncol, scale=-1.0_DFP, &
                            addContribution=.TRUE.)
  IF (MAXVAL(ABS(fint)) .GT. 1.0E-12) STOP "FAILED: addContribution"

  WRITE (*, "(A,I1,A)") "nsd=", nsd, " PASSED"
END SUBROUTINE RunTest

! Linear box element [0,h1]x[0,h2](x[0,h3]) with 2-point Gauss rule
SUBROUTINE BoxElement(nsd, h, elemsd)
  INTEGER(I4B), INTENT(IN) :: nsd
  REAL(DFP), INTENT(IN) :: h(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
  REAL(DFP) :: gp(2), xi(3), sgn(3), dN(3), N0(3)
  INTEGER(I4B) :: nns, nips, ia, ips, ig(3), node(3), kk, ii

  gp = [-1.0_DFP, 1.0_DFP] / SQRT(3.0_DFP)
  nns = 2**nsd
  nips = 2**nsd
  elemsd%nsd = nsd
  elemsd%xidim = nsd
  elemsd%nns = nns
  elemsd%nips = nips
  ALLOCATE (elemsd%N(nns, nips), elemsd%dNdXt(nns, nsd, nips), &
            elemsd%ws(nips), elemsd%js(nips), elemsd%thickness(nips))
  elemsd%thickness = 1.0_DFP
  elemsd%ws = 1.0_DFP
  elemsd%js = PRODUCT(h(1:nsd) / 2.0_DFP)

  DO ips = 1, nips
    ig = 1
    DO kk = 1, nsd
      ig(kk) = MOD((ips - 1) / 2**(kk - 1), 2) + 1
      xi(kk) = gp(ig(kk))
    END DO
    DO ia = 1, nns
      DO kk = 1, nsd
        node(kk) = MOD((ia - 1) / 2**(kk - 1), 2)
        sgn(kk) = REAL(2 * node(kk) - 1, DFP)
        N0(kk) = 0.5_DFP * (1.0_DFP + sgn(kk) * xi(kk))
        dN(kk) = 0.5_DFP * sgn(kk)
      END DO
      elemsd%N(ia, ips) = PRODUCT(N0(1:nsd))
      DO kk = 1, nsd
        elemsd%dNdXt(ia, kk, ips) = dN(kk) * 2.0_DFP / h(kk)
        DO ii = 1, nsd
          IF (ii .NE. kk) elemsd%dNdXt(ia, kk, ips) = &
            elemsd%dNdXt(ia, kk, ips) * N0(ii)
        END DO
      END DO
    END DO
  END DO
END SUBROUTINE BoxElement

END PROGRAM main
