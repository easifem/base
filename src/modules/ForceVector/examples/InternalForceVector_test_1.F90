!> author: Shion Shimizu
! date: 2026-11-02
! summary: InternalForceVector_ from the stress at quadrature points
!
! For a linear elastic material the internal force of the stress
! sigma = C B u equals K u. The element is a box shaped linear element
! (Q4 in 2D, H8 in 3D) built by hand with 2 Gauss points in each
! direction.

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE ForceVector_Method, ONLY: InternalForceVector_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE StiffnessMatrix_Method, ONLY: StiffnessMatrix_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: lambda = 1.3_DFP
REAL(DFP), PARAMETER :: mu = 0.7_DFP
REAL(DFP), PARAMETER :: hbox(3) = [2.0_DFP, 1.0_DFP, 0.5_DFP]
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: 2D, internal force equals K u
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  REAL(DFP) :: K(8, 8), u(4, 2), stress(3, 4), fint(4, 2), ku(8)
  INTEGER(I4B) :: nrow, ncol

  CALL BoxElement(2, hbox(1:2), elemsd)
  CALL StiffnessMatrix_(test=elemsd, trial=elemsd, lambda=lambda, &
                        mu=mu, ans=K, nrow=nrow, ncol=ncol)
  CALL Displacement(4, 2, u)
  CALL LinearStress(elemsd, u, stress)

  CALL InternalForceVector_(test=elemsd, stress=stress, ans=fint, &
                            nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 4 .AND. ncol .EQ. 2, "2D: size of fint is 4x2")

  ku = MATMUL(K, RESHAPE(u, [8]))
  CALL OK(MAXVAL(ABS(RESHAPE(fint, [8]) - ku)) &
          .LE. tol * MAXVAL(ABS(ku)), "2D: fint equals K u")
END BLOCK

! Test 2: 3D, internal force equals K u
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  REAL(DFP) :: K(24, 24), u(8, 3), stress(6, 8), fint(8, 3), ku(24)
  INTEGER(I4B) :: nrow, ncol

  CALL BoxElement(3, hbox(1:3), elemsd)
  CALL StiffnessMatrix_(test=elemsd, trial=elemsd, lambda=lambda, &
                        mu=mu, ans=K, nrow=nrow, ncol=ncol)
  CALL Displacement(8, 3, u)
  CALL LinearStress(elemsd, u, stress)

  CALL InternalForceVector_(test=elemsd, stress=stress, ans=fint, &
                            nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 8 .AND. ncol .EQ. 3, "3D: size of fint is 8x3")

  ku = MATMUL(K, RESHAPE(u, [24]))
  CALL OK(MAXVAL(ABS(RESHAPE(fint, [24]) - ku)) &
          .LE. tol * MAXVAL(ABS(ku)), "3D: fint equals K u")
END BLOCK

! Test 3: scale and addContribution, fint - fint = 0
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  REAL(DFP) :: u(8, 3), stress(6, 8), fint(8, 3)
  INTEGER(I4B) :: nrow, ncol

  CALL BoxElement(3, hbox(1:3), elemsd)
  CALL Displacement(8, 3, u)
  CALL LinearStress(elemsd, u, stress)

  CALL InternalForceVector_(test=elemsd, stress=stress, ans=fint, &
                            nrow=nrow, ncol=ncol)
  CALL InternalForceVector_(test=elemsd, stress=stress, ans=fint, &
                            nrow=nrow, ncol=ncol, scale=-1.0_DFP, &
                            addContribution=.TRUE.)
  CALL OK(MAXVAL(ABS(fint)) .LE. tol, &
          "3D: scale=-1 with addContribution cancels fint")
END BLOCK

CONTAINS

! Smooth nodal displacement u(I, i)
SUBROUTINE Displacement(nns, nsd, u)
  INTEGER(I4B), INTENT(IN) :: nns, nsd
  REAL(DFP), INTENT(OUT) :: u(nns, nsd)
  INTEGER(I4B) :: ia, jj

  DO jj = 1, nsd
    DO ia = 1, nns
      u(ia, jj) = SIN(REAL(ia + 3 * jj, DFP))
    END DO
  END DO
END SUBROUTINE Displacement

! Voigt stress sigma = C eps of an isotropic material at each point
SUBROUTINE LinearStress(elemsd, u, stress)
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd
  REAL(DFP), INTENT(IN) :: u(:, :)
  REAL(DFP), INTENT(OUT) :: stress(:, :)
  REAL(DFP) :: grad(3, 3), eps(3, 3), trEps
  INTEGER(I4B) :: nsd, ips, ii, jj

  nsd = elemsd%nsd
  DO ips = 1, elemsd%nips
    DO jj = 1, nsd
      DO ii = 1, nsd
        grad(ii, jj) = DOT_PRODUCT(u(:, ii), elemsd%dNdXt(:, jj, ips))
      END DO
    END DO
    eps(1:nsd, 1:nsd) = 0.5_DFP * (grad(1:nsd, 1:nsd) &
                                   + TRANSPOSE(grad(1:nsd, 1:nsd)))
    trEps = 0.0_DFP
    DO ii = 1, nsd
      trEps = trEps + eps(ii, ii)
    END DO
    eps(1:nsd, 1:nsd) = 2.0_DFP * mu * eps(1:nsd, 1:nsd)
    DO ii = 1, nsd
      eps(ii, ii) = eps(ii, ii) + lambda * trEps
    END DO
    IF (nsd .EQ. 2) THEN
      stress(:, ips) = [eps(1, 1), eps(2, 2), eps(1, 2)]
    ELSE
      stress(:, ips) = [eps(1, 1), eps(2, 2), eps(3, 3), eps(1, 2), &
                        eps(2, 3), eps(1, 3)]
    END IF
  END DO
END SUBROUTINE LinearStress

! Linear box element [0,h1]x[0,h2](x[0,h3]) with 2-point Gauss rule
SUBROUTINE BoxElement(nsd, h, elemsd)
  INTEGER(I4B), INTENT(IN) :: nsd
  REAL(DFP), INTENT(IN) :: h(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
  REAL(DFP) :: gp(2), xi(3), sgn(3), dN(3), N0(3)
  INTEGER(I4B) :: nns, nips, ia, ips, node(3), kk, ii

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
    DO kk = 1, nsd
      xi(kk) = gp(MOD((ips - 1) / 2**(kk - 1), 2) + 1)
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
          IF (ii .NE. kk) THEN
            elemsd%dNdXt(ia, kk, ips) = elemsd%dNdXt(ia, kk, ips) * N0(ii)
          END IF
        END DO
      END DO
    END DO
  END DO
END SUBROUTINE BoxElement

END PROGRAM main
