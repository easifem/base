!> author: Shion Shimizu
! date: 2026-11-02
! summary: StiffnessMatrix_ with a Voigt tangent at quadrature points
!
! The element is a box shaped linear element (Q4 in 2D, H8 in 3D) built
! by hand with 2 Gauss points in each direction.

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: ReduceVoigtTangent_
USE StiffnessMatrix_Method, ONLY: StiffnessMatrix_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: lambda = 1.3_DFP
REAL(DFP), PARAMETER :: mu = 0.7_DFP
REAL(DFP), PARAMETER :: hbox(3) = [2.0_DFP, 1.0_DFP, 0.5_DFP]
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: 2D, isotropic plane strain tangent gives K(lambda, mu)
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  REAL(DFP) :: Cv(6, 6), Cq(3, 3, 4), K1(8, 8), K2(8, 8)
  INTEGER(I4B) :: nrow, ncol, ips

  CALL BoxElement(2, hbox(1:2), elemsd)
  CALL ReduceVoigtTangent_(C=IsotropicTangent(lambda=lambda, mu=mu), &
                           voigtForm=VoigtFormPlaneStrain, ans=Cv, &
                           nrow=nrow, ncol=ncol)
  DO ips = 1, 4
    Cq(:, :, ips) = Cv(1:3, 1:3)
  END DO

  CALL StiffnessMatrix_(test=elemsd, trial=elemsd, Cijkl=Cq, ans=K1, &
                        nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 8 .AND. ncol .EQ. 8, "2D: size of K is 8x8")

  CALL StiffnessMatrix_(test=elemsd, trial=elemsd, lambda=lambda, &
                        mu=mu, ans=K2, nrow=nrow, ncol=ncol)
  CALL OK(MAXVAL(ABS(K1 - K2)) .LE. tol * MAXVAL(ABS(K2)), &
          "2D: K(Cijkl) equals K(lambda, mu)")
END BLOCK

! Test 2: 3D, isotropic tangent gives K(lambda, mu)
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  REAL(DFP) :: Cv(6, 6), Cq(6, 6, 8), K1(24, 24), K2(24, 24)
  INTEGER(I4B) :: nrow, ncol, ips

  CALL BoxElement(3, hbox(1:3), elemsd)
  CALL ReduceVoigtTangent_(C=IsotropicTangent(lambda=lambda, mu=mu), &
                           voigtForm=VoigtForm3D, ans=Cv, nrow=nrow, &
                           ncol=ncol)
  DO ips = 1, 8
    Cq(:, :, ips) = Cv
  END DO

  CALL StiffnessMatrix_(test=elemsd, trial=elemsd, Cijkl=Cq, ans=K1, &
                        nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 24 .AND. ncol .EQ. 24, "3D: size of K is 24x24")

  CALL StiffnessMatrix_(test=elemsd, trial=elemsd, lambda=lambda, &
                        mu=mu, ans=K2, nrow=nrow, ncol=ncol)
  CALL OK(MAXVAL(ABS(K1 - K2)) .LE. tol * MAXVAL(ABS(K2)), &
          "3D: K(Cijkl) equals K(lambda, mu)")
END BLOCK

! Test 3: 2D, non-symmetric tangent that changes per quadrature point
BLOCK
  TYPE(ElemShapeData_) :: elemsd
  REAL(DFP) :: Cq(3, 3, 4), K1(8, 8), K2(8, 8)
  INTEGER(I4B) :: nrow, ncol, ips, ii, jj

  CALL BoxElement(2, hbox(1:2), elemsd)
  DO ips = 1, 4
    DO jj = 1, 3
      DO ii = 1, 3
        Cq(ii, jj, ips) = COS(REAL(ii + 2 * jj + 5 * ips, DFP))
      END DO
    END DO
  END DO

  CALL StiffnessMatrix_(test=elemsd, trial=elemsd, Cijkl=Cq, ans=K1, &
                        nrow=nrow, ncol=ncol)
  CALL ReferenceStiffness(elemsd, Cq, K2)
  CALL OK(MAXVAL(ABS(K1 - K2)) .LE. tol * MAXVAL(ABS(K2)), &
          "2D: non-symmetric K(Cijkl) equals the reference K")
  CALL OK(MAXVAL(ABS(K1 - TRANSPOSE(K1))) .GT. 1.0E-3_DFP, &
          "2D: K of a non-symmetric tangent is non-symmetric")
END BLOCK

CONTAINS

! Reference K by the index formula
! K((i-1)*nns+I, (j-1)*nns+J) = sum w dN^I/dx_k C_ikjl dN^J/dx_l
SUBROUTINE ReferenceStiffness(elemsd, Cq, K)
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd
  REAL(DFP), INTENT(IN) :: Cq(:, :, :)
  REAL(DFP), INTENT(OUT) :: K(:, :)
  INTEGER(I4B) :: indx(2, 2), nns, ips, ii, jj, kk, ll, ia, ib
  REAL(DFP) :: wt

  indx = RESHAPE([1, 3, 3, 2], [2, 2])
  nns = elemsd%nns
  K = 0.0_DFP
  DO ips = 1, elemsd%nips
    wt = elemsd%ws(ips) * elemsd%js(ips) * elemsd%thickness(ips)
    DO ii = 1, 2
    DO jj = 1, 2
    DO kk = 1, 2
    DO ll = 1, 2
    DO ia = 1, nns
    DO ib = 1, nns
      K((ii - 1) * nns + ia, (jj - 1) * nns + ib) = &
        K((ii - 1) * nns + ia, (jj - 1) * nns + ib) &
        + wt * elemsd%dNdXt(ia, kk, ips) &
        * Cq(indx(ii, kk), indx(jj, ll), ips) * elemsd%dNdXt(ib, ll, ips)
    END DO
    END DO
    END DO
    END DO
    END DO
    END DO
  END DO
END SUBROUTINE ReferenceStiffness

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
