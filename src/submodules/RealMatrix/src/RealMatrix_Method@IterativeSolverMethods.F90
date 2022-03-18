! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!
SUBMODULE(RealMatrix_Method) IterativeSolverMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 CG
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_CG_1
  REAL( DFP ) :: alpha, beta, tol, pap, error0, error, rr1, rr2, bnorm
  REAL( DFP ) :: w(SIZE(rhs),3)
  REAL( DFP ), PARAMETER :: default_atol = 0.0_DFP
  REAL( DFP ), PARAMETER :: default_rtol = 1.0E-6
  INTEGER(I4B), PARAMETER :: default_maxiter = 10
  !! 1=r
  !! 2=p
  !! 3=Ap
  INTEGER( I4B ) :: maxiter0
  INTEGER( I4B ) :: ii
  INTEGER( I4B ) :: convIn
  LOGICAL( LGT ) :: recomputeRes
  !!
  !! temp storage of Ax0
  !!
  w(:,2) = MATMUL(mat, sol) !! BLAS
  !!
  !! r0=b-Ax0
  !!
  w(:,1) = rhs - w(:,2) !! BLAS
  !!
  !! p0=r0
  !!
  w(:,2) = w(:,1) !! BLAS
  !!
  convIn=INPUT(option=convergenceIn, default=convergenceInRes)
  !!
  !! tol
  !!
  IF( INPUT(option=relativeToRHS, default=.FALSE. ) ) THEN
    !!
    !! rto*||b||+atol
    !!
    tol = NORM2(rhs) !! BLAS
    !!
  ELSE
    IF( convIn .EQ. convergenceInRes ) THEN
      !!
      !! rtol*r0+atol
      !!
      tol=NORM2(w(:,1)) !! BLAS
      error0= tol
      !!
    ELSE
      !!
      !! rtol*dx0+atol
      !!
      rr1=DOT_PRODUCT(w(:,1),w(:,1)) !! BLAS
      w(:,3)=MATMUL(mat,w(:,1)) !! BLAS
      pap=DOT_PRODUCT(w(:,1),w(:,3)) !! BLAS
      alpha=rr1/pap
      !! dx0=alpha||p0||
      error0 = SQRT(rr1)
      tol=ABS(alpha)*error0
      !!
    END IF
  END IF
  !!
  tol = INPUT( default=default_rtol, option=rtol) * tol + INPUT( default=default_atol, option=atol)
  !!
  !! Check convergence
  !!
  IF( convIn .EQ. convergenceInRes ) THEN
     IF( error0 .LE. tol  ) THEN
        RETURN
     END IF
  END IF
  !!
  !! maxiter0
  !!
  IF(PRESENT(maxiter)) THEN
    !!
    IF(maxiter .LT. 0) THEN
      maxiter0 = maxI4B
    ELSE
      maxiter0 = maxiter
    END IF
    !!
  ELSE
    !!
    maxiter0=MIN(SIZE(rhs),default_maxiter)
    !!
  END IF
  !!
  !! recomputeRes
  !!
  IF( PRESENT( restartAfter ) ) THEN
    recomputeRes = .TRUE.
  ELSE
    recomputeRes = .FALSE.
  END IF
  !!
  !!
  !!
  ii=0
  !!
  !! Start iteration
  !!
  DO
    rr1=DOT_PRODUCT(w(:,1),w(:,1)) !! BLAS
    w(:,3)=MATMUL(mat,w(:,2)) !! BLAS
    pap=DOT_PRODUCT(w(:,2),w(:,3)) !! BLAS
    alpha=rr1/pap
    !!
    !! increse the iteration
    !!
    ii=ii+1
    !!
    !! update solution
    !!
    sol=sol+alpha*w(:,2) !! BLAS
    !!
    IF( recomputeRes ) THEN
      IF( MOD( ii, restartAfter ) .EQ. 0 ) THEN
        !! temp storage of Ax
        w(:,3)=MATMUL(mat,sol) !! BLAS
        w(:,1)=rhs-w(:,3) !! BLAS
      END IF
    ELSE
      w(:,1)=w(:,1)-alpha*w(:,3) !! BLAS
    END IF
    !!
    rr2=DOT_PRODUCT(w(:,1),w(:,1)) !! BLAS
    !!
    !! check convergence
    !!
    IF( convIn .EQ. convergenceInRes ) THEN
      error = SQRT(rr2)
      IF( (error .LE. tol) .OR. (ii .GT. maxiter0) ) EXIT
    ELSE
      error = alpha*NORM2(w(:,2))
      !! BLAS
      IF( (error .LE. tol) .OR. (ii .GT. maxiter0) ) EXIT
    END IF
    !!
    !! beta
    !!
    beta=rr2/rr1
    !!
    !! update p
    !!
    w(:,2)=w(:,1)+beta*w(:,2) !! BLAS
    !!
  END DO
  !!
END PROCEDURE realmat_CG_1

END SUBMODULE IterativeSolverMethods
