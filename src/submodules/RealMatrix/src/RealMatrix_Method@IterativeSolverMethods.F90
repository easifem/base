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
USE BaseType, ONLY: convOpt => TypeConvergenceOpt
USE BaseType, ONLY: math => TypeMathOpt
USE InputUtility, ONLY: Input
USE GlobalData, ONLY: maxI4B

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        CG
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LinearSolver_CG1
REAL(DFP) :: w(SIZE(rhs), 3)
CALL LinearSolver_CG(mat=mat, rhs=rhs, sol=sol, w=w, maxIter=maxIter, &
                     rtol=rtol, atol=atol, convergenceIn=convergenceIn, &
                     relativeToRHS=relativeToRHS, restartAfter=restartAfter)
END PROCEDURE obj_LinearSolver_CG1

!----------------------------------------------------------------------------
!                                                            LinearSolver_CG
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LinearSolver_CG2
REAL(DFP), PARAMETER :: default_atol = math%zero
REAL(DFP), PARAMETER :: default_rtol = 1.0E-6
INTEGER(I4B), PARAMETER :: default_maxiter = 10

REAL(DFP) :: alpha, beta, tol, pap, error0, error, rr1, rr2, &
             rtol0, atol0
INTEGER(I4B) :: maxiter0, ii, convIn, tsize
LOGICAL(LGT) :: isok, relativeToRHS0, recomputeRes

tsize = SIZE(rhs)

! temp storage of Ax0 @blas
w(1:tsize, 2) = MATMUL(mat(1:tsize, 1:tsize), sol(1:tsize))

! r0=b-Ax0 @blas
w(1:tsize, 1) = rhs(1:tsize) - w(1:tsize, 2)

! p0=r0 @blas
w(1:tsize, 2) = w(1:tsize, 1)

convIn = INPUT(option=convergenceIn, default=convOpt%res)

relativeToRHS0 = INPUT(option=relativeToRHS, default=math%no)

! tol
IF (relativeToRHS0) THEN
  ! rto*||b||+atol
  ! @blas
  tol = NORM2(rhs(1:tsize))

ELSE

  isok = convIn .EQ. convOpt%res

  IF (isok) THEN
    ! rtol*r0+atol
    ! @blas
    tol = NORM2(w(1:tsize, 1))
    error0 = tol

  ELSE

    ! rtol*dx0+atol
    ! @blas
    rr1 = DOT_PRODUCT(w(1:tsize, 1), w(1:tsize, 1))

    ! @blas
    w(1:tsize, 3) = MATMUL(mat(1:tsize, 1:tsize), w(1:tsize, 1))

    ! @blas
    pap = DOT_PRODUCT(w(1:tsize, 1), w(1:tsize, 3))

    alpha = rr1 / pap

    ! dx0=alpha||p0||
    error0 = SQRT(rr1)

    tol = ABS(alpha) * error0
  END IF
END IF

rtol0 = INPUT(default=default_rtol, option=rtol)
atol0 = INPUT(default=default_atol, option=atol)
tol = rtol0 * tol + atol0

! Check convergence
isok = convIn .EQ. convOpt%res
IF (isok) THEN
  IF (error0 .LE. tol) THEN
    RETURN
  END IF
END IF

! maxiter0
isok = PRESENT(maxiter)
maxiter0 = MIN(tsize, default_maxiter)
IF (isok) THEN
  maxiter0 = maxiter
  IF (maxiter .LT. 0) maxiter0 = maxI4B
END IF

! recomputeRes
isok = PRESENT(restartAfter)
recomputeRes = math%no
IF (isok) recomputeRes = math%yes

ii = 0

! Start iteration
DO
  !@blas
  rr1 = DOT_PRODUCT(w(1:tsize, 1), w(1:tsize, 1))

  !@blas
  w(1:tsize, 3) = MATMUL(mat(1:tsize, 1:tsize), w(1:tsize, 2))

  !@blas
  pap = DOT_PRODUCT(w(1:tsize, 2), w(1:tsize, 3))
  alpha = rr1 / pap

  ! increse the iteration
  ii = ii + 1

  ! update solution
  ! @blas
  sol(1:tsize) = sol(1:tsize) + alpha * w(1:tsize, 2)

  IF (recomputeRes) THEN

    isok = MOD(ii, restartAfter) .EQ. 0

    IF (isok) THEN
      ! temp storage of Ax @blas
      w(1:tsize, 3) = MATMUL(mat(1:tsize, 1:tsize), sol(1:tsize))
      w(1:tsize, 1) = rhs(1:tsize) - w(1:tsize, 3)
    END IF

  ELSE

    !@blas
    w(1:tsize, 1) = w(1:tsize, 1) - alpha * w(1:tsize, 3)

  END IF

  !@blas
  rr2 = DOT_PRODUCT(w(1:tsize, 1), w(1:tsize, 1))

  ! check convergence
  isok = convIn .EQ. convOpt%res
  IF (isok) THEN
    error = SQRT(rr2)
    IF ((error .LE. tol) .OR. (ii .GT. maxiter0)) EXIT
  ELSE
    error = alpha * NORM2(w(1:tsize, 2))
    ! BLAS
    IF ((error .LE. tol) .OR. (ii .GT. maxiter0)) EXIT
  END IF

  ! beta
  beta = rr2 / rr1

  ! update p
  ! @blas
  w(1:tsize, 2) = w(1:tsize, 1) + beta * w(1:tsize, 2)
END DO

END PROCEDURE obj_LinearSolver_CG2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IterativeSolverMethods
