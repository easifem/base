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

SUBMODULE(ElasticNitscheMatrix_Method) MatrixTangent
USE BaseMethod
IMPLICIT NONE

CONTAINS
!
! !----------------------------------------------------------------------------
! !                                             ElasticityNitscheMatrixTangent
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE ElasticNitscheMatrixTangent1a
! REAL(DFP), ALLOCATABLE :: realval(:), ff(:, :)
! INTEGER(I4B) :: nns1, nns2, nips, nsd, ips, r1, r2, i
! REAL(DFP) :: dd(3, 3), s(3)
!
! nns1 = SIZE(test%N, 1)
! nns2 = SIZE(trial%N, 1)
! nips = SIZE(trial%N, 2)
! nsd = trial%refElem%nsd
! ALLOCATE (ff(nns1, nsd * nns2), realval(nips))
! realval = trial%Ws * trial%Js * trial%Thickness
! ALLOCATE (ans(nns1 * nsd, nns2 * nsd))
! ans = 0.0_DFP
!
! DO ips = 1, nips
!   dd(1:nsd, 1:nsd) = &
!   & 2.0 * mu(ips) * realval(ips) &
!   & * OUTERPROD(a=trial%normal(1:nsd, ips), &
!               & b=trial%normal(1:nsd, ips))
!
!   ff = OUTERPROD( &
!                   & test%N(1:nns1, ips), &
!                   & RESHAPE( &
!                               & MATMUL( &
!                                         & trial%dNdXt(1:nns2, 1:nsd, ips), &
!                                         & dd(1:nsd, 1:nsd) &
!                                     & ), &
!                               & [nsd * nns2] &
!                           & ) &
!               & )
!
!   r1 = 0; r2 = 0
!   DO i = 1, nsd
!     r1 = r2 + 1; r2 = i * nns1
!     ans(r1:r2, :) = ans(r1:r2, :) + trial%normal(i, ips) * ff
!   END DO
! END DO
!
! DEALLOCATE (realval, ff)
!
! END PROCEDURE ElasticNitscheMatrixTangent1a
!
! !----------------------------------------------------------------------------
! !                                             ElasticityNitscheMatrixTangent
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE ElasticNitscheMatrixTangent1b
! REAL(DFP), ALLOCATABLE :: realval(:), ff(:, :)
! INTEGER(I4B) :: nns1, nns2, nips, nsd, ips, r1, r2, i
! REAL(DFP) :: dd(3, 3)
!
! nns1 = SIZE(test%N, 1)
! nns2 = SIZE(trial%N, 1)
! nips = SIZE(trial%N, 2)
! nsd = trial%refElem%nsd
! ALLOCATE (ff(nns1, nsd * nns2), realval(nips))
! realval = trial%Ws * trial%Js * trial%Thickness
! ALLOCATE (ans(nns1 * nsd, nns2 * nsd))
! ans = 0.0_DFP
!
! DO ips = 1, nips
!   dd(1:nsd, 1:nsd) = &
!   & 2.0 * mu * realval(ips) &
!   & * OUTERPROD(a=trial%normal(1:nsd, ips), &
!               & b=trial%normal(1:nsd, ips))
!
!   ff = OUTERPROD( &
!                   & test%N(1:nns1, ips), &
!                   & RESHAPE( &
!                               & MATMUL( &
!                                         & trial%dNdXt(1:nns2, 1:nsd, ips), &
!                                         & dd(1:nsd, 1:nsd) &
!                                     & ), &
!                               & [nsd * nns2] &
!                           & ) &
!               & )
!
!   r1 = 0; r2 = 0
!   DO i = 1, nsd
!     r1 = r2 + 1; r2 = i * nns1
!     ans(r1:r2, :) = ans(r1:r2, :) + trial%normal(i, ips) * ff
!   END DO
! END DO
!
! DEALLOCATE (realval, ff)
!
! END PROCEDURE ElasticNitscheMatrixTangent1b
!
! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------
!
! MODULE PROCEDURE ElasticNitscheMatrixTangent1c
! REAL(DFP), ALLOCATABLE :: muBar(:)
! CALL getInterpolation(obj=trial, ans=muBar, val=mu)
! ans = ElasticNitscheMatrixTangent( &
! & test=test, trial=trial, mu=muBar)
! DEALLOCATE (muBar)
! END PROCEDURE ElasticNitscheMatrixTangent1c
!
! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------
!
END SUBMODULE MatrixTangent
