! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(FEVariable_InterpolationMethod) VectorMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorConstantGetInterpolation_
INTEGER(I4B) :: ii

nrow = obj%s(1)
ncol = nips
IF (.NOT. addContribution) ans(1:nrow, 1:ncol) = 0.0_DFP

DO ii = 1, ncol
  ans(1:nrow, ii) = ans(1:nrow, ii) + scale * obj%val(1:nrow)
END DO
END PROCEDURE VectorConstantGetInterpolation_

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolationFromNodal_(ans, scale, N, nns, nsd, &
                                                 nips, val, valStart, valEnd)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), INTENT(IN) :: N(:, :)
  INTEGER(I4B), INTENT(IN) :: nns, nsd, nips
  REAL(DFP), INTENT(IN) :: val(:)
  INTEGER(I4B), INTENT(IN) :: valStart
  INTEGER(I4B), INTENT(OUT) :: valEnd

  INTEGER(I4B) :: ips, jj, istart, iend

  DO ips = 1, nips
    DO jj = 1, nns
      istart = (jj - 1) * nsd + 1 + valStart
      iend = jj * nsd + valStart
      ans(1:nsd, ips) = ans(1:nsd, ips) &
                        + scale * N(jj, ips) * val(istart:iend)
    END DO
  END DO

  valEnd = valStart + nns * nsd

END SUBROUTINE MasterGetInterpolationFromNodal_

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolationFromQuadrature_(ans, scale, nsd, &
                                                      nips, val, valStart, &
                                                      valEnd)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  REAL(DFP), INTENT(IN) :: scale
  INTEGER(I4B), INTENT(IN) :: nsd, nips
  REAL(DFP), INTENT(IN) :: val(:)
  INTEGER(I4B), INTENT(IN) :: valStart
  INTEGER(I4B), INTENT(OUT) :: valEnd

  INTEGER(I4B) :: ips, istart, iend

  DO ips = 1, nips
    istart = (ips - 1) * nsd + 1 + valStart
    iend = ips * nsd + valStart
    ans(1:nsd, ips) = ans(1:nsd, ips) + scale * val(istart:iend)
  END DO

  valEnd = valStart + nips * nsd

END SUBROUTINE MasterGetInterpolationFromQuadrature_

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorSpaceGetInterpolation_
INTEGER(I4B) :: valEnd

nrow = obj%s(1)
ncol = nips
IF (.NOT. addContribution) ans(1:nrow, 1:ncol) = 0.0_DFP

SELECT CASE (obj%varType)
CASE (TypeFEVariableOpt%nodal)
  !! Nodal Vector Space
  !! Convert nodal values to quadrature values by using N(:,:)
  !! make sure nns .LE. obj%len

  CALL MasterGetInterpolationFromNodal_(ans=ans, scale=scale, N=N, nns=nns, &
                                        nsd=nrow, nips=nips, val=obj%val, &
                                        valStart=0, valEnd=valEnd)

CASE (TypeFEVariableOpt%quadrature)
  !! No need for interpolation, just returnt the quadrature values
  !! make sure nips .LE. obj%len

  CALL MasterGetInterpolationFromQuadrature_(ans=ans, scale=scale, &
                                             nsd=nrow, nips=nips, &
                                             val=obj%val, valStart=0, &
                                             valEnd=valEnd)

END SELECT

END PROCEDURE VectorSpaceGetInterpolation_

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorSpaceTimeGetInterpolation_
INTEGER(I4B) :: aa, valStart, valEnd
REAL(DFP) :: myscale
LOGICAL(LGT), PARAMETER :: yes = .TRUE.

nrow = obj%s(1)
ncol = nips
IF (.NOT. addContribution) ans(1:nrow, 1:ncol) = 0.0_DFP

SELECT CASE (obj%varType)
CASE (TypeFEVariableOpt%nodal)
  !! Convert nodal values to quadrature values by using N
  !! make sure nns .LE. obj%len
  !! obj%s(1) denotes the nsd in ans
  !! obj%s(2) should be atleast nns
  !! obj%s(3) should be atleast nnt

  valEnd = 0
  DO aa = 1, nnt
    myscale = scale * T(aa)
    valStart = valEnd
    CALL MasterGetInterpolationFromNodal_(ans=ans, scale=myscale, N=N, &
                                          nns=nns, nsd=nrow, nips=nips, &
                                          val=obj%val, valStart=valStart, &
                                          valEnd=valEnd)
  END DO

CASE (TypeFEVariableOpt%quadrature)
  !! No need for interpolation, just returnt the quadrature values
  !! make sure nips .LE. obj%len

  valStart = nips * nrow * (timeIndx - 1)
  CALL MasterGetInterpolationFromQuadrature_(ans=ans, scale=scale, &
                                             nsd=nrow, nips=nips, &
                                             val=obj%val, &
                                             valStart=valStart, &
                                             valEnd=valEnd)

END SELECT

END PROCEDURE VectorSpaceTimeGetInterpolation_

END SUBMODULE VectorMethods
