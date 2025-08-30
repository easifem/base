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

SUBMODULE(FEVariable_Method) ScalarInterpolationMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarConstantGetInterpolation_
INTEGER(I4B) :: ii

tsize = nips
IF (.NOT. addContribution) ans(1:tsize) = 0.0_DFP

DO ii = 1, tsize
  ans(ii) = ans(ii) + scale * obj%val(1)
END DO
END PROCEDURE ScalarConstantGetInterpolation_

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolation_(ans, scale, N, nns, nips, val, &
                                        valStart)
  REAL(DFP), INTENT(INOUT) :: ans(:)
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), INTENT(IN) :: N(:, :)
  INTEGER(I4B), INTENT(IN) :: nns, nips
  REAL(DFP), INTENT(IN) :: val(:)
  INTEGER(I4B), INTENT(IN) :: valStart

  INTEGER(I4B) :: ips, ii

  DO ips = 1, nips
    DO ii = 1, nns
      ans(ips) = ans(ips) + scale * N(ii, ips) * val(valStart + ii)
    END DO
  END DO

END SUBROUTINE MasterGetInterpolation_

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarSpaceGetInterpolation_
INTEGER(I4B) :: ips

tsize = nips
IF (.NOT. addContribution) ans(1:tsize) = 0.0_DFP

SELECT CASE (obj%varType)
CASE (TypeFEVariableOpt%nodal)
  !! convert nodal values to quadrature values by using N
  !! make sure nns .LE. obj%len

  CALL MasterGetInterpolation_(ans=ans, scale=scale, N=N, nns=nns, &
                               nips=nips, val=obj%val, valStart=0)

CASE (TypeFEVariableOpt%quadrature)
  !! No need for interpolation, just returnt the quadrature values
  !! make sure nips .LE. obj%len

  DO ips = 1, tsize
    ans(ips) = ans(ips) + scale * obj%val(ips)
  END DO

END SELECT

END PROCEDURE ScalarSpaceGetInterpolation_

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarSpaceTimeGetInterpolation_
INTEGER(I4B) :: aa, valStart
REAL(DFP) :: myscale
LOGICAL(LGT), PARAMETER :: yes = .TRUE.

tsize = nips
IF (.NOT. addContribution) ans(1:tsize) = 0.0_DFP

SELECT CASE (obj%varType)
CASE (TypeFEVariableOpt%nodal)
  !! convert nodal values to quadrature values by using N
  !! make sure nns .LE. obj%len
  !! obj%s(1) should be atleast nns
  !! obj%s(2) should be atleast nnt

  DO aa = 1, nnt
    myscale = scale * T(aa)
    valStart = (aa - 1) * obj%s(1)
    CALL MasterGetInterpolation_(ans=ans, scale=myscale, N=N, nns=nns, &
                                 nips=nips, val=obj%val, valStart=valStart)
  END DO

CASE (TypeFEVariableOpt%quadrature)
  !! No need for interpolation, just returnt the quadrature values
  !! make sure nips .LE. obj%len

  valStart = (timeIndx - 1) * obj%s(1)
  DO aa = 1, tsize
    ans(aa) = ans(aa) + scale * obj%val(valStart + aa)
  END DO

END SELECT

END PROCEDURE ScalarSpaceTimeGetInterpolation_

END SUBMODULE ScalarInterpolationMethods
