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

SUBMODULE(FEVariable_ScalarInterpolationMethod) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarConstantGetInterpolation_1
INTEGER(I4B) :: ii

tsize = nips
IF (.NOT. addContribution) ans(1:tsize) = 0.0_DFP

DO ii = 1, tsize
  ans(ii) = ans(ii) + scale * obj%val(1)
END DO
END PROCEDURE ScalarConstantGetInterpolation_1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarConstantGetInterpolation_2
INTEGER(I4B) :: ii, ansStart

ansStart = (timeIndx - 1) * ans%s(1)
IF (.NOT. addContribution) ans%val(ansStart + 1:ansStart + nips) = 0.0_DFP

DO ii = 1, nips
  ans%val(ansStart + ii) = ans%val(ansStart + ii) + scale * obj%val(1)
END DO
END PROCEDURE ScalarConstantGetInterpolation_2

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarConstantGetInterpolation_3
IF (.NOT. addContribution) ans = 0.0_DFP
ans = ans + scale * obj%val(1)
END PROCEDURE ScalarConstantGetInterpolation_3

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolation1_(ans, scale, N, nns, nips, val, &
                                         valStart, ansStart)
  REAL(DFP), INTENT(INOUT) :: ans(:)
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), INTENT(IN) :: N(:, :)
  INTEGER(I4B), INTENT(IN) :: nns, nips
  REAL(DFP), INTENT(IN) :: val(:)
  INTEGER(I4B), INTENT(IN) :: valStart, ansStart

  INTEGER(I4B) :: ips, ii

  DO ips = 1, nips
    DO ii = 1, nns
      ans(ansStart + ips) = ans(ansStart + ips) &
                            + scale * N(ii, ips) * val(valStart + ii)
    END DO
  END DO

END SUBROUTINE MasterGetInterpolation1_

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolation3_(ans, scale, N, nns, spaceIndx, val, &
                                         valStart)
  REAL(DFP), INTENT(INOUT) :: ans
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), INTENT(IN) :: N(:, :)
  INTEGER(I4B), INTENT(IN) :: nns, spaceIndx
  REAL(DFP), INTENT(IN) :: val(:)
  INTEGER(I4B), INTENT(IN) :: valStart

  INTEGER(I4B) :: ii

  DO ii = 1, nns
    ans = ans + scale * N(ii, spaceIndx) * val(valStart + ii)
  END DO
END SUBROUTINE MasterGetInterpolation3_

!----------------------------------------------------------------------------
!                                                          GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarSpaceGetInterpolation_1
INTEGER(I4B) :: ips

tsize = nips
IF (.NOT. addContribution) ans(1:tsize) = 0.0_DFP

SELECT CASE (obj%varType)
CASE (TypeFEVariableOpt%nodal)
  !! convert nodal values to quadrature values by using N
  !! make sure nns .LE. obj%len

  CALL MasterGetInterpolation1_(ans=ans, scale=scale, N=N, nns=nns, &
                                nips=nips, val=obj%val, valStart=0, &
                                ansStart=0)

CASE (TypeFEVariableOpt%quadrature)
  !! No need for interpolation, just returnt the quadrature values
  !! make sure nips .LE. obj%len

  DO ips = 1, tsize
    ans(ips) = ans(ips) + scale * obj%val(ips)
  END DO

END SELECT

END PROCEDURE ScalarSpaceGetInterpolation_1

!----------------------------------------------------------------------------
!                                                          GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarSpaceGetInterpolation_2
INTEGER(I4B) :: ips, ansStart, valStart

ansStart = (timeIndx - 1) * ans%s(1)
valStart = 0

IF (.NOT. addContribution) ans%val(1 + ansStart:nips + ansStart) = 0.0_DFP

SELECT CASE (obj%varType)
CASE (TypeFEVariableOpt%nodal)
  CALL MasterGetInterpolation1_(ans=ans%val, scale=scale, N=N, &
                                nns=nns, nips=nips, val=obj%val, &
                                valStart=valStart, ansStart=ansStart)

CASE (TypeFEVariableOpt%quadrature)
  DO ips = 1, nips
    ans%val(ansStart + ips) = ans%val(ansStart + ips) + scale * obj%val(ips)
  END DO

END SELECT

END PROCEDURE ScalarSpaceGetInterpolation_2

!----------------------------------------------------------------------------
!                                                          GetInterpolation_
!----------------------------------------------------------------------------

! obj%vartype is nodal
! convert nodal values to quadrature values by using N
! make sure nns .LE. obj%len
!
! obj%vartype is quadrature
! No need for interpolation, just returnt the quadrature values
! make sure nips .LE. obj%len
MODULE PROCEDURE ScalarSpaceGetInterpolation_3
IF (.NOT. addContribution) ans = 0.0_DFP

SELECT CASE (obj%vartype)
CASE (TypeFEVariableOpt%nodal)
  CALL MasterGetInterpolation3_(ans=ans, scale=scale, N=N, nns=nns, &
                                spaceIndx=spaceIndx, val=obj%val, valStart=0)

CASE (TypeFEVariableOpt%quadrature)
  ans = ans + scale * obj%val(spaceIndx)

END SELECT
END PROCEDURE ScalarSpaceGetInterpolation_3

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarSpaceTimeGetInterpolation_1
INTEGER(I4B) :: aa, valStart, ansStart
REAL(DFP) :: myscale
LOGICAL(LGT), PARAMETER :: yes = .TRUE.

tsize = nips
IF (.NOT. addContribution) ans(1:tsize) = 0.0_DFP

ansStart = 0

SELECT CASE (obj%varType)
CASE (TypeFEVariableOpt%nodal)
  !! convert nodal values to quadrature values by using N
  !! make sure nns .LE. obj%len
  !! obj%s(1) should be atleast nns
  !! obj%s(2) should be atleast nnt

  DO aa = 1, nnt
    myscale = scale * T(aa)
    valStart = (aa - 1) * obj%s(1)
    CALL MasterGetInterpolation1_(ans=ans, scale=myscale, N=N, nns=nns, &
                                  nips=nips, val=obj%val, valStart=valStart, &
                                  ansStart=ansStart)
  END DO

CASE (TypeFEVariableOpt%quadrature)
  !! No need for interpolation, just returnt the quadrature values
  !! make sure nips .LE. obj%len

  valStart = (timeIndx - 1) * obj%s(1)
  DO aa = 1, tsize
    ans(aa) = ans(aa) + scale * obj%val(valStart + aa)
  END DO

END SELECT

END PROCEDURE ScalarSpaceTimeGetInterpolation_1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarSpaceTimeGetInterpolation_2
INTEGER(I4B) :: aa, valStart, ansStart
REAL(DFP) :: myscale
LOGICAL(LGT), PARAMETER :: yes = .TRUE.

ansStart = (timeIndx - 1) * ans%s(1)
IF (.NOT. addContribution) ans%val(1 + ansStart:nips + ansStart) = 0.0_DFP

SELECT CASE (obj%varType)

CASE (TypeFEVariableOpt%nodal)
  DO aa = 1, nnt
    myscale = scale * T(aa)
    valStart = (aa - 1) * obj%s(1)
    CALL MasterGetInterpolation1_(ans=ans%val, scale=myscale, N=N, nns=nns, &
                                  nips=nips, val=obj%val, valStart=valStart, &
                                  ansStart=ansStart)
  END DO

CASE (TypeFEVariableOpt%quadrature)
  valStart = (timeIndx - 1) * obj%s(1)
  DO aa = 1, nips
    ans%val(ansStart + aa) = ans%val(ansStart + aa) &
                             + scale * obj%val(valStart + aa)
  END DO

END SELECT

END PROCEDURE ScalarSpaceTimeGetInterpolation_2

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

! obj%vartype is nodal
! convert nodal values to quadrature values by using N
! make sure nns .LE. obj%len
! obj%s(1) should be atleast nns
! obj%s(2) should be atleast nnt
!
! obj%vartype is quadrature
! No need for interpolation, just returnt the quadrature values
! make sure nips .LE. obj%len

MODULE PROCEDURE ScalarSpaceTimeGetInterpolation_3
INTEGER(I4B) :: aa, valStart
REAL(DFP) :: myscale
LOGICAL(LGT), PARAMETER :: yes = .TRUE.

IF (.NOT. addContribution) ans = 0.0_DFP

SELECT CASE (obj%varType)

CASE (TypeFEVariableOpt%nodal)
  DO aa = 1, nnt
    myscale = scale * T(aa)
    valStart = (aa - 1) * obj%s(1)
    CALL MasterGetInterpolation3_(ans=ans, scale=myscale, N=N, nns=nns, &
                                  spaceIndx=spaceIndx, val=obj%val, &
                                  valStart=valStart)
  END DO

CASE (TypeFEVariableOpt%quadrature)
  valStart = (timeIndx - 1) * obj%s(1)
  ans = ans + scale * obj%val(valStart + spaceIndx)

END SELECT
END PROCEDURE ScalarSpaceTimeGetInterpolation_3

END SUBMODULE Methods
