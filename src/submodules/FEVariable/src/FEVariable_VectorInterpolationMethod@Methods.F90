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

SUBMODULE(FEVariable_VectorInterpolationMethod) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorConstantGetInterpolation_1
INTEGER(I4B) :: ii

nrow = obj%s(1)
ncol = nips
IF (.NOT. addContribution) ans(1:nrow, 1:ncol) = 0.0_DFP

DO ii = 1, ncol
  ans(1:nrow, ii) = ans(1:nrow, ii) + scale * obj%val(1:nrow)
END DO
END PROCEDURE VectorConstantGetInterpolation_1

!----------------------------------------------------------------------------
!                                                          GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorConstantGetInterpolation_2
INTEGER(I4B) :: ii, ansStart, valStart, tsize

tsize = ans%s(1) * ans%s(2)
ansStart = (timeIndx - 1) * tsize
IF (.NOT. addContribution) ans%val(ansStart + 1:ansStart + tsize) = 0.0_DFP

valStart = 0

DO ii = 1, tsize
  ans%val(ansStart + ii) = ans%val(ansStart + ii) &
                           + scale * obj%val(valStart + ii)
END DO
END PROCEDURE VectorConstantGetInterpolation_2

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolationFromNodal1_(ans, scale, N, nns, nsd, &
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

END SUBROUTINE MasterGetInterpolationFromNodal1_

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolationFromNodal2_(ans, scale, N, nns, nsd, &
                                                nips, val, valStart, valEnd, &
                                                  ansStart, ansEnd)
  REAL(DFP), INTENT(INOUT) :: ans(:)
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), INTENT(IN) :: N(:, :)
  INTEGER(I4B), INTENT(IN) :: nns, nsd, nips
  REAL(DFP), INTENT(IN) :: val(:)
  INTEGER(I4B), INTENT(IN) :: valStart
  INTEGER(I4B), INTENT(OUT) :: valEnd
  INTEGER(I4B), INTENT(IN) :: ansStart
  INTEGER(I4B), INTENT(OUT) :: ansEnd

  INTEGER(I4B) :: ips, jj, ival, jval, ians, jans

  DO ips = 1, nips
    ians = (ips - 1) * nsd + 1 + ansStart
    jans = ips * nsd + ansStart

    DO jj = 1, nns
      ival = (jj - 1) * nsd + 1 + valStart
      jval = jj * nsd + valStart
      ans(ians:jans) = ans(ians:jans) &
                       + scale * N(jj, ips) * val(ival:jval)
    END DO
  END DO

  valEnd = valStart + nns * nsd
  ansEnd = ansStart + nips * nsd

END SUBROUTINE MasterGetInterpolationFromNodal2_

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolationFromQuadrature1_(ans, scale, nsd, &
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

END SUBROUTINE MasterGetInterpolationFromQuadrature1_

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolationFromQuadrature2_(ans, scale, nsd, &
                                                       nips, val, valStart, &
                                                       valEnd, ansStart, &
                                                       ansEnd)
  REAL(DFP), INTENT(INOUT) :: ans(:)
  REAL(DFP), INTENT(IN) :: scale
  INTEGER(I4B), INTENT(IN) :: nsd, nips
  REAL(DFP), INTENT(IN) :: val(:)
  INTEGER(I4B), INTENT(IN) :: valStart
  INTEGER(I4B), INTENT(OUT) :: valEnd
  INTEGER(I4B), INTENT(IN) :: ansStart
  INTEGER(I4B), INTENT(OUT) :: ansEnd

  INTEGER(I4B) :: ii, tsize

  tsize = nips * nsd
  valEnd = valStart + tsize
  ansEnd = ansStart + tsize

  DO ii = 1, tsize
    ans(ansStart + ii) = ans(ansStart + ii) + scale * val(valStart + ii)
  END DO
END SUBROUTINE MasterGetInterpolationFromQuadrature2_

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorSpaceGetInterpolation_1
INTEGER(I4B) :: valEnd

nrow = obj%s(1)
ncol = nips
IF (.NOT. addContribution) ans(1:nrow, 1:ncol) = 0.0_DFP

SELECT CASE (obj%varType)
CASE (TypeFEVariableOpt%nodal)
  !! Nodal Vector Space
  !! Convert nodal values to quadrature values by using N(:,:)
  !! make sure nns .LE. obj%len

  CALL MasterGetInterpolationFromNodal1_(ans=ans, scale=scale, N=N, nns=nns, &
                                        nsd=nrow, nips=nips, val=obj%val, &
                                        valStart=0, valEnd=valEnd)

CASE (TypeFEVariableOpt%quadrature)
  !! No need for interpolation, just returnt the quadrature values
  !! make sure nips .LE. obj%len

  CALL MasterGetInterpolationFromQuadrature1_(ans=ans, scale=scale, &
                                             nsd=nrow, nips=nips, &
                                             val=obj%val, valStart=0, &
                                             valEnd=valEnd)

END SELECT

END PROCEDURE VectorSpaceGetInterpolation_1

!----------------------------------------------------------------------------
!                                                          GetInterpolation_
!----------------------------------------------------------------------------

! Following points should be noted
! obj%s(1) and ans%s(1) should be same
! ans%s(2) and nips should be same
! when obj var type is quadrature, then nips should be same as obj%s(2)
MODULE PROCEDURE VectorSpaceGetInterpolation_2
INTEGER(I4B) :: ansStart, valStart, valEnd, ansEnd, nsd

nsd = obj%s(1)
ansStart = (timeIndx - 1) * ans%s(1) * ans%s(2)
ansEnd = ansStart + ans%s(1) * ans%s(2)
valStart = 0

IF (.NOT. addContribution) ans%val(1 + ansStart:ansEnd) = 0.0_DFP

SELECT CASE (obj%varType)
CASE (TypeFEVariableOpt%nodal)
  CALL MasterGetInterpolationFromNodal2_(ans=ans%val, scale=scale, N=N, &
                                         nns=nns, nsd=nsd, nips=nips, &
                                         val=obj%val, &
                                         valStart=valStart, valEnd=valEnd, &
                                         ansStart=ansStart, ansEnd=ansEnd)

CASE (TypeFEVariableOpt%quadrature)
  CALL MasterGetInterpolationFromQuadrature2_(ans=ans%val, scale=scale, &
                                              nsd=nsd, nips=nips, &
                                              val=obj%val, &
                                              valStart=valStart, &
                                              valEnd=valEnd, &
                                              ansStart=ansStart, &
                                              ansEnd=ansEnd)

END SELECT
END PROCEDURE VectorSpaceGetInterpolation_2

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorSpaceTimeGetInterpolation_1
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
    CALL MasterGetInterpolationFromNodal1_(ans=ans, scale=myscale, N=N, &
                                          nns=nns, nsd=nrow, nips=nips, &
                                          val=obj%val, valStart=valStart, &
                                          valEnd=valEnd)
  END DO

CASE (TypeFEVariableOpt%quadrature)
  !! No need for interpolation, just returnt the quadrature values
  !! make sure nips .LE. obj%len

  valStart = nips * nrow * (timeIndx - 1)
  CALL MasterGetInterpolationFromQuadrature1_(ans=ans, scale=scale, &
                                             nsd=nrow, nips=nips, &
                                             val=obj%val, &
                                             valStart=valStart, &
                                             valEnd=valEnd)

END SELECT

END PROCEDURE VectorSpaceTimeGetInterpolation_1

!----------------------------------------------------------------------------
!                                                         GetInterpolation_
!----------------------------------------------------------------------------

! Convert nodal values to quadrature values by using N
! make sure nns .LE. obj%len
! obj%s(1) denotes the nsd in ans
! obj%s(2) should be atleast nns
! obj%s(3) should be atleast nnt
!
! No need for interpolation, just returnt the quadrature values
! make sure nips .LE. obj%len
MODULE PROCEDURE VectorSpaceTimeGetInterpolation_2
INTEGER(I4B) :: ansStart, ansEnd, valStart, valEnd, nsd, aa
REAL(DFP) :: myscale
LOGICAL(LGT), PARAMETER :: yes = .TRUE.

nsd = obj%s(1)
ansStart = (timeIndx - 1) * ans%s(1) * ans%s(2)
ansEnd = ansStart + ans%s(1) * ans%s(2)
valStart = 0

SELECT CASE (obj%varType)

CASE (TypeFEVariableOpt%nodal)
  valEnd = 0
  DO aa = 1, nnt
    myscale = scale * T(aa)
    valStart = valEnd
    CALL MasterGetInterpolationFromNodal2_(ans=ans%val, scale=myscale, N=N, &
                                           nns=nns, nsd=nsd, nips=nips, &
                                           val=obj%val, valStart=valStart, &
                                           valEnd=valEnd, ansStart=ansStart, &
                                           ansEnd=ansEnd)
  END DO

CASE (TypeFEVariableOpt%quadrature)
  valStart = nips * nsd * (timeIndx - 1)
  ansStart = nips * nsd * (timeIndx - 1)
  CALL MasterGetInterpolationFromQuadrature2_(ans=ans%val, scale=scale, &
                                              nsd=nsd, nips=nips, &
                                              val=obj%val, &
                                              valStart=valStart, &
                                              valEnd=valEnd, &
                                              ansStart=ansStart, &
                                              ansEnd=ansEnd)

END SELECT

END PROCEDURE VectorSpaceTimeGetInterpolation_2

END SUBMODULE Methods
