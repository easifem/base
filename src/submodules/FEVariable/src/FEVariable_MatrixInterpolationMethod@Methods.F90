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

SUBMODULE(FEVariable_MatrixInterpolationMethod) Methods
USE BaseType, ONLY: TypeFEVariableConstant, TypeFEVariableSpace, &
                    TypeFEVariableTime, TypeFEVariableSpaceTime
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixConstantGetInterpolation_1
INTEGER(I4B) :: ips, ii, jj, indx

dim1 = obj%s(1)
dim2 = obj%s(2)
dim3 = nips

IF (.NOT. addContribution) ans(1:dim1, 1:dim2, 1:dim3) = 0.0_DFP

DO ips = 1, dim3
  DO jj = 1, dim2
    DO ii = 1, dim1
      indx = (jj - 1) * dim1 + ii
      ans(ii, jj, ips) = ans(ii, jj, ips) + scale * obj%val(indx)
    END DO
  END DO
END DO
END PROCEDURE MatrixConstantGetInterpolation_1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixConstantGetInterpolation_2
INTEGER(I4B) :: tsize, ansStart, valStart, ii

tsize = ans%s(1) * ans%s(2) * nips
ansStart = (timeIndx - 1) * tsize
IF (.NOT. addContribution) ans%val(ansStart + 1:ansStart + tsize) = 0.0_DFP

valStart = 0
DO ii = 1, tsize
  ans%val(ansStart + ii) = ans%val(ansStart + ii) &
                           + scale * obj%val(valStart + ii)
END DO
END PROCEDURE MatrixConstantGetInterpolation_2

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixConstantGetInterpolation_3
INTEGER(I4B) :: ii, jj, indx

nrow = obj%s(1)
ncol = obj%s(2)

IF (.NOT. addContribution) ans(1:nrow, 1:ncol) = 0.0_DFP

DO jj = 1, ncol
  DO ii = 1, nrow
    indx = (jj - 1) * nrow + ii
    ans(ii, jj) = ans(ii, jj) + scale * obj%val(indx)
  END DO
END DO
END PROCEDURE MatrixConstantGetInterpolation_3

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolationFromNodal1_(ans, scale, N, nns, dim1, &
                                                  dim2, nips, val, valStart, &
                                                  valEnd)
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), INTENT(IN) :: N(:, :)
  INTEGER(I4B), INTENT(IN) :: nns, nips, dim1, dim2
  REAL(DFP), INTENT(IN) :: val(:)
  INTEGER(I4B), INTENT(IN) :: valStart
  INTEGER(I4B), INTENT(OUT) :: valEnd

  INTEGER(I4B) :: ips, ii, jj, inode, tsize, indx, a, b

  tsize = dim1 * dim2

  DO ips = 1, nips
    DO inode = 1, nns
      a = (inode - 1) * tsize

      DO jj = 1, dim2
        b = (jj - 1) * dim1

        DO ii = 1, dim1
          indx = a + b + ii + valStart
          ans(ii, jj, ips) = ans(ii, jj, ips) &
                             + scale * N(inode, ips) * val(indx)

        END DO
      END DO
    END DO
  END DO

  valEnd = valStart + nns * tsize
END SUBROUTINE MasterGetInterpolationFromNodal1_

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolationFromNodal2_(ans, scale, N, nns, dim1, &
                                                  dim2, nips, val, valStart, &
                                                  valEnd, ansStart, ansEnd)
  REAL(DFP), INTENT(INOUT) :: ans(:)
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), INTENT(IN) :: N(:, :)
  INTEGER(I4B), INTENT(IN) :: nns, nips, dim1, dim2
  REAL(DFP), INTENT(IN) :: val(:)
  INTEGER(I4B), INTENT(IN) :: valStart
  INTEGER(I4B), INTENT(OUT) :: valEnd
  INTEGER(I4B), INTENT(IN) :: ansStart
  INTEGER(I4B), INTENT(OUT) :: ansEnd

  INTEGER(I4B) :: ips, jj, ival, jval, ians, jans, tsize

  tsize = dim1 * dim2

  DO ips = 1, nips
    ians = (ips - 1) * tsize + 1 + ansStart
    jans = ips * tsize + ansStart

    DO jj = 1, nns
      ival = (jj - 1) * tsize + 1 + valStart
      jval = jj * tsize + valStart

      ans(ians:jans) = ans(ians:jans) &
                       + scale * N(jj, ips) * val(ival:jval)
    END DO
  END DO

  valEnd = valStart + nns * tsize
  ansEnd = ansStart + nips * tsize
END SUBROUTINE MasterGetInterpolationFromNodal2_

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolationFromNodal3_(ans, scale, N, nns, dim1, &
                                                  dim2, spaceIndx, &
                                                  val, valStart, valEnd)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), INTENT(IN) :: N(:, :)
  INTEGER(I4B), INTENT(IN) :: nns, dim1, dim2, spaceIndx
  REAL(DFP), INTENT(IN) :: val(:)
  INTEGER(I4B), INTENT(IN) :: valStart
  INTEGER(I4B), INTENT(OUT) :: valEnd

  INTEGER(I4B) :: ii, jj, inode, tsize, indx, a, b

  tsize = dim1 * dim2

  DO inode = 1, nns
    a = (inode - 1) * tsize

    DO jj = 1, dim2
      b = (jj - 1) * dim1

      DO ii = 1, dim1
        indx = a + b + ii + valStart
        ans(ii, jj) = ans(ii, jj) &
                      + scale * N(inode, spaceIndx) * val(indx)

      END DO
    END DO
  END DO

  valEnd = valStart + nns * tsize
END SUBROUTINE MasterGetInterpolationFromNodal3_

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolationFromQuadrature1_(ans, scale, dim1, &
                                                       dim2, nips, val, &
                                                       valStart, valEnd)
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  REAL(DFP), INTENT(IN) :: scale
  INTEGER(I4B), INTENT(IN) :: dim1, dim2, nips
  REAL(DFP), INTENT(IN) :: val(:)
  INTEGER(I4B), INTENT(IN) :: valStart
  INTEGER(I4B), INTENT(OUT) :: valEnd

  INTEGER(I4B) :: ips, ii, jj, tsize, indx, a, b

  tsize = dim1 * dim2

  DO ips = 1, nips
    a = (ips - 1) * tsize

    DO jj = 1, dim2
      b = (jj - 1) * dim1

      DO ii = 1, dim1
        indx = a + b + ii + valStart
        ans(ii, jj, ips) = ans(ii, jj, ips) + scale * val(indx)

      END DO
    END DO
  END DO

  valEnd = valStart + nips * tsize
END SUBROUTINE MasterGetInterpolationFromQuadrature1_

!----------------------------------------------------------------------------
!                                      MasterGetInterpolationFromQuadrature_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolationFromQuadrature2_(ans, scale, dim1, &
                                                       dim2, nips, val, &
                                                       valStart, valEnd, &
                                                       ansStart, ansEnd)
  REAL(DFP), INTENT(INOUT) :: ans(:)
  REAL(DFP), INTENT(IN) :: scale
  INTEGER(I4B), INTENT(IN) :: dim1, dim2, nips
  REAL(DFP), INTENT(IN) :: val(:)
  INTEGER(I4B), INTENT(IN) :: valStart
  INTEGER(I4B), INTENT(OUT) :: valEnd
  INTEGER(I4B), INTENT(IN) :: ansStart
  INTEGER(I4B), INTENT(OUT) :: ansEnd

  INTEGER(I4B) :: ii, tsize

  tsize = nips * dim1 * dim2
  valEnd = valStart + tsize
  ansEnd = ansStart + tsize

  DO ii = 1, tsize
    ans(ansStart + ii) = ans(ansStart + ii) + scale * val(valStart + ii)
  END DO
END SUBROUTINE MasterGetInterpolationFromQuadrature2_

!----------------------------------------------------------------------------
!                                                    MasterGetInterpolation_
!----------------------------------------------------------------------------

PURE SUBROUTINE MasterGetInterpolationFromQuadrature3_(ans, scale, dim1, &
                                                       dim2, spaceIndx, val, &
                                                       valStart, valEnd)
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  REAL(DFP), INTENT(IN) :: scale
  INTEGER(I4B), INTENT(IN) :: dim1, dim2, spaceIndx
  REAL(DFP), INTENT(IN) :: val(:)
  INTEGER(I4B), INTENT(IN) :: valStart
  INTEGER(I4B), INTENT(OUT) :: valEnd

  INTEGER(I4B) :: ii, jj, tsize, indx, a, b

  tsize = dim1 * dim2

  a = (spaceIndx - 1) * tsize
  DO jj = 1, dim2
    b = (jj - 1) * dim1

    DO ii = 1, dim1
      indx = a + b + ii + valStart
      ans(ii, jj) = ans(ii, jj) + scale * val(indx)
    END DO
  END DO

  valEnd = valStart + tsize
END SUBROUTINE MasterGetInterpolationFromQuadrature3_

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

! obj%defineon is nodal
! Nodal Matrix Space
! Convert nodal values to quadrature values by using N(:,:)
! make sure nns .LE. obj%len
!
! obj%defineon is quadrature
! No need for interpolation, just returnt the quadrature values
! make sure nips .LE. obj%len
MODULE PROCEDURE MatrixSpaceGetInterpolation_1
INTEGER(I4B) :: valEnd

dim1 = obj%s(1)
dim2 = obj%s(2)
dim3 = nips

IF (.NOT. addContribution) ans(1:dim1, 1:dim2, 1:dim3) = 0.0_DFP

SELECT CASE (obj%defineon)
CASE (TypeFEVariableOpt%nodal)

  CALL MasterGetInterpolationFromNodal1_(ans=ans, scale=scale, N=N, &
                                         nns=nns, nips=nips, val=obj%val, &
                                         dim1=dim1, dim2=dim2, &
                                         valStart=0, valEnd=valEnd)

CASE (TypeFEVariableOpt%quadrature)

  CALL MasterGetInterpolationFromQuadrature1_(ans=ans, scale=scale, &
                                              nips=nips, dim1=dim1, &
                                              dim2=dim2, val=obj%val, &
                                              valStart=0, valEnd=valEnd)

END SELECT
END PROCEDURE MatrixSpaceGetInterpolation_1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixSpaceGetInterpolation_2
INTEGER(I4B) :: valStart, valEnd, ansStart, ansEnd, dim1, dim2, dim3, tsize

dim1 = ans%s(1)
dim2 = ans%s(2)
dim3 = nips

tsize = dim1 * dim2 * dim3
ansStart = (timeIndx - 1) * tsize
ansEnd = ansStart + tsize
valStart = 0

IF (.NOT. addContribution) ans%val(ansStart + 1:ansEnd) = 0.0_DFP

SELECT CASE (obj%defineon)
CASE (TypeFEVariableOpt%nodal)

  CALL MasterGetInterpolationFromNodal2_(ans=ans%val, scale=scale, N=N, &
                                         nns=nns, dim1=dim1, dim2=dim2, &
                                         nips=nips, val=obj%val, &
                                         valStart=valStart, valEnd=valEnd, &
                                         ansStart=ansStart, ansEnd=ansEnd)

CASE (TypeFEVariableOpt%quadrature)

  CALL MasterGetInterpolationFromQuadrature2_(ans=ans%val, scale=scale, &
                                              nips=nips, dim1=dim1, &
                                              dim2=dim2, val=obj%val, &
                                              valStart=valStart, &
                                              valEnd=valEnd, &
                                              ansStart=ansStart, &
                                              ansEnd=ansEnd)

END SELECT
END PROCEDURE MatrixSpaceGetInterpolation_2

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixSpaceGetInterpolation_3
INTEGER(I4B) :: valEnd

nrow = obj%s(1)
ncol = obj%s(2)
IF (.NOT. addContribution) ans(1:nrow, 1:ncol) = 0.0_DFP

SELECT CASE (obj%defineon)
CASE (TypeFEVariableOpt%nodal)

  CALL MasterGetInterpolationFromNodal3_(ans=ans, scale=scale, N=N, &
                                         nns=nns, val=obj%val, &
                                         dim1=nrow, dim2=ncol, &
                                         valStart=0, valEnd=valEnd, &
                                         spaceIndx=spaceIndx)

CASE (TypeFEVariableOpt%quadrature)

  CALL MasterGetInterpolationFromQuadrature3_(ans=ans, scale=scale, &
                                              dim1=nrow, dim2=ncol, &
                                              val=obj%val, &
                                              spaceIndx=spaceIndx, &
                                              valStart=0, valEnd=valEnd)

END SELECT
END PROCEDURE MatrixSpaceGetInterpolation_3

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

! Convert nodal values to quadrature values by using N
! make sure nns .LE. obj%len
! obj%s(1) denotes the nsd in ans
! obj%s(2) should be atleast nns
! obj%s(3) should be atleast nnt
!
! No need for interpolation, just returnt the quadrature values
! make sure nips .LE. obj%len
MODULE PROCEDURE MatrixSpaceTimeGetInterpolation_1
INTEGER(I4B) :: aa, valStart, valEnd
REAL(DFP) :: myscale

dim1 = obj%s(1)
dim2 = obj%s(2)
dim3 = nips

IF (.NOT. addContribution) ans(1:dim1, 1:dim2, 1:dim3) = 0.0_DFP

SELECT CASE (obj%defineon)
CASE (TypeFEVariableOpt%nodal)

  valEnd = 0
  DO aa = 1, nnt
    myscale = scale * T(aa)
    valStart = valEnd
    CALL MasterGetInterpolationFromNodal1_(ans=ans, scale=myscale, N=N, &
                                           nns=nns, dim1=dim1, dim2=dim2, &
                                           nips=nips, val=obj%val, &
                                           valStart=valStart, valEnd=valEnd)
  END DO

CASE (TypeFEVariableOpt%quadrature)

  valStart = nips * dim1 * dim2 * (timeIndx - 1)
  CALL MasterGetInterpolationFromQuadrature1_(ans=ans, scale=scale, &
                                              dim1=dim1, dim2=dim2, &
                                              nips=nips, val=obj%val, &
                                              valStart=valStart, &
                                              valEnd=valEnd)

END SELECT
END PROCEDURE MatrixSpaceTimeGetInterpolation_1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixSpaceTimeGetInterpolation_2
INTEGER(I4B) :: aa, valStart, valEnd, ansStart, ansEnd, dim1, dim2, dim3, &
                tsize
REAL(DFP) :: myscale

dim1 = obj%s(1)
dim2 = obj%s(2)
dim3 = nips

tsize = dim1 * dim2 * dim3
ansStart = (timeIndx - 1) * tsize
ansEnd = ansStart + tsize
valStart = 0

IF (.NOT. addContribution) ans%val(ansStart + 1:ansEnd) = 0.0_DFP

SELECT CASE (obj%defineon)
CASE (TypeFEVariableOpt%nodal)

  valEnd = 0
  DO aa = 1, nnt
    myscale = scale * T(aa)
    valStart = valEnd
    CALL MasterGetInterpolationFromNodal2_(ans=ans%val, scale=myscale, N=N, &
                                           nns=nns, dim1=dim1, dim2=dim2, &
                                           nips=nips, val=obj%val, &
                                           valStart=valStart, valEnd=valEnd, &
                                           ansStart=ansStart, ansEnd=ansEnd)
  END DO

CASE (TypeFEVariableOpt%quadrature)

  valStart = tsize * (timeIndx - 1)
  CALL MasterGetInterpolationFromQuadrature2_(ans=ans%val, scale=scale, &
                                              dim1=dim1, dim2=dim2, &
                                              nips=nips, val=obj%val, &
                                              valStart=valStart, &
                                              valEnd=valEnd, &
                                              ansStart=ansStart, &
                                              ansEnd=ansEnd)

END SELECT
END PROCEDURE MatrixSpaceTimeGetInterpolation_2

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixSpaceTimeGetInterpolation_3
INTEGER(I4B) :: aa, valStart, valEnd
REAL(DFP) :: myscale

nrow = obj%s(1)
ncol = obj%s(2)

IF (.NOT. addContribution) ans(1:nrow, 1:ncol) = 0.0_DFP

SELECT CASE (obj%defineon)
CASE (TypeFEVariableOpt%nodal)

  valEnd = 0
  DO aa = 1, nnt
    myscale = scale * T(aa)
    valStart = valEnd
    CALL MasterGetInterpolationFromNodal3_(ans=ans, scale=myscale, N=N, &
                                           nns=nns, dim1=nrow, dim2=ncol, &
                                           spaceIndx=spaceIndx, val=obj%val, &
                                           valStart=valStart, valEnd=valEnd)
  END DO

CASE (TypeFEVariableOpt%quadrature)

  valStart = obj%s(3) * nrow * ncol * (timeIndx - 1)
  CALL MasterGetInterpolationFromQuadrature3_(ans=ans, scale=scale, &
                                              dim1=nrow, dim2=ncol, &
                                              spaceIndx=spaceIndx, &
                                              val=obj%val, &
                                              valStart=valStart, &
                                              valEnd=valEnd)

END SELECT
END PROCEDURE MatrixSpaceTimeGetInterpolation_3

!----------------------------------------------------------------------------
!                                                        MatrixInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixGetInterpolation_3
INTEGER(I4B) :: vartype
vartype = obj%varType
SELECT CASE (vartype)
CASE (TypeFEVariableOpt%constant)
  CALL GetInterpolation_( &
    obj=obj, rank=rank, vartype=TypeFEVariableConstant, N=N, nns=nns, &
    spaceIndx=spaceIndx, timeIndx=timeIndx, scale=scale, &
    addContribution=addContribution, ans=ans, nrow=nrow, ncol=ncol)

CASE (TypeFEVariableOpt%space)
  CALL GetInterpolation_( &
    obj=obj, rank=rank, vartype=TypeFEVariableSpace, N=N, nns=nns, &
    spaceIndx=spaceIndx, timeIndx=timeIndx, scale=scale, &
    addContribution=addContribution, ans=ans, nrow=nrow, ncol=ncol)

CASE (TypeFEVariableOpt%time)

CASE (TypeFEVariableOpt%spacetime)
  CALL GetInterpolation_( &
    obj=obj, rank=rank, vartype=TypeFEVariableSpaceTime, N=N, nns=nns, &
    spaceIndx=spaceIndx, timeIndx=timeIndx, T=T, nnt=nnt, scale=scale, &
    addContribution=addContribution, ans=ans, nrow=nrow, ncol=ncol)
END SELECT
END PROCEDURE MatrixGetInterpolation_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
