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

SUBMODULE(ElemshapeData_ProjectionMethods) Methods
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE FEVariable_Method, ONLY: GetInterpolation_
USE MatmulUtility, ONLY: Matmul_
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                       GetProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNdXt_1
INTEGER(I4B) :: nrow, ncol

nrow = obj%nns
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)
CALL GetProjectionOfdNdXt_(obj=obj, ans=ans, c=c, nrow=nrow, ncol=ncol)
END PROCEDURE GetProjectionOfdNdXt_1

!----------------------------------------------------------------------------
!                                                      GetProjectionOfdNdXt_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNdXt1_
INTEGER(I4B) :: ii, nsd

nrow = obj%nns
ncol = obj%nips
nsd = obj%nsd

DO ii = 1, ncol
  ans(1:nrow, ii) = MATMUL(obj%dNdXt(1:nrow, 1:nsd, ii), c(1:nsd))
END DO
END PROCEDURE GetProjectionOfdNdXt1_

!----------------------------------------------------------------------------
!                                                       GetProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNdXt_2
INTEGER(I4B) :: nrow, ncol

nrow = obj%nns
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)
CALL GetProjectionOfdNdXt_(obj=obj, ans=ans, c=c, crank=crank, nrow=nrow, &
                           ncol=ncol)
END PROCEDURE GetProjectionOfdNdXt_2

!----------------------------------------------------------------------------
!                                                      GetProjectionOfdNdXt_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNdXt2_
INTEGER(I4B) :: ips, nsd, i1
! REAL(DFP) :: cbar(SIZE(obj%dNdXt, 2), SIZE(obj%dNdXt, 3))
REAL(DFP) :: cbar(3), T(0)

#ifdef DEBUG_VER
IF (c%s(1) .NE. obj%nsd) THEN
  ERROR STOP 'projection: c component count must equal nsd'
END IF
#endif

#ifdef DEBUG_VER
IF (c%vartype .EQ. TypeFEVariableOpt%time .OR. &
    c%vartype .EQ. TypeFEVariableOpt%spacetime) THEN
  ERROR STOP 'NdXt projection: time-dependent c unsupported'
END IF
#endif

nrow = obj%nns
ncol = obj%nips
nsd = obj%nsd

DO ips = 1, obj%nips
  cbar = 0.0_DFP
  CALL GetInterpolation_( &
    obj=c, RANK=crank, N=obj%N, nns=obj%nns, spaceIndx=ips, timeIndx=0_I4B, &
    T=T, nnt=0_I4B, scale=1.0_DFP, addContribution=.FALSE., ans=cbar, &
    tsize=i1)
#ifdef DEBUG_VER
  IF (i1 .NE. nsd) THEN
    ERROR STOP 'projection: interpolated component count must equal nsd'
  END IF
#endif

  ans(1:nrow, ips) = MATMUL(obj%dNdXt(1:nrow, 1:nsd, ips), cbar(1:nsd))
END DO
END PROCEDURE GetProjectionOfdNdXt2_

!----------------------------------------------------------------------------
!                                                       getProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNdXt_3
INTEGER(I4B) :: nrow, ncol

nrow = obj%nns
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)
CALL GetProjectionOfdNdXt_(obj=obj, ans=ans, c=c, nrow=nrow, ncol=ncol)
END PROCEDURE GetProjectionOfdNdXt_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNdXt3_
INTEGER(I4B) :: ips, nsd

nrow = obj%nns
ncol = obj%nips
nsd = obj%nsd

DO ips = 1, obj%nips
  ans(1:nrow, ips) = MATMUL(obj%dNdXt(1:nrow, 1:nsd, ips), c(1:nsd, ips))
END DO
END PROCEDURE GetProjectionOfdNdXt3_

!----------------------------------------------------------------------------
!                                                      GetProjectionOfdNdXt_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNdXt4_
INTEGER(I4B) :: nsd

tsize = obj%nns
nsd = obj%nsd
ans(1:tsize) = MATMUL(obj%dNdXt(1:tsize, 1:nsd, ips), c(1:nsd))
END PROCEDURE GetProjectionOfdNdXt4_

!----------------------------------------------------------------------------
!                                                      GetProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNTdXt_1
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = obj%nns
dim2 = obj%nnt
dim3 = obj%nips

CALL Reallocate(ans, dim1, dim2, dim3)
CALL GetProjectionOfdNTdXt_(obj=obj, ans=ans, c=c, dim1=dim1, dim2=dim2, &
                            dim3=dim3)
END PROCEDURE GetProjectionOfdNTdXt_1

!----------------------------------------------------------------------------
!                                                      GetProjectionOfdNTdXt_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNTdXt1_
INTEGER(I4B) :: ips, nsd, i1, i2

dim1 = obj%nns
dim2 = obj%nnt
dim3 = obj%nips
nsd = obj%nsd

DO ips = 1, obj%nips
  CALL Matmul_(a1=obj%dNTdXt(1:dim1, 1:dim2, 1:nsd, ips), &
               a2=c(1:nsd), ans=ans(:, :, ips), nrow=i1, ncol=i2)
END DO
END PROCEDURE GetProjectionOfdNTdXt1_

!----------------------------------------------------------------------------
!                                                      GetProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNTdXt_2
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = obj%nns
dim2 = obj%nnt
dim3 = obj%nips

CALL Reallocate(ans, dim1, dim2, dim3)
CALL GetProjectionOfdNTdXt_(obj=obj, ans=ans, c=c, crank=crank, &
                            dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE GetProjectionOfdNTdXt_2

!----------------------------------------------------------------------------
!                                                      GetProjectionOfdNTdXt_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNTdXt2_
INTEGER(I4B) :: ips, nsd, i1, i2
REAL(DFP) :: cbar(3)

#ifdef DEBUG_VER
IF (c%s(1) .NE. obj%nsd) THEN
  ERROR STOP 'projection: c component count must equal nsd'
END IF
#endif

dim1 = obj%nns
dim2 = obj%nnt
dim3 = obj%nips
nsd = obj%nsd

#ifdef DEBUG_VER
IF (c%vartype .EQ. TypeFEVariableOpt%time) THEN
  ERROR STOP 'NTdXt projection: time-only c unsupported'
END IF
#endif

DO ips = 1, obj%nips
  cbar = 0.0_DFP
  CALL GetInterpolation_( &
    obj=c, RANK=crank, N=obj%N, nns=obj%nns, spaceIndx=ips, timeIndx=1_I4B, &
    T=obj%T, nnt=obj%nnt, scale=1.0_DFP, addContribution=.FALSE., ans=cbar, &
    tsize=i1)
#ifdef DEBUG_VER
  IF (i1 .NE. nsd) THEN
    ERROR STOP 'projection: interpolated component count must equal nsd'
  END IF
#endif

  CALL Matmul_(a1=obj%dNTdXt(1:dim1, 1:dim2, 1:nsd, ips), &
               a2=cbar(1:nsd), ans=ans(:, :, ips), nrow=i1, ncol=i2)
END DO
END PROCEDURE GetProjectionOfdNTdXt2_

!----------------------------------------------------------------------------
!                                                      GetProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNTdXt_3
INTEGER(I4B) :: dim1, dim2, dim3, dim4

dim1 = obj(1)%nns
dim2 = obj(1)%nnt
dim3 = obj(1)%nips
dim4 = SIZE(obj)
CALL Reallocate(ans, dim1, dim2, dim3, dim4)
CALL GetProjectionOfdNTdXt_(obj=obj, ans=ans, c=c, crank=crank, &
                            dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
END PROCEDURE GetProjectionOfdNTdXt_3

!----------------------------------------------------------------------------
!                                                      GetProjectionOfdNTdXt_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNTdXt3_
INTEGER(I4B) :: ips, ipt, nsd, i1, i2
REAL(DFP) :: cbar(3)

#ifdef DEBUG_VER
IF (c%s(1) .NE. obj(1)%nsd) THEN
  ERROR STOP 'projection: c component count must equal nsd'
END IF
#endif

dim1 = obj(1)%nns
dim2 = obj(1)%nnt
dim3 = obj(1)%nips
dim4 = SIZE(obj)
nsd = obj(1)%nsd

#ifdef DEBUG_VER
IF (c%vartype .EQ. TypeFEVariableOpt%time) THEN
  ERROR STOP 'NTdXt projection: time-only c unsupported'
END IF
#endif

DO ipt = 1, dim4
  DO ips = 1, obj(ipt)%nips
    cbar = 0.0_DFP
    CALL GetInterpolation_( &
      obj=c, RANK=crank, N=obj(ipt)%N, nns=obj(ipt)%nns, &
      spaceIndx=ips, timeIndx=ipt, T=obj(ipt)%T, nnt=obj(ipt)%nnt, &
      scale=1.0_DFP, addContribution=.FALSE., ans=cbar, tsize=i1)
#ifdef DEBUG_VER
    IF (i1 .NE. nsd) THEN
      ERROR STOP 'projection: interpolated component count must equal nsd'
    END IF
#endif

    CALL Matmul_(a1=obj(ipt)%dNTdXt(1:dim1, 1:dim2, 1:nsd, ips), &
                 a2=cbar(1:nsd), ans=ans(:, :, ips, ipt), nrow=i1, ncol=i2)
  END DO
END DO
END PROCEDURE GetProjectionOfdNTdXt3_

!----------------------------------------------------------------------------
!                                                      GetProjectionOfdNTdXt_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNTdXt4_
INTEGER(I4B) :: nsd, i1, i2
REAL(DFP) :: cbar(3)

#ifdef DEBUG_VER
IF (c%s(1) .NE. obj(ipt)%nsd) THEN
  ERROR STOP 'projection: c component count must equal nsd'
END IF
#endif

nrow = obj(ipt)%nns
ncol = obj(ipt)%nnt
nsd = obj(ipt)%nsd

#ifdef DEBUG_VER
IF (c%vartype .EQ. TypeFEVariableOpt%time) THEN
  ERROR STOP 'NTdXt projection: time-only c unsupported'
END IF
#endif

cbar = 0.0_DFP
CALL GetInterpolation_( &
  obj=c, RANK=crank, N=obj(ipt)%N, nns=obj(ipt)%nns, &
  spaceIndx=ips, timeIndx=ipt, T=obj(ipt)%T, nnt=obj(ipt)%nnt, &
  scale=1.0_DFP, addContribution=.FALSE., ans=cbar, tsize=i1)
#ifdef DEBUG_VER
IF (i1 .NE. nsd) THEN
  ERROR STOP 'projection: interpolated component count must equal nsd'
END IF
#endif

CALL Matmul_(a1=obj(ipt)%dNTdXt(1:nrow, 1:ncol, 1:nsd, ips), &
             a2=cbar(1:nsd), ans=ans, nrow=i1, ncol=i2)
END PROCEDURE GetProjectionOfdNTdXt4_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
