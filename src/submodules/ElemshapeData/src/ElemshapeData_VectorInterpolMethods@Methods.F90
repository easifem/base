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

SUBMODULE(ElemshapeData_VectorInterpolMethods) Methods
USE ReallocateUtility, ONLY: Reallocate
USE FEVariable_Method, ONLY: FEVariableSize => Size

IMPLICIT NONE

CONTAINS

!---------------------------------------------------------------------------
!                                                          GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation1
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.
INTEGER(I4B) :: nrow, ncol

nrow = SIZE(val, 1)
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, ncol=ncol, &
                       scale=one, addContribution=no)
END PROCEDURE GetInterpolation1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_1
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.
CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, ncol=ncol, &
                       scale=one, addContribution=no)
END PROCEDURE GetInterpolation_1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_1a
INTEGER(I4B) :: valNNS, minNNS
nrow = SIZE(val, 1)
ncol = obj%nips

valNNS = SIZE(val, 2)
minNNS = MIN(valNNS, obj%nns)

IF (.NOT. addContribution) ans(1:nrow, 1:ncol) = 0.0_DFP
ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + &
                      scale * MATMUL(val(1:nrow, 1:minNNS), &
                                     obj%N(1:minNNS, 1:ncol))
END PROCEDURE GetInterpolation_1a

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation2
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.
INTEGER(I4B) :: nrow, ncol

nrow = SIZE(val, 1)
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, ncol=ncol, &
                       scale=one, addContribution=no)
END PROCEDURE GetInterpolation2

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_2
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.
CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, ncol=ncol, &
                       scale=one, addContribution=no)
END PROCEDURE GetInterpolation_2

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_2a
LOGICAL(LGT), PARAMETER :: yes = .TRUE.
INTEGER(I4B) :: minNNT, valNNT, aa
REAL(DFP) :: myscale

nrow = SIZE(val, 1)
ncol = obj%nips

valNNT = SIZE(val, 3)
minNNT = MIN(valNNT, obj%nnt)

IF (.NOT. addContribution) ans(1:nrow, 1:ncol) = 0.0_DFP

DO aa = 1, minNNT
  myscale = obj%T(aa) * scale
  CALL GetInterpolation_(obj=obj, ans=ans, val=val(:, :, aa), nrow=nrow, &
                         ncol=ncol, scale=myscale, addContribution=yes)
END DO
END PROCEDURE GetInterpolation_2a

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation3
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

INTEGER(I4B) :: dim1, dim2, dim3

dim1 = SIZE(val, 1)
dim2 = obj(1)%nips
dim3 = SIZE(obj)

CALL Reallocate(ans, dim1, dim2, dim3)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, dim2=dim2, &
                       dim3=dim3, scale=one, addContribution=no)
END PROCEDURE GetInterpolation3

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_3
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, dim2=dim2, &
                       dim3=dim3, scale=one, addContribution=no)
END PROCEDURE GetInterpolation_3

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_3a
INTEGER(I4B) :: ipt

dim3 = SIZE(obj)

DO ipt = 1, dim3
  CALL GetInterpolation_(obj=obj(ipt), ans=ans(:, :, ipt), &
                         val=val, nrow=dim1, ncol=dim2, scale=scale, &
                         addContribution=addContribution)
END DO
END PROCEDURE GetInterpolation_3a

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation4
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.
INTEGER(I4B) :: nrow, ncol

nrow = FEVariableSize(val, 1)
ncol = obj%nips

CALL Reallocate(ans, nrow, ncol)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, ncol=ncol, &
                       scale=one, addContribution=no)
END PROCEDURE GetInterpolation4

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_4
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, ncol=ncol, &
                       scale=one, addContribution=no)
END PROCEDURE GetInterpolation_4

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_4a
! INTEGER(I4B) :: ii
!
! SELECT CASE (val%vartype)
! CASE (Constant)
!   CALL Get_(obj=val, rank=TypeFEVariableVector, &
!             vartype=TypeFEVariableConstant, &
!             val=interpol(:, 1), tsize=nrow)
!   ncol = SIZE(obj%N, 2)
!   DO ii = 2, ncol
!     interpol(1:nrow, ii) = interpol(1:nrow, 1)
!   END DO
! CASE (Space)
!   IF (val%DefineOn .EQ. Nodal) THEN
!     CALL GetInterpolation_(obj=obj, &
!                            val=Get(val, TypeFEVariableVector, &
!                                    TypeFEVariableSpace), &
!                            ans=interpol, &
!                            nrow=nrow, ncol=ncol)
!   ELSE
!     CALL Get_(obj=val, rank=TypeFEVariableVector, &
!               vartype=TypeFEVariableSpace, &
!               val=interpol, nrow=nrow, ncol=ncol)
!   END IF
! CASE (SpaceTime)
!   SELECT TYPE (obj)
!   TYPE IS (STElemShapeData_)
!     CALL GetInterpolation_(obj=obj, &
!                            val=Get(val, TypeFEVariableVector, &
!                                    TypeFEVariableSpaceTime), &
!                            ans=interpol, &
!                            nrow=nrow, ncol=ncol)
!   END SELECT
! END SELECT
END PROCEDURE GetInterpolation_4a

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation5
! REAL(DFP), ALLOCATABLE :: m1(:)
! INTEGER(I4B) :: ii, jj
! INTEGER(I4B), ALLOCATABLE :: s(:)
! !!
! !! main
! !!
! s = SHAPE(val)
! CALL Reallocate(interpol, s(1), SIZE(obj(1)%N, 2), SIZE(obj))
! !!
! SELECT CASE (val%vartype)
! !!
! !! Constant
! !!
! CASE (Constant)
!   !!
!   m1 = Get(val, TypeFEVariableVector, TypeFEVariableConstant)
!   !!
!   DO jj = 1, SIZE(interpol, 3)
!     DO ii = 1, SIZE(interpol, 2)
!       interpol(:, ii, jj) = m1
!     END DO
!   END DO
!   DEALLOCATE (m1)
! !!
! !! Space
! !!
! CASE (Space)
!   !!
!   IF (val%DefineOn .EQ. Nodal) THEN
!     !!
!     DO ii = 1, SIZE(obj)
!       interpol(:, :, ii) = Interpolation(obj(ii), &
!         & Get(val, TypeFEVariableVector, TypeFEVariableSpace))
!     END DO
!     !!
!   ELSE
!     !!
!     interpol(:, :, 1) = Get(val, TypeFEVariableVector, TypeFEVariableSpace)
!     !!
!     DO ii = 2, SIZE(obj)
!       interpol(:, :, ii) = interpol(:, :, 1)
!     END DO
!     !!
!   END IF
! !!
! !! SpaceTime
! !!
! CASE (SpaceTime)
!   !!
!   IF (val%DefineOn .EQ. Nodal) THEN
!     !!
!     DO ii = 1, SIZE(obj)
!       interpol(:, :, ii) = STinterpolation(obj(ii), &
!         & Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
!     END DO
!     !!
!   ELSE
!     interpol = Get(val, TypeFEVariableVector, typeFEVariableSpaceTime)
!   END IF
! !!
! !!
! !!
! !!
! END SELECT
! !!
END PROCEDURE GetInterpolation5

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_5
! INTEGER(I4B) :: ii, jj
!
! dim1 = SIZE(val, 1)
! dim2 = SIZE(obj(1)%N, 2)
! dim3 = SIZE(obj)
! SELECT CASE (val%vartype)
! CASE (Constant)
!   CALL Get_(obj=val, rank=TypeFEVariableVector, &
!             vartype=TypeFEVariableConstant, &
!             val=interpol(:, 1, 1), tsize=dim1)
!   DO jj = 1, dim3
!     DO ii = 1, dim2
!       IF (jj .EQ. 1 .AND. ii .EQ. 1) CYCLE
!       interpol(1:dim1, ii, jj) = interpol(1:dim1, 1, 1)
!     END DO
!   END DO
! CASE (Space)
!   IF (val%DefineOn .EQ. Nodal) THEN
!     DO ii = 1, dim3
!       CALL GetInterpolation_(obj=obj(ii), &
!                              val=Get(val, TypeFEVariableVector, &
!                                      TypeFEVariableSpace), &
!                              ans=interpol(1:dim1, 1:dim2, ii), &
!                              nrow=dim1, ncol=dim2)
!     END DO
!   ELSE
!     CALL Get_(obj=val, rank=TypeFEVariableVector, &
!               vartype=TypeFEVariableSpace, &
!               val=interpol(:, :, 1), nrow=dim1, ncol=dim2)
!     DO ii = 2, SIZE(obj)
!       interpol(1:dim1, 1:dim2, ii) = interpol(1:dim1, 1:dim2, 1)
!     END DO
!   END IF
! CASE (SpaceTime)
!   IF (val%DefineOn .EQ. Nodal) THEN
!     DO ii = 1, SIZE(obj)
!       CALL GetInterpolation_(obj=obj(ii), &
!                              val=Get(val, TypeFEVariableVector, &
!                                      TypeFEVariableSpaceTime), &
!                              ans=interpol(1:dim1, 1:dim2, ii), &
!                              nrow=dim1, ncol=dim2)
!     END DO
!   ELSE
!     CALL Get_(obj=val, rank=TypeFEVariableVector, &
!               vartype=TypeFEVariableSpaceTime, &
!               val=interpol, dim1=dim1, dim2=dim2, dim3=dim3)
!   END IF
! END SELECT
END PROCEDURE GetInterpolation_5

!----------------------------------------------------------------------------
!                                                               Interpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE Interpolation1
! interpol = MATMUL(val, obj%N)
END PROCEDURE Interpolation1

!----------------------------------------------------------------------------
!                                                             STInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE STInterpolation1

END PROCEDURE STInterpolation1

END SUBMODULE Methods
