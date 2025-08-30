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

SUBMODULE(ElemshapeData_ScalarInterpolMethods) Methods
USE ReallocateUtility, ONLY: Reallocate
USE BaseType, ONLY: TypeFEVariableOpt, TypeFEVariableScalar, &
                    TypeFEVariableConstant, TypeFEVariableSpace, &
                    TypeFEVariableSpaceTime
USE FEVariable_Method, ONLY: FEVariableGetInterpolation_ => GetInterpolation_
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            GetInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation1
INTEGER(I4B) :: tsize
CALL Reallocate(ans, obj%nips)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, &
                       tsize=tsize, scale=1.0_DFP, addContribution=.FALSE.)
END PROCEDURE GetInterpolation1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_1
CALL GetInterpolation_(obj=obj, ans=ans, val=val, &
                       tsize=tsize, scale=1.0_DFP, addContribution=.FALSE.)
END PROCEDURE GetInterpolation_1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_1a
INTEGER(I4B) :: minNNS, valNNS, ips, ii

tsize = obj%nips
valNNS = SIZE(val)
minNNS = MIN(valNNS, obj%nns)

IF (.NOT. addContribution) ans(1:tsize) = 0.0_DFP

!ans(1:obj%nips) = MATMUL(val(1:minNNS), obj%N(1:minNNS, 1:obj%nips))
DO ips = 1, obj%nips
  DO ii = 1, minNNS
    ans(ips) = ans(ips) + scale * val(ii) * obj%N(ii, ips)
  END DO
END DO
END PROCEDURE GetInterpolation_1a

!----------------------------------------------------------------------------
!                                                            GetInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation2
INTEGER(I4B) :: tsize
CALL Reallocate(ans, obj%nips)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, &
                       tsize=tsize, scale=1.0_DFP, addContribution=.FALSE.)
END PROCEDURE GetInterpolation2

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_2
CALL GetInterpolation_(obj=obj, ans=ans, val=val, &
                       tsize=tsize, scale=1.0_DFP, addContribution=.FALSE.)
END PROCEDURE GetInterpolation_2

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_2a
INTEGER(I4B) :: minNNT, valNNT, aa
REAL(DFP) :: myscale

tsize = 0 !! We will read tsize in the loop below
valNNT = SIZE(val, 2)
minNNT = MIN(valNNT, obj%nnt)

IF (.NOT. addContribution) ans(1:obj%nips) = 0.0_DFP

DO aa = 1, minNNT
  myscale = obj%T(aa) * scale
  CALL GetInterpolation_(obj=obj, ans=ans, val=val(:, aa), &
                         tsize=tsize, scale=myscale, addContribution=.TRUE.)
END DO
END PROCEDURE GetInterpolation_2a

!----------------------------------------------------------------------------
!                                                            GetInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation3
INTEGER(I4B) :: nrow, ncol

nrow = obj(1)%nips
ncol = SIZE(obj)
CALL Reallocate(ans, nrow, ncol)
CALL GetInterpolation_(obj=obj, ans=ans, &
                       val=val, nrow=nrow, ncol=ncol, scale=1.0_DFP, &
                       addContribution=.FALSE.)
END PROCEDURE GetInterpolation3

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_3
CALL GetInterpolation_(obj=obj, ans=ans, &
                       val=val, nrow=nrow, ncol=ncol, scale=1.0_DFP, &
                       addContribution=.FALSE.)
END PROCEDURE GetInterpolation_3

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_3a
INTEGER(I4B) :: ipt

nrow = 0 !! We will read nrow in the loop below
ncol = SIZE(obj)

DO ipt = 1, ncol
  CALL GetInterpolation_(obj=obj(ipt), ans=ans(:, ipt), &
                         val=val, tsize=nrow, scale=scale, &
                         addContribution=addContribution)
END DO
END PROCEDURE GetInterpolation_3a

!----------------------------------------------------------------------------
!                                                            GetInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation4
INTEGER(I4B) :: tsize
CALL Reallocate(ans, obj%nips)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, tsize=tsize)
END PROCEDURE GetInterpolation4

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_4
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.
CALL GetInterpolation_(obj=obj, ans=ans, val=val, tsize=tsize, &
                       scale=one, addContribution=no)
END PROCEDURE GetInterpolation_4

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_4a
INTEGER(I4B) :: timeIndx0

timeIndx0 = 1_I4B
IF (PRESENT(timeIndx)) timeIndx0 = timeIndx

SELECT CASE (val%vartype)
CASE (TypeFEVariableOpt%constant)
  CALL FEVariableGetInterpolation_(obj=val, rank=TypeFEVariableScalar, &
                                   vartype=TypeFEVariableConstant, &
                                   N=obj%N, nns=obj%nns, nips=obj%nips, &
                                   scale=scale, &
                                   addContribution=addContribution, &
                                   ans=ans, tsize=tsize)

CASE (TypeFEVariableOpt%space)
  CALL FEVariableGetInterpolation_(obj=val, rank=TypeFEVariableScalar, &
                                   vartype=TypeFEVariableSpace, &
                                   N=obj%N, nns=obj%nns, nips=obj%nips, &
                                   scale=scale, &
                                   addContribution=addContribution, &
                                   ans=ans, tsize=tsize)

CASE (TypeFEVariableOpt%spacetime)
  SELECT TYPE (obj); TYPE IS (STElemShapeData_)
    CALL FEVariableGetInterpolation_(obj=val, rank=TypeFEVariableScalar, &
                                     vartype=TypeFEVariableSpaceTime, &
                                     N=obj%N, nns=obj%nns, nips=obj%nips, &
                                     T=obj%T, nnt=obj%nnt, &
                                     scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, tsize=tsize, &
                                     timeIndx=timeIndx0)

  END SELECT

END SELECT
END PROCEDURE GetInterpolation_4a

!----------------------------------------------------------------------------
!                                                            GetInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation5
INTEGER(I4B) :: nrow, ncol
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

nrow = obj(1)%nips
ncol = SIZE(obj)
CALL Reallocate(ans, nrow, ncol)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, &
                       ncol=ncol, scale=one, addContribution=no)
END PROCEDURE GetInterpolation5

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_5
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, &
                       ncol=ncol, scale=one, addContribution=no)
END PROCEDURE GetInterpolation_5

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_5a
INTEGER(I4B) :: ipt

nrow = 0
ncol = SIZE(obj)

DO ipt = 1, ncol
  CALL GetInterpolation_(obj=obj(ipt), ans=ans(:, ipt), &
                         val=val, tsize=nrow, scale=scale, &
                         addContribution=addContribution, timeIndx=ipt)
END DO
END PROCEDURE GetInterpolation_5a

!----------------------------------------------------------------------------
!                                                               Interpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE Interpolation1
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.
INTEGER(I4B) :: tsize
CALL Reallocate(ans, obj%nips)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, tsize=tsize, &
                       scale=one, addContribution=no)
END PROCEDURE Interpolation1

!----------------------------------------------------------------------------
!                                                             STInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE STInterpolation1
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.
INTEGER(I4B) :: tsize
CALL Reallocate(ans, obj%nips)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, tsize=tsize, &
                       scale=one, addContribution=no)
END PROCEDURE STInterpolation1

END SUBMODULE Methods
