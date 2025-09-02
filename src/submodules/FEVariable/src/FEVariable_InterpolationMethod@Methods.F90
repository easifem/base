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

SUBMODULE(FEVariable_InterpolationMethod) Methods
USE FEVariable_Method, ONLY: FEVariableCopy => Copy, &
                             FEVariableGetInterpolation_ => GetInterpolation_
USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableVector, &
                    TypeFEVariableMatrix, &
                    TypeFEVariableConstant, &
                    TypeFEVariableSpace, &
                    TypeFEVariableTime, &
                    TypeFEVariableSpaceTime

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE FEVariableGetInterpolation_1
INTEGER(I4B) :: timeIndx

timeIndx = 1

! if val is a nodal variable then interpolate
SELECT CASE (obj%rank)

CASE (TypeFEVariableOpt%scalar)

  SELECT CASE (obj%vartype)
  CASE (TypeFEVariableOpt%constant)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableScalar, &
                                     vartype=TypeFEVariableConstant, &
                                     N=N, nns=nns, nips=nips, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)

  CASE (TypeFEVariableOpt%space, TypeFEVariableOpt%spacetime)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableScalar, &
                                     vartype=TypeFEVariableSpace, &
                                     N=N, nns=nns, nips=nips, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)
    ! CASE (TypeFEVariableOpt%time)

  END SELECT

CASE (TypeFEVariableOpt%vector)

  SELECT CASE (obj%vartype)
  CASE (TypeFEVariableOpt%constant)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableVector, &
                                     vartype=TypeFEVariableConstant, &
                                     N=N, nns=nns, nips=nips, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)

  CASE (TypeFEVariableOpt%space, TypeFEVariableOpt%spacetime)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableVector, &
                                     vartype=TypeFEVariableSpace, &
                                     N=N, nns=nns, nips=nips, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)
    ! CASE (TypeFEVariableOpt%time)

  END SELECT

CASE (TypeFEVariableOpt%matrix)

  SELECT CASE (obj%vartype)
  CASE (TypeFEVariableOpt%constant)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
                                     vartype=TypeFEVariableConstant, &
                                     N=N, nns=nns, nips=nips, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)

  CASE (TypeFEVariableOpt%space, TypeFEVariableOpt%spacetime)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
                                     vartype=TypeFEVariableSpace, &
                                     N=N, nns=nns, nips=nips, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)
    ! CASE (TypeFEVariableOpt%time)

  END SELECT

END SELECT

END PROCEDURE FEVariableGetInterpolation_1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE FEVariableGetInterpolation_2
! if val is a nodal variable then interpolate
SELECT CASE (obj%rank)

CASE (TypeFEVariableOpt%scalar)

  SELECT CASE (obj%vartype)
  CASE (TypeFEVariableOpt%constant)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableScalar, &
                                     vartype=TypeFEVariableConstant, &
                                     N=N, nns=nns, nips=nips, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)
  CASE (TypeFEVariableOpt%space)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableScalar, &
                                     vartype=TypeFEVariableSpace, &
                                     N=N, nns=nns, nips=nips, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)
    ! CASE (TypeFEVariableOpt%time)

  CASE (TypeFEVariableOpt%spacetime)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableScalar, &
                                     vartype=TypeFEVariableSpaceTime, &
                                     N=N, nns=nns, nips=nips, &
                                     T=T, nnt=nnt, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)

  END SELECT

CASE (TypeFEVariableOpt%vector)

  SELECT CASE (obj%vartype)
  CASE (TypeFEVariableOpt%constant)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableVector, &
                                     vartype=TypeFEVariableConstant, &
                                     N=N, nns=nns, nips=nips, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)
  CASE (TypeFEVariableOpt%space)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableVector, &
                                     vartype=TypeFEVariableSpace, &
                                     N=N, nns=nns, nips=nips, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)
    ! CASE (TypeFEVariableOpt%time)

  CASE (TypeFEVariableOpt%spacetime)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableVector, &
                                     vartype=TypeFEVariableSpaceTime, &
                                     N=N, nns=nns, nips=nips, &
                                     T=T, nnt=nnt, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)

  END SELECT

CASE (TypeFEVariableOpt%matrix)

  SELECT CASE (obj%vartype)
  CASE (TypeFEVariableOpt%constant)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
                                     vartype=TypeFEVariableConstant, &
                                     N=N, nns=nns, nips=nips, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)
  CASE (TypeFEVariableOpt%space)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
                                     vartype=TypeFEVariableSpace, &
                                     N=N, nns=nns, nips=nips, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)
    ! CASE (TypeFEVariableOpt%time)

  CASE (TypeFEVariableOpt%spacetime)
    CALL FEVariableGetInterpolation_(obj=obj, rank=TypeFEVariableMatrix, &
                                     vartype=TypeFEVariableSpaceTime, &
                                     N=N, nns=nns, nips=nips, &
                                     T=T, nnt=nnt, scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, timeIndx=timeIndx)

  END SELECT

END SELECT

END PROCEDURE FEVariableGetInterpolation_2

END SUBMODULE Methods
