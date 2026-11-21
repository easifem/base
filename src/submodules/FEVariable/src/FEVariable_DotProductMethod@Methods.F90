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

SUBMODULE(FEVariable_DotProductMethod) Methods
USE FEVariable_ConstructorMethod, ONLY: Initiate
USE FEVariable_MultiplicationMethod, ONLY: OPERATOR(*)

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                DOT_PRODUCT
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_dot_product
TYPE(FEVariable_) :: tmp
INTEGER(I4B) :: ncomp, npoint, ii, tshape, s(TypeFEVariableOpt%maxRank)

! element-wise product, a vector with shape [ncomp, space, time]
tmp = obj1 * obj2

ncomp = tmp%s(1)
npoint = tmp%len / ncomp

! the result drops the component dimension
IF (tmp%tshape .EQ. 1) THEN
  tshape = 1
  s(1) = 1
ELSE
  tshape = tmp%tshape - 1
  s(1:tshape) = tmp%s(2:tmp%tshape)
END IF

CALL Initiate(obj=ans, s=s(1:tshape), defineon=tmp%defineOn, &
              vartype=tmp%varType, rank=TypeFEVariableOpt%scalar, &
              len=npoint)

DO ii = 1, npoint
  ans%val(ii) = SUM(tmp%val((ii - 1) * ncomp + 1:ii * ncomp))
END DO

END PROCEDURE fevar_dot_product

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
