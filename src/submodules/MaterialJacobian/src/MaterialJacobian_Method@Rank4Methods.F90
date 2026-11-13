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
! along with this program.  If not, see <https://www.gnu.org/licenses/>

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Isotropic rank-4 tensors in Voigt form
!
! Ported from the old `Old_Rank4Tensors.part` (`ISO4`, `TraceISO4`,
! `SymISO4` and the `VoigtMat*ISO4` functions). The non-minor-symmetric
! tensors of the old file (`IdentityISO4`, `TransposeISO4`,
! `AntiSymISO4`) cannot be stored in a Voigt 6x6 and are not ported.
! All matrices follow the engineering-strain convention of
! [[MaterialJacobian_]].

SUBMODULE(MaterialJacobian_Method) Rank4Methods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                           IsotropicTangent
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_IsotropicTangent
ans = lambda * Rank4IdentityOtimesIdentity() &
      + 2.0_DFP * mu * Rank4SymIdentity()
END PROCEDURE MaterialJacobian_IsotropicTangent

!----------------------------------------------------------------------------
!                                                           Rank4SymIdentity
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_Rank4SymIdentity
INTEGER(I4B) :: ii

ans = 0.0_DFP
DO ii = 1, 3
  ans(ii, ii) = 1.0_DFP
  ans(ii + 3, ii + 3) = 0.5_DFP
END DO
END PROCEDURE MaterialJacobian_Rank4SymIdentity

!----------------------------------------------------------------------------
!                                                Rank4IdentityOtimesIdentity
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_Rank4IdentityOtimesIdentity
ans = 0.0_DFP
ans(1:3, 1:3) = 1.0_DFP
END PROCEDURE MaterialJacobian_Rank4IdentityOtimesIdentity

!----------------------------------------------------------------------------
!                                                   Rank4DeviatoricProjector
!----------------------------------------------------------------------------

MODULE PROCEDURE MaterialJacobian_Rank4DeviatoricProjector
ans = Rank4SymIdentity() - Rank4IdentityOtimesIdentity() / 3.0_DFP
END PROCEDURE MaterialJacobian_Rank4DeviatoricProjector

END SUBMODULE Rank4Methods
