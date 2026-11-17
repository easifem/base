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


!> author: Shion Shimizu
! date: 2026-11-17
! summary: Tensor operations on [[VoigtRank2Tensor_]]
!
! Thin wrappers around the array routines of VoigtUtility. The shear
! entries are converted to tensor components with the scale of the
! tensor where the result depends on it.

SUBMODULE(VoigtRank2Tensor_Method) TensorMethods
USE VoigtUtility, ONLY: VoigtDev
USE VoigtUtility, ONLY: VoigtOuter
USE VoigtUtility, ONLY: VoigtStressNorm
USE VoigtUtility, ONLY: VoigtSymEigen
USE VoigtUtility, ONLY: VoigtTrace
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                      Trace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Trace
ans = VoigtTrace(obj%V)
END PROCEDURE obj_Trace

!----------------------------------------------------------------------------
!                                                                        Dev
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Dev
ans = obj
ans%V = VoigtDev(obj%V)
END PROCEDURE obj_Dev

!----------------------------------------------------------------------------
!                                                              FrobeniusNorm
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_FrobeniusNorm
ans = VoigtStressNorm(TensorComponents(obj))
END PROCEDURE obj_FrobeniusNorm

!----------------------------------------------------------------------------
!                                                                      Eigen
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Eigen
CALL VoigtSymEigen(v=TensorComponents(obj), vals=WR, vecs=QR)
END PROCEDURE obj_Eigen

!----------------------------------------------------------------------------
!                                                                     Otimes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Otimes
ans = VoigtOuter(TensorComponents(obj1), TensorComponents(obj2))
END PROCEDURE obj_Otimes

!----------------------------------------------------------------------------
!                                                           TensorComponents
!----------------------------------------------------------------------------

!> Voigt vector with tensor shear components, V(4:6) times the scale

PURE FUNCTION TensorComponents(obj) RESULT(ans)
  TYPE(VoigtRank2Tensor_), INTENT(IN) :: obj
  !! Voigt tensor
  REAL(DFP) :: ans(6)
  !! [T11, T22, T33, T12, T23, T13]

  ans(1:3) = obj%V(1:3)
  ans(4:6) = obj%Scale * obj%V(4:6)
END FUNCTION TensorComponents

END SUBMODULE TensorMethods
