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

MODULE ForceVector_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------


!> author: Vikas Sharma, Ph. D.
! date: 4 May 2022
! summary: Force vector
!
!# Introduction
!
! This subroutine computes the following expression:
!
! $$
! F_{I}=\int_{\Omega}N^{I}d\Omega
! $$

INTERFACE
  MODULE PURE FUNCTION ForceVector_1(test) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION ForceVector_1
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector_1
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------


!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! $$
! F_{I}=\int_{\Omega}\rho N^{I}d\Omega
! $$

INTERFACE
  MODULE PURE FUNCTION ForceVector_2(test, c, crank) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT( IN ) :: c
    TYPE(FEVariableScalar_), INTENT( IN ) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION ForceVector_2
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector_2
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------


!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! This routine computes the following integral
!
! $$
! F(i,I)=\int_{\Omega}v_{i}N^{I}d\Omega
! $$

INTERFACE
  MODULE PURE FUNCTION ForceVector_3(test, c, crank) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT( IN ) :: c
    TYPE(FEVariableVector_), INTENT( IN ) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ForceVector_3
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector_3
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! This routine computes the following integral
!
! $$
! F(i,j,I)=\int_{\Omega}k_{ij}N^{I}d\Omega
! $$

INTERFACE
  MODULE PURE FUNCTION ForceVector_4(test, c, crank) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT( IN ) :: c
    TYPE(FEVariableMatrix_), INTENT( IN ) :: crank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION ForceVector_4
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector_4
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------


!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! This routine computes the following integral
!
! $$
! F_{I}=\int_{\Omega}\rho_{1}\rho_{2}N^{I}d\Omega
! $$

INTERFACE
  MODULE PURE FUNCTION ForceVector_5(test, c1, c1rank, c2, c2rank) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    TYPE(FEVariableScalar_), INTENT( IN ) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION ForceVector_5
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector_5
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------


!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! This routine computes the following integral.
!
! $$
!
! $$

INTERFACE
  MODULE PURE FUNCTION ForceVector_6(test, c1, c1rank, c2, c2rank) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    TYPE(FEVariableVector_), INTENT( IN ) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION ForceVector_6
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector_6
END INTERFACE ForceVector

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------


!> author: Vikas Sharma, Ph. D.
! date: 20 Jan 2022
! summary: Force vector
!
!# Introduction
!
! This routine computes the following.
!
! $$
! F(i,j,I)=\int_{\Omega}\rho k_{ij}N^{I}d\Omega
! $$

INTERFACE
  MODULE PURE FUNCTION ForceVector_7(test, c1, c1rank, c2, c2rank) &
    & RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT( IN ) :: c1
    TYPE(FEVariable_), INTENT( IN ) :: c2
    TYPE(FEVariableScalar_), INTENT( IN ) :: c1rank
    TYPE(FEVariableMatrix_), INTENT( IN ) :: c2rank
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION ForceVector_7
END INTERFACE

INTERFACE ForceVector
  MODULE PROCEDURE ForceVector_7
END INTERFACE ForceVector

END MODULE ForceVector_Method