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

MODULE ForceVectorNormal_Method
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: FEVariableScalar_
USE BaseType, ONLY: FEVariableVector_
USE BaseType, ONLY: FEVariableMatrix_
IMPLICIT NONE
PRIVATE

PUBLIC :: ForceVectorNormal_

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# ForceVectorNormal_
!
! This subroutine computes the following expression:
!
! $$
! F_{I}=\int_{\Omega}N^{I} \mathbf{c} \cdot \mathbf{n} d\Omega
! $$

INTERFACE ForceVectorNormal_
  MODULE SUBROUTINE ForceVectorNormal_1(test, c, ans, tsize, scale, &
                                        addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    REAL(DFP), INTENT(IN) :: c(:)
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE ForceVectorNormal_1
END INTERFACE ForceVectorNormal_

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# ForceVectorNormal_
!
! This subroutine computes the following expression:
!
! $$
! F_{I}=\int_{\Omega}N^{I} \mathbf{c} \cdot \mathbf{n} d\Omega
! $$

INTERFACE ForceVectorNormal_
  MODULE SUBROUTINE ForceVectorNormal_2(test, c, crank, ans, tsize, &
                                        scale, addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c
    TYPE(FEVariableVector_), INTENT(IN) :: crank
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE ForceVectorNormal_2
END INTERFACE ForceVectorNormal_

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# ForceVectorNormal_
!
! This subroutine computes the following expression:
!
! $$
! F_{I}=\int_{\Omega}N^{I} c_{1} \mathbf{c}_{2} \cdot \mathbf{n} d\Omega
! $$

INTERFACE ForceVectorNormal_
  MODULE SUBROUTINE ForceVectorNormal_3(test, c1, c1rank, c2, ans, tsize, &
                                        scale, addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    REAL(DFP), INTENT(IN) :: c2(:)
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE ForceVectorNormal_3
END INTERFACE ForceVectorNormal_

!----------------------------------------------------------------------------
!                                                         ForceVectorNormal_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-21
! summary: Compute normal force vector
!
!# ForceVectorNormal_
!
! This subroutine computes the following expression:
!
! $$
! F_{I}=\int_{\Omega}N^{I} c_{1} \mathbf{c}_{2} \cdot \mathbf{n} d\Omega
! $$

INTERFACE ForceVectorNormal_
  MODULE SUBROUTINE ForceVectorNormal_4(test, c1, c1rank, c2, c2rank, ans, &
                                        tsize, scale, addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: test
    TYPE(FEVariable_), INTENT(IN) :: c1
    TYPE(FEVariableScalar_), INTENT(IN) :: c1rank
    TYPE(FEVariable_), INTENT(IN) :: c2
    TYPE(FEVariableVector_), INTENT(IN) :: c2rank
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE ForceVectorNormal_4
END INTERFACE ForceVectorNormal_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ForceVectorNormal_Method
