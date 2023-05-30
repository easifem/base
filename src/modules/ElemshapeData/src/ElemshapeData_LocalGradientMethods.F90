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

module ElemshapeData_LocalGradientMethods
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: getLocalGradient
PUBLIC :: LocalGradient

!----------------------------------------------------------------------------
!                                           getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a scalar
!
! $$
! \frac{\partial \phi }{\partial \xi_{i} } =\phi_{I} \frac{\partial N^{I}}
! {\partial \xi_{i} }
! $$
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_1(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! local gradients of scalar
    REAL(DFP), INTENT(IN) :: val(:)
    !! Space nodal values of scalar
  END SUBROUTINE elemsd_getLocalGradient_1
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_1
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                           getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a vector

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_2(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :)
    !! local gradient at integration points
    !! first index: space component of V
    !! second index: space component of x
    !! third index: integration point
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space nodal values of vector in `xiJ` format
    !! row index: space component
    !! col index: node number
  END SUBROUTINE elemsd_getLocalGradient_2
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_2
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                          getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a scalar
!
! $$
! \frac{\partial \phi }{\partial \xi_{i} } =\phi^{a}_{I} T_{a}\frac
! {\partial N^{I}}{\partial \xi_{i} }
! $$
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_3(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! local gradient of scalar (space-time nodal)
    !! first index = space component of xi
    !! second index= integration point in space
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values of scalar
    !! first index = space node
    !! second index = time node
  END SUBROUTINE elemsd_getLocalGradient_3
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_3
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                          getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a vector
!
! $$
! \frac{\partial v_{i} }{\partial \xi_{j} } =v^{a}_{iI} T_{a}\frac
! {\partial N^{I}}{\partial \xi_{j} }
! $$
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_4(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :)
    !! local gradient at integration points
    !! first index : space compo of V
    !! second index: space compo of Xi
    !! third index: integration point in space
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
    !! first index: space compo
    !! second index: space node
    !! third index: time node
  END SUBROUTINE elemsd_getLocalGradient_4
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_4
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                          getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a scalar
!
! $$
! \frac{\partial \phi }{\partial \xi_{i} } =\phi_{I} \frac{\partial N^{I}}
! {\partial \xi_{i} }
! $$
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_5(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :)
    !! local gradient of scalar
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Scalar finite elememt variable
  END SUBROUTINE elemsd_getLocalGradient_5
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_5
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                           getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a vector
!
! $$
! \frac{\partial v_{i} }{\partial \xi_{j} } =v^{a}_{iI} T_{a}\frac
! {\partial N^{I}}{\partial \xi_{j} }
! $$
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_6(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :)
    !! local gradient of vector at integration points
    !! first index : space compo of V
    !! second index: space compo of Xi
    !! third index: integration point in space
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector fe variable
  END SUBROUTINE elemsd_getLocalGradient_6
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_6
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                           getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a vector

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_7(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :, :)
    !! local gradient at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space nodal values of matrix in (i,j,I) format
  END SUBROUTINE elemsd_getLocalGradient_7
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_7
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                           getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a vector

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_8(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :, :)
    !! local gradient at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values of matrix in (i,j,I,a) format
  END SUBROUTINE elemsd_getLocalGradient_8
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_8
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                           getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient of a vector

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_9(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: lg(:, :, :, :)
    !! local gradient at integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space nodal values of matrix in (i,j,I) format
  END SUBROUTINE elemsd_getLocalGradient_9
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_9
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                           getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the local gradient
!
!# Introduction
!
! - This routine returns local gradient in [[FEVariable_]]
! the input is also a [[FEVariable_]].
! - This routine can be considered as a master routine
!
INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_10(obj, lg, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: lg
    !! local gradient of scalar/vector/matrix at space integration points
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE elemsd_getLocalGradient_10
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_10
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!                                         getLocalGradient@GradientMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! update: 2021-11-26
! summary: This subroutine returns the Local gradient
!
!# Introduction
!
! - This routine returns Local gradient in [[FEVariable_]]
! the input is also a [[FEVariable_]].
! - This routine can be considered as a master routine

INTERFACE
  MODULE PURE SUBROUTINE elemsd_getLocalGradient_11(obj, lg, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(INOUT) :: lg
    !! Local gradient of scalar/vector/matrix at space-time
    !! integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space time nodal values of scalar/vector/matrix
  END SUBROUTINE elemsd_getLocalGradient_11
END INTERFACE

INTERFACE getLocalGradient
  MODULE PROCEDURE elemsd_getLocalGradient_11
END INTERFACE getLocalGradient

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION elemsd_LocalGradient_1(obj, val) RESULT(Ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION elemsd_LocalGradient_1
END INTERFACE

INTERFACE localGradient
  MODULE PROCEDURE elemsd_LocalGradient_1
END INTERFACE localGradient

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION elemsd_LocalGradient_2(obj, val) RESULT(Ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION elemsd_LocalGradient_2
END INTERFACE

INTERFACE LocalGradient
  MODULE PROCEDURE elemsd_LocalGradient_2
END INTERFACE LocalGradient

end module ElemshapeData_LocalGradientMethods
