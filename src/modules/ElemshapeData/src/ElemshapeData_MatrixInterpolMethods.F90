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
!
! This file contains the interpolation methods interfaces\

MODULE ElemshapeData_MatrixInterpolMethods
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: ElemShapeData_, STElemShapeData_, FEVariable_
IMPLICIT NONE
PRIVATE

PUBLIC :: GetInterpolation
PUBLIC :: GetInterpolation_
PUBLIC :: Interpolation
PUBLIC :: STInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolation of matrix

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation1(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! interpolation of matrix
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! nodal value of matrix
  END SUBROUTINE GetInterpolation1
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  get interpolation of matrix without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1(obj, ans, val, &
                                            dim1, dim2, dim3)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE GetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  get interpolation of matrix without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1a(obj, ans, val, &
                                             dim1, dim2, dim3, scale, &
                                             addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_1a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolation of matrix
!
!# Introduction
!
! This subroutine performs interpolation of matrix from its space-time
! nodal values

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation2(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: ans(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal value of matrix
  END SUBROUTINE GetInterpolation2
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  get interpolation of matrix without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2(obj, ans, val, &
                                            dim1, dim2, dim3)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE GetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  get interpolation of matrix without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2a(obj, ans, val, &
                                             dim1, dim2, dim3, scale, &
                                             addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_2a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolation of matrix
!
!# Introduction
!
! This subroutine performs interpolation of matrix from its space-time
! nodal values

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation3(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: ans(:, :, :, :)
    !! space-time interpolation
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal value of matrix
  END SUBROUTINE GetInterpolation3
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolation of matrix
!
!# Introduction
!
! This subroutine performs interpolation of matrix from its space-time
! nodal values

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_3(obj, ans, val, dim1, dim2, &
                                            dim3, dim4)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! space-time interpolation
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal value of matrix
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    !! size of data written in ans
  END SUBROUTINE GetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolation of matrix
!
!# Introduction
!
! This subroutine performs interpolation of matrix from its space-time
! nodal values

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_3a(obj, ans, val, dim1, dim2, &
                                             dim3, dim4, scale, &
                                             addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! space-time interpolation
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal value of matrix
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    !! size of data written in ans
    REAL(DFP), INTENT(IN) :: scale
    !! scaling factor
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_3a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolation of matrix FEVariable
!
INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation4(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! interpolation of matrix
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix fe variable
  END SUBROUTINE GetInterpolation4
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  get interpolation of matrix without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_4(obj, ans, val, &
                                            dim1, dim2, dim3)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE GetInterpolation_4
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-04
! summary: Get interpolation of matrix without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_4a(obj, ans, val, &
                                             dim1, dim2, dim3, scale, &
                                             addContribution, timeIndx)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
    INTEGER(I4B), INTENT(IN), OPTIONAL :: timeIndx
  END SUBROUTINE GetInterpolation_4a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-04
! summary:  get interpolation of matrix without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_4b(obj, ans, val, &
                                             nrow, ncol, scale, &
                                             addContribution, spaceIndx, &
                                             timeIndx)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
    INTEGER(I4B), INTENT(IN) :: timeIndx, spaceIndx
  END SUBROUTINE GetInterpolation_4b
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation@Methods
!----------------------------------------------------------------------------

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation5(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
    !! space-time interpolation of matrix
    TYPE(FEVariable_), INTENT(IN) :: val
    !! matrix fe variable
  END SUBROUTINE GetInterpolation5
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                  GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  get interpolation of matrix without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_5(obj, ans, val, &
                                            dim1, dim2, dim3, dim4)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
  END SUBROUTINE GetInterpolation_5
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                  GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-04
! summary:  get interpolation of matrix without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_5a(obj, ans, val, &
                                             dim1, dim2, dim3, dim4, &
                                             scale, addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_5a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                       Interpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of matrix

INTERFACE
  MODULE PURE FUNCTION Interpolation1(obj, val) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
  END FUNCTION Interpolation1
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE Interpolation1
END INTERFACE Interpolation

!----------------------------------------------------------------------------
!                                                     STInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: This function performs interpolations of matrix
!
!!# Introduction
!
! This function performs interpolation of a matrix from its space-time nodal
! values.
! $$u=u^{a}_{I}N^{I}T_{a}$$

INTERFACE

  MODULE PURE FUNCTION STInterpolation1(obj, val) RESULT(ans)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! spatial nodal values of matrix
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
    !! Interpolation value of matrix
  END FUNCTION STInterpolation1
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE STInterpolation1
END INTERFACE STInterpolation

END MODULE ElemshapeData_MatrixInterpolMethods
