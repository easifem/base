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

MODULE ElemshapeData_GradientMethods
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: FEVariableScalar_
USE BaseType, ONLY: FEVariableVector_
USE BaseType, ONLY: FEVariableMatrix_
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: GetSpatialGradient
PUBLIC :: GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar
!
!# GetSpatialGradient
!
! This method returns the spatial gradient of scalar
!

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient1(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! Spatial gradient of scalar
    REAL(DFP), INTENT(IN) :: val(:)
    !! Nodal values of scalar
  END SUBROUTINE obj_GetSpatialGradient1
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar
!
!# GetSpatialGradient_
!
! This method returns the spatial gradient of scalar. The result will be
! kept in ans. The first index of ans denotes the space component, and
! the second index of ans denotes the spatial nodal value.
!

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_1(obj, val, ans, nrow, ncol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    !! Nodal values of scalar
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Spatial gradient of scalar
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! data written nrow
  END SUBROUTINE obj_GetSpatialGradient_1
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of vector
!
!# GetSpatialGradient
!
! This method returns the spatial gradient of vector.
! The value of vector is kept in val.
! The first index of val denotes the space component.
! The second index of val denotes the nodal value.
!
! The result will returned in ans.
!
! The first index denotes space component of val.
! The second index of ans denote the space component
! The third index denotes the space components.

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient2(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient of vector at integration points
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values of vector in `xiJ` format
  END SUBROUTINE obj_GetSpatialGradient2
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of vector
!
!# GetSpatialGradient_
!
! This method returns the spatial gradient of vector.
! The value of vector is kept in val.
! The first index of val denotes the space component.
! The second index of val denotes the nodal value.
!
! The result will returned in ans.
!
! The first index denotes space component of val.
! The second index of ans denote the space component
! The third index denotes the space components.

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_2( &
    obj, val, ans, dim1, dim2, dim3)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! nodal values of vector in `xiJ` format
    !! first index is space component.
    !! second index is spatial nodal value
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient of vector at integration points
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE obj_GetSpatialGradient_2
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                          GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar
!
!# GetSpatialGradient
!
! This method returns the spatial gradient of scalar.
! Here val denotes the space-time nodal values of scalar.
!

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient3(obj, ans, val)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! This should be a STElemShapeData_
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! Spatial gradient of scalar
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time Nodal values of scalar
  END SUBROUTINE obj_GetSpatialGradient3
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar
!
!# GetSpatialGradient_
!
! This method returns the spatial gradient of scalar.
! Here val denotes the space-time nodal values of scalar.
! The first index of val denotes the spatial nodal value.
! The second index of val denotes the temporal nodal value.
!
! The result will be returned in ans. The first index of ans denotes
! the space component, and the second component denotes the
! spatial quadrature.

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_3( &
    obj, val, ans, nrow, ncol)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    !! This should be a STElemShapeData_
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time Nodal values of scalar
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Spatial gradient of scalar
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetSpatialGradient_3
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                          GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of vector
!
!# GetSpatialGradient
!
! This method returns the spatial gradient of vector.
! Here val denotes the space-time nodal values of vector.
!

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient4(obj, ans, val)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient of vector at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
    !! first index: space component
    !! second index: space nodal value
    !! third index: time nodal value
  END SUBROUTINE obj_GetSpatialGradient4
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of vector
!
!# GetSpatialGradient_
!
! This method returns the spatial gradient of vector.
! Here val denotes the space-time nodal values of vector.
! The first index of val denotes the space component of val.
! The second index of val denotes the spatial nodal value and the
! third index of val denotes the temporal nodal value of val.
! The size of val should be extact
!
! The result will be returned in ans. The first index of ans denotes the
! space component of val, the second index denotes the space component,
! the thrid index of ans denotes the space quadrature value.
!

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_4( &
    obj, val, ans, dim1, dim2, dim3)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space-time nodal values of vector in `xiJa` format
    !! first index: space component
    !! second index: space nodal value
    !! third index: time nodal value
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient of vector at integration points
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! extent of data written in ans
  END SUBROUTINE obj_GetSpatialGradient_4
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                          GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar
!
!# GetSpatialGradient
!
! This method returns the spatial gradient of scalar.
! The values of scalar field is stored inside the fevariable `val`.
! The fevariable can be constant, space, and spacetime dependent
!

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient5(obj, ans, val, valRank)
    TYPE(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! Spatial gradient of scalar
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Nodal values of scalar
    TYPE(FEVariableScalar_), INTENT(IN) :: valRank
  END SUBROUTINE obj_GetSpatialGradient5
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar
!
!# GetSpatialGradient_
!
! This method returns the spatial gradient of scalar.
!
! The values of scalar field is stored inside the fevariable `val`.
! The fevariable can be constant, space, and spacetime dependent
!
! Note that the FEVariable should be nodalVariable, that is, it should
! be defined on the nodes and not on the quadrature.
!
! Note that the FEVariable cannot be a space-time variable in this case.
! If it is a space-time variable then obj should be a STElemShapeData_
!
! The result will be stored inside ans. The first index of ans
! denotes space component, and second index denotes spatial node.

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_5( &
    obj, val, valRank, ans, nrow, ncol, tempVec)
    TYPE(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Nodal values of scalar
    TYPE(FEVariableScalar_), INTENT(IN) :: valRank
    !! nodal value of scalar
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Spatial gradient of scalar
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! extent of data written in ans
    REAL(DFP), INTENT(INOUT) :: tempVec(:)
    !! temporary vector for internal use, its size should be
    !! atleast obj%nns
  END SUBROUTINE obj_GetSpatialGradient_5
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of vector
!
!# GetSpatialGradient
!
! This method returns the spatial gradient of vector.
! The values of vector field is stored inside the fevariable `val`.
! The fevariable can be constant, space, and spacetime dependent.
!
INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient6(obj, ans, val, valRank)
    TYPE(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient of vector at integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space-time nodal values of vector in `xiJa` format
    TYPE(FEVariableVector_), INTENT(IN) :: valRank
  END SUBROUTINE obj_GetSpatialGradient6
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of vector
!
!# GetSpatialGradient
!
! This method returns the spatial gradient of vector.
! The values of vector field is stored inside the fevariable `val`.
!
! The fevariable can be constant, space, and spacetime dependent.
!
! The fevariable should be a vector which is defined on nodes.
! The fevariable should not be space-time,
! it can be only constant or space dependent.
!
! If fevariable is space-time then obj should be STElemShapeData_
!
INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_6( &
    obj, val, valRank, ans, dim1, dim2, dim3, tempMat2)
    TYPE(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: valRank
    !! vector finite element variable
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient of vector at integration points
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! Extent of data written in ans
    REAL(DFP), INTENT(INOUT) :: tempMat2(:, :)
    !! two dimensional matrix for internal use. Its size should be
    !! enough to keep the nodal value of vector from val
    !! The first index denotes the space components.
    !! The second index denotes the space components
  END SUBROUTINE obj_GetSpatialGradient_6
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of a matrix
!
!# GetSpatialGradient
!
! This method returns the spatial gradient of matrix.
! The values of matrix field is stored inside the fevariable `val`.
! The fevariable can be constant, space, and spacetime dependent.

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient7(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space nodal values of matrix in (i,j,I) format
  END SUBROUTINE obj_GetSpatialGradient7
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of a matrix
!
!# GetSpatialGradient
!
! This method returns the spatial gradient of matrix.
!
! The values of matrix field is stored inside the `val`.
! The first and second indices of val denotes space components of
! matrix, and the third index denotes the space nodal value.
!
! The result included in ans, which is a rank 4 array.
! The first and second indices of ans denotes space components of
! matrix, the third index denotes the space components, and the
! fourth index denotes the quadrature points.

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_7( &
    obj, val, ans, dim1, dim2, dim3, dim4)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    !! space nodal values of matrix in (i,j,I) format
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient at integration points
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    !! Extent of data written in ans
  END SUBROUTINE obj_GetSpatialGradient_7
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of a matrix
!
!# GetSpatialGradient
!
! This method returns the spatial gradient of matrix.
!
! The values of matrix field is stored inside the `val`.
! The first and second indices of val denotes space components of
! matrix, and the third index denotes the space nodal value, and
! the fourth index denotes the time nodal value.
!
! The result included in ans, which is a rank 4 array.
! The first and second indices of ans denotes space components of
! matrix, the third index denotes the space components, and the
! fourth index denotes the quadrature points.

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient8(obj, ans, val)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient at integration points
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values of matrix in (i,j,I,a) format
  END SUBROUTINE obj_GetSpatialGradient8
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of a matrix
!
!# GetSpatialGradient_
!
! This method returns the spatial gradient of matrix.
!
! The values of matrix field is stored inside the `val`.
! The first and second indices of val denotes space components of
! matrix, and the third index denotes the space nodal value, and
! the fourth index denotes the time nodal value.
!
! The result included in ans, which is a rank 4 array.
! The first and second indices of ans denotes space components of
! matrix, the third index denotes the space components, and the
! fourth index denotes the quadrature points.

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_8( &
    obj, val, ans, dim1, dim2, dim3, dim4)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    !! space-time nodal values of matrix in (i,j,I,a) format
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient at integration points
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    !! Extent of data written in ans
  END SUBROUTINE obj_GetSpatialGradient_8
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of a vector

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient9(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient at integration points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space nodal values of matrix in (i,j,I) format
  END SUBROUTINE obj_GetSpatialGradient9
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of matrix
!
!# GetSpatialGradient_
!
! This subroutine returns the spatial gradient of matrix.
!
! The value of matrix is stored in val, FEVariable_.
! The FEVariable should be matrix, its varType should be constant or
! space.
!
! If FEVariable is space-time, then obj should be STElemShapeData_
!
! The result is returned inside ans, which is a rank four array.
! The first two index denotes the space components of matrix.
!

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_9( &
    obj, val, valRank, ans, dim1, dim2, dim3, dim4, tempMat3)
    TYPE(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space nodal values of matrix in (i,j,I) format
    TYPE(FEVariableMatrix_), INTENT(IN) :: valRank
    !! matrix finite element variable.
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient at integration points
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    !! Extent of data written in ans
    REAL(DFP), INTENT(INOUT) :: tempMat3(:, :, :)
    !! temporary matrix of rank 3, its size should be enough
    !! to keep the matrix nodal value from val
  END SUBROUTINE obj_GetSpatialGradient_9
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar
!
!# GetSpatialGradient
!
! This method returns the spatial gradient of scalar.
!
! The values of scalar field is stored inside the fevariable `val`.
! The fevariable can be constant, space, and spacetime dependent
!
! Note that the FEVariable should be nodalVariable, that is, it should
! be defined on the nodes and not on the quadrature.
!
! Note that the FEVariable cannot be a space-time variable in this case.
! If it is a space-time variable then obj should be a STElemShapeData_
!
! The result will be stored inside ans. The first index of ans
! denotes space component, and second index denotes spatial node.

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient10( &
    obj, val, valRank, ans)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Nodal values of scalar
    TYPE(FEVariableScalar_), INTENT(IN) :: valRank
    !! nodal value of scalar
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
    !! Spatial gradient of scalar
  END SUBROUTINE obj_GetSpatialGradient10
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of scalar
!
!# GetSpatialGradient_
!
! This method returns the spatial gradient of scalar.
!
! The values of scalar field is stored inside the fevariable `val`.
! The fevariable can be constant, space, and spacetime dependent
!
! Note that the FEVariable should be nodalVariable, that is, it should
! be defined on the nodes and not on the quadrature.
!
! Note that the FEVariable cannot be a space-time variable in this case.
! If it is a space-time variable then obj should be a STElemShapeData_
!
! The result will be stored inside ans. The first index of ans
! denotes space component, and second index denotes spatial node.

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_10( &
    obj, val, valRank, ans, nrow, ncol, tempMat2)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Nodal values of scalar
    TYPE(FEVariableScalar_), INTENT(IN) :: valRank
    !! nodal value of scalar
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Spatial gradient of scalar
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! extent of data written in ans
    REAL(DFP), INTENT(INOUT) :: tempMat2(:, :)
    !! temporary vector for internal use, its size should be
    !! atleast obj%nns, obj%nnt
  END SUBROUTINE obj_GetSpatialGradient_10
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of vector
!
!# GetSpatialGradient
!
! This method returns the spatial gradient of vector.
! The values of vector field is stored inside the fevariable `val`.
!
! The fevariable can be constant, space, and spacetime dependent.
!
! The fevariable should be a vector which is defined on nodes.
! The fevariable should not be space-time,
! it can be only constant or space dependent.
!
! If fevariable is space-time then obj should be STElemShapeData_
!
INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient11( &
    obj, val, valRank, ans)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: valRank
    !! vector finite element variable.
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient of vector at integration points
  END SUBROUTINE obj_GetSpatialGradient11
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of vector
!
!# GetSpatialGradient
!
! This method returns the spatial gradient of vector.
! The values of vector field is stored inside the fevariable `val`.
!
! The fevariable can be constant, space, and spacetime dependent.
!
! The fevariable should be a vector which is defined on nodes.
! The fevariable should not be space-time,
! it can be only constant or space dependent.
!
! If fevariable is space-time then obj should be STElemShapeData_
!
INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_11( &
    obj, val, valRank, ans, dim1, dim2, dim3, tempMat3)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    !! vector finite element variable
    TYPE(FEVariableVector_), INTENT(IN) :: valRank
    !! vector finite element variable.
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! spatial gradient of vector at integration points
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! Extent of data written in ans
    REAL(DFP), INTENT(INOUT) :: tempMat3(:, :, :)
    !! three dimensional matrix for internal use.
    !! Its size should be enough to keep the space-time nodal
    !! value of vector from val.
    !! the first index of tempMat3 denotes the space components of
    !! vector, the second index should be atleast obj%nns,
    !! the third index should be atleast obj%nnt
  END SUBROUTINE obj_GetSpatialGradient_11
END INTERFACE GetSpatialGradient_

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of matrix
!
!# GetSpatialGradient_
!
! This subroutine returns the spatial gradient of matrix.
!
! The value of matrix is stored in val, FEVariable_.
! The FEVariable should be matrix, its varType should be constant or
! space.
!
! If FEVariable is space-time, then obj should be STElemShapeData_
!
! The result is returned inside ans, which is a rank four array.
! The first two index denotes the space components of matrix.
!

INTERFACE GetSpatialGradient
  MODULE PURE SUBROUTINE obj_GetSpatialGradient12( &
    obj, val, valRank, ans)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space nodal values of matrix in (i,j,I) format
    TYPE(FEVariableMatrix_), INTENT(IN) :: valRank
    !! matrix finite element variable.
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient at integration points
  END SUBROUTINE obj_GetSpatialGradient12
END INTERFACE GetSpatialGradient

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-26
! summary: This subroutine returns the spatial gradient of matrix
!
!# GetSpatialGradient_
!
! This subroutine returns the spatial gradient of matrix.
!
! The value of matrix is stored in val, FEVariable_.
! The FEVariable should be matrix, its varType should be constant or
! space.
!
! If FEVariable is space-time, then obj should be STElemShapeData_
!
! The result is returned inside ans, which is a rank four array.
! The first two index denotes the space components of matrix.
!

INTERFACE GetSpatialGradient_
  MODULE PURE SUBROUTINE obj_GetSpatialGradient_12( &
    obj, val, valRank, ans, dim1, dim2, dim3, dim4, tempMat4)
    TYPE(STElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    !! space nodal values of matrix in (i,j,I) format
    TYPE(FEVariableMatrix_), INTENT(IN) :: valRank
    !! matrix finite element variable.
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :, :)
    !! spatial gradient at integration points
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4
    !! Extent of data written in ans
    REAL(DFP), INTENT(INOUT) :: tempMat4(:, :, :, :)
    !! temporary matrix of rank 4, its size should be enough
    !! to keep the matrix nodal value from val
    !! index 1 and index 2 should be atleast shape of matrix.
    !! index 3 and index 4 of tempMat4 should be atleast
    !! obj%nns, obj%nnt
  END SUBROUTINE obj_GetSpatialGradient_12
END INTERFACE GetSpatialGradient_

END MODULE ElemshapeData_GradientMethods
