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

SUBMODULE(FEVariable_Method) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Deallocate
IF (ALLOCATED(obj%val)) DEALLOCATE (obj%val)
obj%s = 0
obj%defineOn = 0
obj%varType = 0
obj%rank = 0
obj%len = 0
obj%capacity = 0
END PROCEDURE fevar_Deallocate

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Constant
obj%val = [val]
obj%s(1) = 1
obj%defineOn = NODAL
obj%rank = SCALAR
obj%varType = CONSTANT
obj%len = 1
obj%capacity = 1
END PROCEDURE Nodal_Scalar_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Space
obj%val = val
obj%s(1) = SIZE(val)
obj%defineOn = NODAL
obj%rank = SCALAR
obj%varType = SPACE
obj%len = SIZE(val)
obj%capacity = obj%len
END PROCEDURE Nodal_Scalar_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Time
obj%val = val
obj%s(1) = SIZE(val)
obj%defineOn = NODAL
obj%rank = SCALAR
obj%varType = TIME
obj%len = SIZE(val)
obj%capacity = obj%len
END PROCEDURE Nodal_Scalar_Time

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Spacetime
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:2) = SHAPE(val)
obj%defineOn = NODAL
obj%rank = SCALAR
obj%varType = SPACETIME
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Nodal_Scalar_Spacetime

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Constant
obj%val = val
obj%s(1:1) = SHAPE(val)
obj%defineOn = NODAL
obj%rank = VECTOR
obj%varType = CONSTANT
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Nodal_Vector_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Space
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:2) = SHAPE(val)
obj%defineOn = NODAL
obj%rank = VECTOR
obj%varType = SPACE
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Nodal_Vector_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Time
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:2) = SHAPE(val)
obj%defineOn = NODAL
obj%rank = VECTOR
obj%varType = TIME
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Nodal_Vector_Time

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Spacetime
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:3) = SHAPE(val)
obj%defineOn = NODAL
obj%rank = VECTOR
obj%varType = SPACETIME
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Nodal_Vector_Spacetime

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Constant
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:2) = SHAPE(val)
obj%defineOn = NODAL
obj%rank = MATRIX
obj%varType = CONSTANT
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Nodal_Matrix_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Space
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:3) = SHAPE(val)
obj%defineOn = NODAL
obj%rank = MATRIX
obj%varType = SPACE
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Nodal_Matrix_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Time
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:3) = SHAPE(val)
obj%defineOn = NODAL
obj%rank = MATRIX
obj%varType = TIME
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Nodal_Matrix_Time

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Spacetime
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:4) = SHAPE(val)
obj%defineOn = NODAL
obj%rank = MATRIX
obj%varType = SPACETIME
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Nodal_Matrix_Spacetime

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Constant
obj%val = [val]
obj%s = 0
obj%defineOn = Quadrature
obj%rank = SCALAR
obj%varType = CONSTANT
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Quadrature_Scalar_Constant

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Space
obj%val = val
obj%s(1) = SIZE(val)
obj%defineOn = Quadrature
obj%rank = SCALAR
obj%varType = SPACE
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Quadrature_Scalar_Space

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Time
obj%val = val
obj%s(1) = SIZE(val)
obj%defineOn = Quadrature
obj%rank = SCALAR
obj%varType = TIME
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Quadrature_Scalar_Time

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Spacetime
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:2) = SHAPE(val)
obj%defineOn = Quadrature
obj%rank = SCALAR
obj%varType = SPACETIME
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Quadrature_Scalar_Spacetime

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Constant
obj%val = val
obj%s(1:1) = SHAPE(val)
obj%defineOn = Quadrature
obj%rank = VECTOR
obj%varType = CONSTANT
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Quadrature_Vector_Constant

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Space
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:2) = SHAPE(val)
obj%defineOn = Quadrature
obj%rank = VECTOR
obj%varType = SPACE
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Quadrature_Vector_Space

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Time
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:2) = SHAPE(val)
obj%defineOn = Quadrature
obj%rank = VECTOR
obj%varType = TIME
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Quadrature_Vector_Time

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Spacetime
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:3) = SHAPE(val)
obj%defineOn = Quadrature
obj%rank = VECTOR
obj%varType = SPACETIME
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Quadrature_Vector_Spacetime

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Constant
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:2) = SHAPE(val)
obj%defineOn = Quadrature
obj%rank = MATRIX
obj%varType = CONSTANT
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Quadrature_Matrix_Constant

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Space
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:3) = SHAPE(val)
obj%defineOn = Quadrature
obj%rank = MATRIX
obj%varType = SPACE
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Quadrature_Matrix_Space

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Time
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:3) = SHAPE(val)
obj%defineOn = Quadrature
obj%rank = MATRIX
obj%varType = TIME
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Quadrature_Matrix_Time

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Spacetime
obj%val = RESHAPE(val, [SIZE(val)])
obj%s(1:4) = SHAPE(val)
obj%defineOn = Quadrature
obj%rank = MATRIX
obj%varType = SPACETIME
obj%len = SIZE(obj%val)
obj%capacity = obj%len
END PROCEDURE Quadrature_Matrix_Spacetime

END SUBMODULE ConstructorMethods
