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

SUBMODULE(FEVariable_Method) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_deallocate
  IF( ALLOCATED( obj%R1 ) ) DEALLOCATE( obj%R1 )
  IF( ALLOCATED( obj%R2 ) ) DEALLOCATE( obj%R2 )
  IF( ALLOCATED( obj%R3 ) ) DEALLOCATE( obj%R3 )
  IF( ALLOCATED( obj%R4 ) ) DEALLOCATE( obj%R4 )
  obj%R0 = 0.0_DFP
  obj%DefineOn = 0
  obj%VarType = 0
  obj%Rank = 0
  obj%CaseType = 0
END PROCEDURE fe_deallocate
!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Constant
  obj%R0 = Val
  obj%DefineOn = Nodal
  obj%Rank = Scalar
  obj%VarType = Constant
  obj%CaseType = 1
END PROCEDURE Nodal_Scalar_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Space
  obj%R1 = Val
  obj%DefineOn = Nodal
  obj%Rank = Scalar
  obj%VarType = Space
  obj%CaseType = 2
END PROCEDURE Nodal_Scalar_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Spacetime
  obj%R2 = Val
  obj%DefineOn = Nodal
  obj%Rank = Scalar
  obj%VarType = Spacetime
  obj%CaseType = 3
END PROCEDURE Nodal_Scalar_Spacetime

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Constant
  obj%R1 = Val
  obj%DefineOn = Nodal
  obj%Rank = Vector
  obj%VarType = Constant
  obj%CaseType = 4
END PROCEDURE Nodal_Vector_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Space
  obj%R2 = Val
  obj%DefineOn = Nodal
  obj%Rank = Vector
  obj%VarType = Space
  obj%CaseType = 5
END PROCEDURE Nodal_Vector_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Spacetime
  obj%R3 = Val
  obj%DefineOn = Nodal
  obj%Rank = Vector
  obj%VarType = Spacetime
  obj%CaseType = 6
END PROCEDURE Nodal_Vector_Spacetime

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Constant
  obj%R2 = Val
  obj%DefineOn = Nodal
  obj%Rank = Matrix
  obj%VarType = Constant
  obj%CaseType = 7
END PROCEDURE Nodal_Matrix_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Space
  obj%R3 = Val
  obj%DefineOn = Nodal
  obj%Rank = Matrix
  obj%VarType = Space
  obj%CaseType = 8
END PROCEDURE Nodal_Matrix_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Spacetime
  obj%R4 = Val
  obj%DefineOn = Nodal
  obj%Rank = Matrix
  obj%VarType = Spacetime
  obj%CaseType = 9
END PROCEDURE Nodal_Matrix_Spacetime

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Constant
  obj%R0 = Val
  obj%DefineOn = Quadrature
  obj%Rank = Scalar
  obj%VarType = Constant
  obj%CaseType = 10
END PROCEDURE Quadrature_Scalar_Constant

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Space
  obj%R1 = Val
  obj%DefineOn = Quadrature
  obj%Rank = Scalar
  obj%VarType = Space
  obj%CaseType = 11
END PROCEDURE Quadrature_Scalar_Space

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Spacetime
  obj%R2 = Val
  obj%DefineOn = Quadrature
  obj%Rank = Scalar
  obj%VarType = Spacetime
  obj%CaseType = 12
END PROCEDURE Quadrature_Scalar_Spacetime

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Constant
  obj%R1 = Val
  obj%DefineOn = Quadrature
  obj%Rank = Vector
  obj%VarType = Constant
  obj%CaseType = 13
END PROCEDURE Quadrature_Vector_Constant

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Space
  obj%R2 = Val
  obj%DefineOn = Quadrature
  obj%Rank = Vector
  obj%VarType = Space
  obj%CaseType = 14
END PROCEDURE Quadrature_Vector_Space

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Spacetime
  obj%R3 = Val
  obj%DefineOn = Quadrature
  obj%Rank = Vector
  obj%VarType = Spacetime
  obj%CaseType = 15
END PROCEDURE Quadrature_Vector_Spacetime

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Constant
  obj%R2 = Val
  obj%DefineOn = Quadrature
  obj%Rank = Matrix
  obj%VarType = Constant
  obj%CaseType = 16
END PROCEDURE Quadrature_Matrix_Constant

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Space
  obj%R3 = Val
  obj%DefineOn = Quadrature
  obj%Rank = Matrix
  obj%VarType = Space
  obj%CaseType = 17
END PROCEDURE Quadrature_Matrix_Space

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Spacetime
  obj%R4 = Val
  obj%DefineOn = Quadrature
  obj%Rank = Matrix
  obj%VarType = Spacetime
  obj%CaseType = 18
END PROCEDURE Quadrature_Matrix_Spacetime

END SUBMODULE Constructor