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

SUBMODULE( FEVariable_Method ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_deallocate
  IF( ALLOCATED( Obj%R1 ) ) DEALLOCATE( Obj%R1 )
  IF( ALLOCATED( Obj%R2 ) ) DEALLOCATE( Obj%R2 )
  IF( ALLOCATED( Obj%R3 ) ) DEALLOCATE( Obj%R3 )
  IF( ALLOCATED( Obj%R4 ) ) DEALLOCATE( Obj%R4 )
  Obj%R0 = 0.0_DFP
  Obj%DefineOn = 0
  Obj%VarType = 0
  Obj%Rank = 0
  Obj%CaseType = 0
END PROCEDURE fe_deallocate
!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Constant
  Obj%R0 = Val
  Obj%DefineOn = Nodal
  Obj%Rank = Scalar
  Obj%VarType = Constant
  Obj%CaseType = 1
END PROCEDURE Nodal_Scalar_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Space
  Obj%R1 = Val
  Obj%DefineOn = Nodal
  Obj%Rank = Scalar
  Obj%VarType = Space
  Obj%CaseType = 2
END PROCEDURE Nodal_Scalar_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Spacetime
  Obj%R2 = Val
  Obj%DefineOn = Nodal
  Obj%Rank = Scalar
  Obj%VarType = Spacetime
  Obj%CaseType = 3
END PROCEDURE Nodal_Scalar_Spacetime

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Constant
  Obj%R1 = Val
  Obj%DefineOn = Nodal
  Obj%Rank = Vector
  Obj%VarType = Constant
  Obj%CaseType = 4
END PROCEDURE Nodal_Vector_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Space
  Obj%R2 = Val
  Obj%DefineOn = Nodal
  Obj%Rank = Vector
  Obj%VarType = Space
  Obj%CaseType = 5
END PROCEDURE Nodal_Vector_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Spacetime
  Obj%R3 = Val
  Obj%DefineOn = Nodal
  Obj%Rank = Vector
  Obj%VarType = Spacetime
  Obj%CaseType = 6
END PROCEDURE Nodal_Vector_Spacetime

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Constant
  Obj%R2 = Val
  Obj%DefineOn = Nodal
  Obj%Rank = Matrix
  Obj%VarType = Constant
  Obj%CaseType = 7
END PROCEDURE Nodal_Matrix_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Space
  Obj%R3 = Val
  Obj%DefineOn = Nodal
  Obj%Rank = Matrix
  Obj%VarType = Space
  Obj%CaseType = 8
END PROCEDURE Nodal_Matrix_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Spacetime
  Obj%R4 = Val
  Obj%DefineOn = Nodal
  Obj%Rank = Matrix
  Obj%VarType = Spacetime
  Obj%CaseType = 9
END PROCEDURE Nodal_Matrix_Spacetime

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Constant
  Obj%R0 = Val
  Obj%DefineOn = Quadrature
  Obj%Rank = Scalar
  Obj%VarType = Constant
  Obj%CaseType = 10
END PROCEDURE Quadrature_Scalar_Constant

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Space
  Obj%R1 = Val
  Obj%DefineOn = Quadrature
  Obj%Rank = Scalar
  Obj%VarType = Space
  Obj%CaseType = 11
END PROCEDURE Quadrature_Scalar_Space

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Spacetime
  Obj%R2 = Val
  Obj%DefineOn = Quadrature
  Obj%Rank = Scalar
  Obj%VarType = Spacetime
  Obj%CaseType = 12
END PROCEDURE Quadrature_Scalar_Spacetime

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Constant
  Obj%R1 = Val
  Obj%DefineOn = Quadrature
  Obj%Rank = Vector
  Obj%VarType = Constant
  Obj%CaseType = 13
END PROCEDURE Quadrature_Vector_Constant

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Space
  Obj%R2 = Val
  Obj%DefineOn = Quadrature
  Obj%Rank = Vector
  Obj%VarType = Space
  Obj%CaseType = 14
END PROCEDURE Quadrature_Vector_Space

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Spacetime
  Obj%R3 = Val
  Obj%DefineOn = Quadrature
  Obj%Rank = Vector
  Obj%VarType = Spacetime
  Obj%CaseType = 15
END PROCEDURE Quadrature_Vector_Spacetime

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Constant
  Obj%R2 = Val
  Obj%DefineOn = Quadrature
  Obj%Rank = Matrix
  Obj%VarType = Constant
  Obj%CaseType = 16
END PROCEDURE Quadrature_Matrix_Constant

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Space
  Obj%R3 = Val
  Obj%DefineOn = Quadrature
  Obj%Rank = Matrix
  Obj%VarType = Space
  Obj%CaseType = 17
END PROCEDURE Quadrature_Matrix_Space

!----------------------------------------------------------------------------
!                                                        QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Spacetime
  Obj%R4 = Val
  Obj%DefineOn = Quadrature
  Obj%Rank = Matrix
  Obj%VarType = Spacetime
  Obj%CaseType = 18
END PROCEDURE Quadrature_Matrix_Spacetime

END SUBMODULE Constructor