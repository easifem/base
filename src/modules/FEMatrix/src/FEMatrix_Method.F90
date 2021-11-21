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

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This module contains method to construct finite element matrices

MODULE FEMatrix_Method
USE BaseType
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space domain
!
!# Introduction
!
! This subroutine makes space matrix in space domain, Here Rho $\rho$ is a
! finite element variable
!
! $$\int_{\Omega } N^{I}\rho N^{J}d\Omega$$
!

INTERFACE
MODULE PURE FUNCTION Space_MassMatrix( Test, Trial, Rho, nCopy ) RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Test
    !! Shapedata for test function
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Trial
    !! Shapedata for trial function
  CLASS( FEVariable_ ), INTENT( IN ), OPTIONAL :: Rho
    !! Finite element variable (density)
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: nCopy
    !! number of diagonal copies
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION Space_MassMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                              MassMatrix@MassMatrixMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine makes mass matrix in space time domain
!
!# Introduction
!
! This subroutine makes space matrix in space domain, Here Rho $\rho$ is a
!  finite element variable. Following expression can be evaluated
!
! $$\int_{\Omega } N^{I}T_{a}\rho N^{J}T_{b}d\Omega$$
! $$\int \frac{\partial N^{I}T_{a}}{\partial t} \rho N^{J}T_{b}d\Omega dt$$
! $$\int \frac{\partial N^{I}T_{a}}{\partial t} \rho
! \frac{\partial N^{J}T_{b}}{\partial t} d\Omega dt$$
! $$\int N^{I}T_{a}\rho \frac{\partial N^{J}T_{b}}{\partial t} d\Omega dt$$
!

INTERFACE
MODULE PURE FUNCTION st_massMatrix_a( Test, Trial, Rho, Term1, Term2, nCopy )&
  & RESULT( Ans )
  CLASS( STElemshapeData_ ), INTENT( IN ) :: Test(:)
  CLASS( STElemshapeData_ ), INTENT( IN ) :: Trial(:)
  TYPE( FEVariable_ ), OPTIONAL, INTENT( IN ) :: Rho
  INTEGER( I4B ), INTENT( IN ) :: Term1
    !! If 0 then time derivative in first term is absent
    !! If 1 then first order time derivative is present in first term
  INTEGER( I4B ), INTENT( IN ) :: Term2
    !! If 0 then time derivative in second term absent
    !! If 1 then first order time derivative is present in the second term
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nCopy
    !! number of diagonal copies
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
    !! returned finite element matrix.
END FUNCTION st_massMatrix_a
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 MassMatrix
!----------------------------------------------------------------------------

INTERFACE MassMatrix
  MODULE PROCEDURE Space_MassMatrix, st_massMatrix_a
END INTERFACE MassMatrix

PUBLIC :: MassMatrix

!----------------------------------------------------------------------------
!                                            DiffusionMatrix@DiffusionMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine returns the diffusion matrix in space domain
!
!# Introduction
!
! This function returns the diffusion matrix in space domain
!
! $$\int^{}_{\Omega } \frac{\partial N^{I}}{\partial x_{i}} \frac{\partial N^
! {J}}{\partial x_{i}} d\Omega$$

INTERFACE
MODULE PURE FUNCTION Space_DiffusionMatrix( Test, Trial, nCopy ) RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN )  :: Test, Trial
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: nCopy
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION Space_DiffusionMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                            DiffusionMatrix@DiffusionMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine returns the diffusion matrix in space domain
!
!# Introduction
!
! This function returns the diffusion matrix in space domain
!
! $$\int^{}_{\Omega } \frac{\partial N^{I}}{\partial x_{i}} k_{ij}
!\frac{\partial N^{J}}{\partial x_{j}} d\Omega$$

INTERFACE
MODULE PURE FUNCTION Space_DiffusionMatrix_K( Test, Trial, K, nCopy ) &
  & RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN )  :: Test, Trial
  CLASS( FEVariable_ ), INTENT( IN ) :: K
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: nCopy
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION Space_DiffusionMatrix_K
END INTERFACE

!----------------------------------------------------------------------------
!                                            DiffusionMatrix@DiffusionMatrix
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: This subroutine returns the diffusion matrix in space domain
!
!# Introduction
!
! This function returns the diffusion matrix in space domain
!
! $$\int^{}_{\Omega } \frac{\partial N^{I}}{\partial x_{i}} c_i
!\frac{\partial N^{J}}{\partial x_{j}} c_j d\Omega$$
!

INTERFACE
MODULE PURE FUNCTION Space_DiffusionMatrix_C( Test, Trial, C1, C2, nCopy ) &
  & RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN )  :: Test, Trial
  CLASS( FEVariable_ ), INTENT( IN ) :: C1, C2
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: nCopy
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION Space_DiffusionMatrix_C
END INTERFACE

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION st_diffusionMatrix( Test, Trial, nCopy ) RESULT( Ans )
  CLASS( STElemshapeData_ ), INTENT( IN )  :: Test( : )
  CLASS( STElemshapeData_ ), INTENT( IN )  :: Trial( : )
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: nCopy
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION st_diffusionMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION st_diffusionMatrix_K( Test, Trial, K, nCopy ) &
  & RESULT( Ans )
  CLASS( STElemshapeData_ ), INTENT( IN )  :: Test(:)
  CLASS( STElemshapeData_ ), INTENT( IN )  :: Trial(:)
  CLASS( FEVariable_ ), INTENT( IN ) :: K
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: nCopy
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION st_diffusionMatrix_K
END INTERFACE

!----------------------------------------------------------------------------
!                                                           DiffusionMatrix
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION st_diffusionMatrix_C( Test, Trial, C1, C2, nCopy ) &
  & RESULT( Ans )
  CLASS( STElemshapeData_ ), INTENT( IN )  :: Test( : )
  CLASS( STElemshapeData_ ), INTENT( IN )  :: Trial( : )
  CLASS( FEVariable_ ), INTENT( IN ) :: C1
  CLASS( FEVariable_ ), INTENT( IN ) :: C2
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: nCopy
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION st_diffusionMatrix_C
END INTERFACE

INTERFACE DiffusionMatrix
  MODULE PROCEDURE &
    & Space_DiffusionMatrix, &
    & Space_DiffusionMatrix_K,&
    & Space_DiffusionMatrix_C, &
    & st_DiffusionMatrix, &
    & st_DiffusionMatrix_K,&
    & st_DiffusionMatrix_C
END INTERFACE DiffusionMatrix

PUBLIC :: DiffusionMatrix

!----------------------------------------------------------------------------
!                                            StiffnessMatrix@StiffnessMatrix
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION femat_StiffnessMatrix1( Test, Trial, Cijkl ) &
  & RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Test, Trial
  CLASS( FEVariable_ ), INTENT( IN ) :: Cijkl
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION femat_StiffnessMatrix1
END INTERFACE

!----------------------------------------------------------------------------
!                                            StiffnessMatrix@StiffnessMatrix
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION femat_StiffnessMatrix2( Test, Trial, Lambda, Mu ) &
  & RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Test, Trial
  CLASS( FEVariable_ ), INTENT( IN ) :: Lambda, Mu
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION femat_StiffnessMatrix2
END INTERFACE

!----------------------------------------------------------------------------
!                                            StiffnessMatrix@StiffnessMatrix
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION femat_StiffnessMatrix3( Test, Trial, Lambda,  &
  & Mu) RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Test, Trial
  REAL( DFP ), INTENT( IN ) :: Lambda, Mu
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION femat_StiffnessMatrix3
END INTERFACE

!----------------------------------------------------------------------------
!                                            StiffnessMatrix@StiffnessMatrix
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION femat_StiffnessMatrix4( Test, Trial, Cijkl ) &
  & RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Test, Trial
  REAL( DFP ), INTENT( IN ) :: Cijkl( :, : )
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION femat_StiffnessMatrix4
END INTERFACE

INTERFACE StiffnessMatrix
  MODULE PROCEDURE femat_StiffnessMatrix1, femat_StiffnessMatrix2, &
    & femat_StiffnessMatrix3, femat_StiffnessMatrix4
END INTERFACE StiffnessMatrix

PUBLIC :: StiffnessMatrix

!----------------------------------------------------------------------------
!                                                NitscheMatrix@NitscheMatrix
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION space_nitsche_mat_1( Test, Trial, Lambda, Mu, Evec ) &
  & RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Test, Trial
  CLASS( FEVariable_ ), INTENT( IN ) :: Lambda, Mu, Evec
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION space_nitsche_mat_1
END INTERFACE

INTERFACE
MODULE PURE FUNCTION space_nitsche_mat_3( Test, Trial, Lambda, Mu, Evec ) &
  & RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Test, Trial
  CLASS( FEVariable_ ), INTENT( IN ) :: Evec
  REAL( DFP ), INTENT( IN ) :: Lambda, Mu
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION space_nitsche_mat_3
END INTERFACE

INTERFACE
MODULE PURE FUNCTION space_nitsche_mat_5( Test, Trial, Lambda, Mu, isNoSlip)&
  & RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Test, Trial
  REAL( DFP ), INTENT( IN ) :: Lambda, Mu
  LOGICAL( LGT ), INTENT( IN ) :: isNoSlip
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION space_nitsche_mat_5
END INTERFACE

INTERFACE
MODULE PURE FUNCTION space_nitsche_mat_7( Test, Trial, Lambda, Mu, isNoSlip )&
  & RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Test, Trial
  CLASS( FEVariable_ ), INTENT( IN ) :: Lambda, Mu
  LOGICAL( LGT ), INTENT( IN ) :: isNoSlip
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION space_nitsche_mat_7
END INTERFACE

INTERFACE
MODULE PURE FUNCTION space_nitsche_mat_2( Test, Trial, Alpha, Evec ) &
  & RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Test, Trial
  CLASS( FEVariable_ ), INTENT( IN ) :: Alpha, Evec
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION space_nitsche_mat_2
END INTERFACE

INTERFACE
MODULE PURE FUNCTION space_nitsche_mat_4( Test, Trial, Alpha, Evec ) &
  & RESULT( Ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: Test, Trial
  CLASS( FEVariable_ ), INTENT( IN ) :: Evec
  REAL( DFP ), INTENT( IN ) :: Alpha
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION space_nitsche_mat_4
END INTERFACE

INTERFACE NitscheMatrix
  MODULE PROCEDURE space_nitsche_mat_1, space_nitsche_mat_2, &
    & space_nitsche_mat_3, space_nitsche_mat_4, space_nitsche_mat_5, &
    & space_nitsche_mat_7
END INTERFACE NitscheMatrix

PUBLIC :: NitscheMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEMatrix_Method
