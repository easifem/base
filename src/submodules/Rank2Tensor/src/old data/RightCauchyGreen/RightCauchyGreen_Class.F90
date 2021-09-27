!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   RightCauchyGreen_Class.F90
!                   Last Update :   Dec-29-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!   TYPE :: Module
!
!   DESCRIPTION
!       -   Rank2Tensor class is extended to Left Cauchy green tensor
!==============================================================================

 MODULE RightCauchyGreen_Class
    USE GlobalData
    USE IO
    USE Rank2Tensor_Class
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: RightCauchyGreen_, RightCauchyGreen, RightCauchyGreen_Pointer

!------------------------------------------------------------------------------
!                                                            RightCauchyGreen_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( Rank2Tensor_ ) :: RightCauchyGreen_

 END TYPE RightCauchyGreen_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    INTERFACE RightCauchyGreen
        MODULE PROCEDURE Constructor1
    END INTERFACE

    INTERFACE RightCauchyGreen_Pointer
        MODULE PROCEDURE Constructor_1
    END INTERFACE

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

 CONTAINS

!------------------------------------------------------------------------------
!                                                               Constructor_1
!------------------------------------------------------------------------------

 FUNCTION Constructor_1( F )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1.  Constructing using deformation gradient tensor
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    USE DeformationGradient_Class

    ! Define intent of dummy varialbes
    CLASS( RightCauchyGreen_ ), POINTER :: Constructor_1
    TYPE( DeformationGradient_ ), INTENT( IN ) :: F

    ALLOCATE( Constructor_1 )
    Constructor_1 = ( .transpose. F ) .matmul. F

 END FUNCTION Constructor_1

!------------------------------------------------------------------------------
!                                                               Constructor1
!------------------------------------------------------------------------------

 FUNCTION Constructor1( F )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1. Constructing using deformation gradient tensor
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    USE DeformationGradient_Class

    ! Define intent of dummy variables
    TYPE( RightCauchyGreen_ ) :: Constructor1
    TYPE( DeformationGradient_ ), INTENT( IN ) :: F

    Constructor1 = ( .transpose. F ) .matmul. F

 END FUNCTION Constructor1

 END MODULE RightCauchyGreen_Class

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

