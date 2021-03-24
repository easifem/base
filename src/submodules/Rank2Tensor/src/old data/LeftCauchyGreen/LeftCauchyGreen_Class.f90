!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   LeftCauchyGreen_Class.f90
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

 MODULE LeftCauchyGreen_Class
    USE GlobalData
    USE IO
    USE Rank2Tensor_Class
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: LeftCauchyGreen_, LeftCauchyGreen, LeftCauchyGreen_Pointer
    
!------------------------------------------------------------------------------
!                                                            LeftCauchyGreen_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( Rank2Tensor_ ) :: LeftCauchyGreen_

 END TYPE LeftCauchyGreen_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                                  
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    INTERFACE LeftCauchyGreen
        MODULE PROCEDURE Constructor1
    END INTERFACE

    INTERFACE LeftCauchyGreen_Pointer
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
    CLASS( LeftCauchyGreen_ ), POINTER :: Constructor_1
    TYPE( DeformationGradient_ ), INTENT( IN ) :: F

    ALLOCATE( Constructor_1 )
    Constructor_1 =  F .matmul. (.transpose. F)

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
    TYPE( LeftCauchyGreen_ ) :: Constructor1
    TYPE( DeformationGradient_ ), INTENT( IN ) :: F

    Constructor1 =  F .matmul. (.transpose. F)

 END FUNCTION Constructor1

 END MODULE LeftCauchyGreen_Class

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

