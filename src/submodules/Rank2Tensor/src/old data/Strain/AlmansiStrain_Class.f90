!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   AlmansiStrain_Class.f90
!                   Last Update :   Dec-29-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!   TYPE :: Module
!
!   DESCRIPTION
!       -   Rank2Tensor class is extended to Define AlmansiStrain Class
!==============================================================================

 MODULE AlmansiStrain_Class
    USE GlobalData
    USE IO
    USE Strain_Class
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: AlmansiStrain_, AlmansiStrain, AlmansiStrain_Pointer
    
!------------------------------------------------------------------------------
!                                                               AlmansiStrain_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( Strain_ ) :: AlmansiStrain_
    
 END TYPE AlmansiStrain_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                                  
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    INTERFACE AlmansiStrain
        MODULE PROCEDURE Constructor1, Constructor2
    END INTERFACE

    INTERFACE AlmansiStrain_Pointer
        MODULE PROCEDURE Constructor_1, Constructor_2
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
    CLASS( AlmansiStrain_ ), POINTER :: Constructor_1
    TYPE( DeformationGradient_ ), INTENT( IN ) :: F

    ALLOCATE( Constructor_1 )
    Constructor_1 = .AlmansiStrain. F

 END FUNCTION Constructor_1

!------------------------------------------------------------------------------
!                                                               Constructor_2
!------------------------------------------------------------------------------

 FUNCTION Constructor_2( B )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1.  Constructing using Right Cauchy Green tensor                                                          
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    USE LeftCauchyGreen_Class

    ! Define intent of dummy varialbes
    CLASS( AlmansiStrain_ ), POINTER :: Constructor_2
    TYPE( LeftCauchyGreen_ ), INTENT( IN ) :: B

    ALLOCATE( Constructor_2 )
    Constructor_2 = 0.5_DFP*( Eye3 - ( .inv. B ) )

 END FUNCTION Constructor_2

!------------------------------------------------------------------------------
!                                                               Constructor1
!------------------------------------------------------------------------------

 FUNCTION Constructor1( F )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1. Constructing using deformation gradient tensor                                             
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
    
    USE DeformationGradient_Class

    ! Define intent of dummy variables
    TYPE( AlmansiStrain_ ) :: Constructor1
    TYPE( DeformationGradient_ ), INTENT( IN ) :: F

    Constructor1 = .AlmansiStrain. F

 END FUNCTION Constructor1

!------------------------------------------------------------------------------
!                                                               Constructor2
!------------------------------------------------------------------------------

 FUNCTION Constructor2( B )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1.  Constructing using Right Cauchy Green tensor                                                           
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    USE LeftCauchyGreen_Class

    ! Define intent of dummy varialbes
    TYPE( AlmansiStrain_ ) :: Constructor2
    TYPE( LeftCauchyGreen_ ), INTENT( IN ) :: B

    Constructor2 = 0.5_DFP*( Eye3 - ( .inv. B ) )

 END FUNCTION Constructor2

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

 END MODULE AlmansiStrain_Class

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

