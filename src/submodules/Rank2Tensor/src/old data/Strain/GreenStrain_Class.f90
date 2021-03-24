!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   GreenStrain_Class.f90
!                   Last Update :   Dec-29-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!   TYPE :: Module
!
!   DESCRIPTION
!       -   Rank2Tensor class is extended to Define GreenStrain Class
!==============================================================================

 MODULE GreenStrain_Class
    USE GlobalData
    USE IO
    USE Strain_Class
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: GreenStrain_, GreenStrain, GreenStrain_Pointer
    
!------------------------------------------------------------------------------
!                                                               GreenStrain_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( Strain_ ) :: GreenStrain_
    
 END TYPE GreenStrain_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                                  
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    INTERFACE GreenStrain
        MODULE PROCEDURE Constructor1, Constructor2
    END INTERFACE

    INTERFACE GreenStrain_Pointer
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
    CLASS( GreenStrain_ ), POINTER :: Constructor_1
    TYPE( DeformationGradient_ ), INTENT( IN ) :: F

    ALLOCATE( Constructor_1 )
    Constructor_1 = .GreenStrain. F

 END FUNCTION Constructor_1

!------------------------------------------------------------------------------
!                                                               Constructor_2
!------------------------------------------------------------------------------

 FUNCTION Constructor_2( C )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1.  Constructing using Right Cauchy Green tensor                                                          
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    USE RightCauchyGreen_Class

    ! Define intent of dummy varialbes
    CLASS( GreenStrain_ ), POINTER :: Constructor_2
    TYPE( RightCauchyGreen_ ), INTENT( IN ) :: C

    ALLOCATE( Constructor_2 )
    Constructor_2 = 0.5_DFP*( C - Eye3 )

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
    TYPE( GreenStrain_ ) :: Constructor1
    TYPE( DeformationGradient_ ), INTENT( IN ) :: F

    Constructor1 = .GreenStrain. F

 END FUNCTION Constructor1

!------------------------------------------------------------------------------
!                                                               Constructor2
!------------------------------------------------------------------------------

 FUNCTION Constructor2( C )

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1.  Constructing using Right Cauchy Green tensor                                                           
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    USE RightCauchyGreen_Class

    ! Define intent of dummy varialbes
    TYPE( GreenStrain_ ) :: Constructor2
    TYPE( RightCauchyGreen_ ), INTENT( IN ) :: C


    Constructor2 = 0.5_DFP*( C - Eye3 )

 END FUNCTION Constructor2

 END MODULE GreenStrain_Class

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

