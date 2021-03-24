!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   DeformationGradient_Class.f90
!                   Last Update :   Dec-17-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!   TYPE :: Module
!
!   DESCRIPTION
!       -   Rank2Tensor class is extended to deformation gradient
!==============================================================================

 MODULE DeformationGradient_Class
    USE GlobalData
    USE IO
    USE Rank2Tensor_Class
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: DeformationGradient_, DeformationGradient_Pointer, &
              DeformationGradient

!------------------------------------------------------------------------------
!                                                         DeformationGradient_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( Rank2Tensor_ ) :: DeformationGradient_

!   DESCRIPTION
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!       1.  -  DeformationGradient
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ) :: R
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ) :: U
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ) :: V
    REAL( DFP ), ALLOCATABLE, DIMENSION( : ) :: EigenVal
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ) :: EigenVec_U
    REAL( DFP ), ALLOCATABLE, DIMENSION( :, : ) :: EigenVec_V

    CONTAINS

        ! Constructor.part

            PROCEDURE, PUBLIC, PASS :: Initiate2, Initiate5
            PROCEDURE, PUBLIC, PASS :: DeallocateData

        ! Component.part

            PROCEDURE, PUBLIC, PASS :: m_RightStretch
            PROCEDURE, PUBLIC, PASS :: m_Rotation
            PROCEDURE, PUBLIC, PASS :: m_LeftStretch
            PROCEDURE, PUBLIC, PASS :: m_EigenValues
            PROCEDURE, PUBLIC, PASS :: m_EigenVectors_U
            PROCEDURE, PUBLIC, PASS :: m_EigenVectors_V
            PROCEDURE, PUBLIC, PASS :: m_Jacobian

            GENERIC, PUBLIC :: OPERATOR( .R. ) => m_Rotation
            GENERIC, PUBLIC :: OPERATOR( .U. ) => m_RightStretch
            GENERIC, PUBLIC :: OPERATOR( .V. ) => m_LeftStretch
            GENERIC, PUBLIC :: OPERATOR( .EigenValues. ) => m_EigenValues
            GENERIC, PUBLIC :: OPERATOR( .EigenVectorsU. ) => m_EigenVectors_U
            GENERIC, PUBLIC :: OPERATOR( .EigenVectorsV. ) => m_EigenVectors_V
            GENERIC, PUBLIC :: OPERATOR( .J. ) => m_Jacobian

        ! DeformationTensor.part

            PROCEDURE, PUBLIC, PASS :: m_RightCauchyGreen
            GENERIC, PUBLIC :: RightCauchyGreen => m_RightCauchyGreen
            GENERIC, PUBLIC :: OPERATOR( .C. ) => m_RightCauchyGreen

            PROCEDURE, PUBLIC, PASS :: m_LeftCauchyGreen
            GENERIC, PUBLIC :: LeftCauchyGreen => m_LeftCauchyGreen
            GENERIC, PUBLIC :: OPERATOR( .B. ) => m_LeftCauchyGreen

        ! StrainTensor.part

            PROCEDURE, PUBLIC, PASS :: m_GreenStrain
            GENERIC, PUBLIC :: OPERATOR( .GreenStrain. ) => m_GreenStrain
            GENERIC, PUBLIC :: GreenStrain => m_GreenStrain


            PROCEDURE, PUBLIC, PASS :: m_AlmansiStrain
            GENERIC, PUBLIC :: OPERATOR( .AlmansiStrain. ) => m_AlmansiStrain
            GENERIC, PUBLIC :: AlmansiStrain => m_AlmansiStrain

        ! Display.part

        PROCEDURE, PUBLIC, PASS :: Display

 END TYPE DeformationGradient_


!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                DeformationGradient_Pointer
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


    INTERFACE DeformationGradient_Pointer
        MODULE PROCEDURE Constructor1, Constructor2, Constructor3
    END INTERFACE

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                        DeformationGradient
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


    INTERFACE DeformationGradient
        MODULE PROCEDURE Constructor4, Constructor5, Constructor6
    END INTERFACE

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                            OPERATOR( .C. )
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


    INTERFACE OPERATOR( .C. )
        MODULE PROCEDURE m_RightCauchyGreen, f_RightCauchyGreen
    END INTERFACE

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                           RightCauchyGreen
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


    INTERFACE RightCauchyGreen
        MODULE PROCEDURE m_RightCauchyGreen, f_RightCauchyGreen
    END INTERFACE

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                            OPERATOR( .B. )
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


    INTERFACE OPERATOR( .B. )
        MODULE PROCEDURE m_LeftCauchyGreen, f_LeftCauchyGreen
    END INTERFACE

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                            LeftCauchyGreen
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


    INTERFACE LeftCauchyGreen
        MODULE PROCEDURE m_LeftCauchyGreen, f_LeftCauchyGreen
    END INTERFACE


!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                                GreenTensor
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


    INTERFACE GreenTensor
        MODULE PROCEDURE m_GreenStrain, f_GreenStrain
    END INTERFACE


!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                                GreenTensor
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


    INTERFACE OPERATOR( .GreenTensor. )

        MODULE PROCEDURE m_GreenStrain, f_GreenStrain

    END INTERFACE


!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                               AlmansiTensor
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


    INTERFACE AlmansiTensor

        MODULE PROCEDURE m_AlmansiStrain, f_AlmansiStrain

    END INTERFACE


!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                               AlmansiTensor
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


    INTERFACE OPERATOR( .AlmansiTensor. )

        MODULE PROCEDURE m_AlmansiStrain, f_AlmansiStrain

    END INTERFACE

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


 CONTAINS

#include "./Constructor.part"
#include "./Display.part"
#include "./Components.part"
#include "./StrainTensor.part"
#include "./DeformationTensor.part"

 END MODULE DeformationGradient_Class

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

