!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   Stress_Class.f90
!                   Last Update :   Dec-17-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!   TYPE :: Module
!
!   DESCRIPTION
!       -   Stress_ Class is defined
!==============================================================================

 MODULE Stress_Class
    USE GlobalData
    USE IO
    USE Voigt
    IMPLICIT NONE

    PRIVATE

    PUBLIC :: Stress_, Stress, Stress_Pointer


!------------------------------------------------------------------------------
!                                                                      Stress_
!------------------------------------------------------------------------------

 TYPE :: Stress_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!       1.  -  Stress class is defined
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    REAL( DFP ), ALLOCATABLE :: V( : )
    INTEGER( I4B ) :: NSD
    CHARACTER( LEN = 50 ) :: StressType = ""

    CONTAINS


        ! Constructor.part

        PROCEDURE, PUBLIC, PASS :: Initiate1, Initiate2, Initiate3, &
        Initiate4, Initiate5, Initiate6

        GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2, Initiate3, &
        Initiate4, Initiate5, Initiate6

        GENERIC, PUBLIC :: ASSIGNMENT( = ) => Initiate3

        PROCEDURE, PUBLIC, PASS :: isInitiated
        PROCEDURE, PUBLIC, PASS :: DeallocateData
        PROCEDURE, PUBLIC, PASS :: getVoigtLen
        GENERIC, PUBLIC :: OPERATOR( .SIZE. ) => getVoigtLen
        PROCEDURE, PUBLIC, PASS :: getNSD, setNSD


        ! StressType.part

        PROCEDURE, PUBLIC, PASS( obj ) :: getStressType, setStressType
        GENERIC, PUBLIC :: OPERATOR( .StressType. ) => getStressType
        GENERIC, PUBLIC :: ASSIGNMENT( = ) => setStressType


        ! getStress.part

        PROCEDURE, PUBLIC, PASS :: s_getStress_1
        PROCEDURE, PUBLIC, PASS :: s_getStress_2
        PROCEDURE, PUBLIC, PASS :: s_getStress_5
        GENERIC, PUBLIC :: getStress => s_getStress_1, s_getStress_2, &
        s_getStress_5

        PROCEDURE, PUBLIC, PASS( obj ) :: s_getStress_3, s_getStress_4, &
        s_getStress_6
        GENERIC, PUBLIC :: ASSIGNMENT( = ) => s_getStress_3, s_getStress_4, &
        s_getStress_6

        ! OperatorOverloading/Addition.part

        PROCEDURE, PUBLIC, PASS :: obj_Add_obj
        PROCEDURE, PUBLIC, PASS :: obj_Add_Mat
        PROCEDURE, PUBLIC, PASS :: obj_Add_Vec
        PROCEDURE, PUBLIC, PASS :: obj_Add_Scalar
        PROCEDURE, PUBLIC, PASS( obj ) :: Mat_Add_obj
        PROCEDURE, PUBLIC, PASS( obj ) :: Vec_Add_obj
        PROCEDURE, PUBLIC, PASS( obj ) :: Scalar_Add_obj

        GENERIC, PUBLIC :: OPERATOR( + ) => obj_Add_Mat, obj_Add_obj, &
        Mat_Add_obj, obj_Add_Vec, Vec_Add_obj, Scalar_Add_obj, &
        obj_Add_Scalar

        PROCEDURE, PUBLIC, PASS :: obj_Minus_obj
        PROCEDURE, PUBLIC, PASS :: obj_Minus_Mat
        PROCEDURE, PUBLIC, PASS :: obj_Minus_Vec
        PROCEDURE, PUBLIC, PASS :: obj_Minus_Scalar
        PROCEDURE, PUBLIC, PASS( obj ) :: Mat_Minus_obj
        PROCEDURE, PUBLIC, PASS( obj ) :: Vec_Minus_obj
        PROCEDURE, PUBLIC, PASS( obj ) :: Scalar_Minus_obj

        GENERIC, PUBLIC :: OPERATOR( - ) => obj_Minus_Mat, obj_Minus_obj, &
        Mat_Minus_obj, obj_Minus_Vec, Vec_Minus_obj, Scalar_Minus_obj, &
        obj_Minus_Scalar


        ! OperatorOverloading/Assignment.part

        PROCEDURE, PUBLIC, PASS( obj ) :: obj_From_Tensor, &
        obj_From_Mat, obj_From_Vec

        GENERIC, PUBLIC :: ASSIGNMENT( = ) => obj_From_Tensor, &
        obj_From_Mat, obj_From_Vec


        ! OperatorOverLoading/Asterics.part

        PROCEDURE, PUBLIC, PASS :: objTimesScalar_1
        PROCEDURE, PUBLIC, PASS :: objTimesScalar_2
        PROCEDURE, PUBLIC, PASS :: objTimesobj
        PROCEDURE, PUBLIC, PASS :: objTimesMat
        PROCEDURE, PUBLIC, PASS :: objTimesVector
        PROCEDURE, PUBLIC, PASS :: objTimesTensor
        PROCEDURE, PUBLIC, PASS( obj ) :: ScalarTimesobj_1
        PROCEDURE, PUBLIC, PASS( obj ) :: ScalarTimesobj_2
        PROCEDURE, PUBLIC, PASS( obj ) :: MatTimesobj
        PROCEDURE, PUBLIC, PASS( obj ) :: VectorTimesobj
        PROCEDURE, PUBLIC, PASS( obj ) :: TensorTimesobj

        GENERIC, PUBLIC :: OPERATOR( * ) => objTimesScalar_1, &
        objTimesScalar_2, objTimesobj, objTimesMat, ScalarTimesobj_1, &
        ScalarTimesobj_2, MatTimesobj, objTimesVector, VectorTimesobj, &
        TensorTimesobj, objTimesTensor


        ! OperatorOverLoading/Matmul.part

        PROCEDURE, PUBLIC, PASS :: obj_matmul_obj
        PROCEDURE, PUBLIC, PASS :: obj_matmul_Mat
        PROCEDURE, PUBLIC, PASS :: obj_matmul_Tensor
        PROCEDURE, PUBLIC, PASS :: obj_matmul_Vec
        PROCEDURE, PUBLIC, PASS( obj ) :: Mat_matmul_obj
        PROCEDURE, PUBLIC, PASS( obj ) :: Tensor_matmul_obj
        PROCEDURE, PUBLIC, PASS( obj ) :: Vec_matmul_obj

        GENERIC, PUBLIC :: OPERATOR( .matmul. ) => obj_matmul_obj , &
        obj_matmul_Mat, obj_matmul_Tensor, Mat_matmul_obj, &
        Tensor_matmul_obj, obj_matmul_Vec, Vec_matmul_obj


        ! OperatorOverLoading/Otimes.part

        PROCEDURE, PUBLIC, PASS :: obj_Otimes_obj
        PROCEDURE, PUBLIC, PASS :: obj_Otimes_Mat
        PROCEDURE, PUBLIC, PASS :: obj_Otimes_Tensor
        PROCEDURE, PUBLIC, PASS :: obj_Otimes_Vec
        PROCEDURE, PUBLIC, PASS :: m_obj_Otimes_Vec
        PROCEDURE, PUBLIC, PASS( obj ) :: Mat_Otimes_obj
        PROCEDURE, PUBLIC, PASS( obj ) :: Tensor_Otimes_obj
        PROCEDURE, PUBLIC, PASS( obj ) :: Vec_Otimes_obj

        GENERIC, PUBLIC :: OPERATOR( .Otimes. ) => obj_Otimes_obj, &
        obj_Otimes_Mat, obj_Otimes_Tensor, Mat_Otimes_obj, &
        Tensor_Otimes_obj, Vec_Otimes_obj, obj_Otimes_Vec

        GENERIC, PUBLIC :: Otimes => obj_Otimes_obj, &
        obj_Otimes_Mat, obj_Otimes_Tensor, obj_Otimes_Vec, &
        m_obj_Otimes_Vec


        ! OperatorOverLoading/Invariant.part

        PROCEDURE, PUBLIC, PASS :: Invar_I1
        GENERIC, PUBLIC :: Invariant_I1 => Invar_I1
        GENERIC, PUBLIC :: OPERATOR( .Ione. ) => Invar_I1

        PROCEDURE, PUBLIC, PASS :: Invar_I2
        GENERIC, PUBLIC :: Invariant_I2 => Invar_I2
        GENERIC, PUBLIC :: OPERATOR( .Itwo. ) => Invar_I2

        PROCEDURE, PUBLIC, PASS :: Invar_I3
        GENERIC, PUBLIC :: Invariant_I3 => Invar_I3
        GENERIC, PUBLIC :: OPERATOR( .Ithree. ) => Invar_I3

        PROCEDURE, PUBLIC, PASS :: Invar_J2
        GENERIC, PUBLIC :: Invariant_J2 => Invar_J2
        GENERIC, PUBLIC :: OPERATOR( .Jtwo. ) => Invar_J2

        PROCEDURE, PUBLIC, PASS :: Invar_J3
        GENERIC, PUBLIC :: Invariant_J3 => Invar_J3
        GENERIC, PUBLIC :: OPERATOR( .Jthree. ) => Invar_J3

        PROCEDURE, PUBLIC, PASS :: Sigma_m
        GENERIC, PUBLIC :: OPERATOR( .sigmaM. ) => Sigma_m

        PROCEDURE, PUBLIC, PASS :: Sigma_Bar
        GENERIC, PUBLIC :: OPERATOR( .sigmaBAR. ) => Sigma_Bar

        PROCEDURE, PUBLIC, PASS :: Invar_Z
        GENERIC, PUBLIC :: Invariant_Z => Invar_Z
        GENERIC, PUBLIC :: OPERATOR( .z. ) => Invar_Z

        PROCEDURE, PUBLIC, PASS :: Invar_r
        GENERIC, PUBLIC :: OPERATOR( .r. ) => Invar_r

        PROCEDURE, PUBLIC, PASS :: m_Invar_LodeAngle
        GENERIC, PUBLIC :: LodeAngle => m_Invar_LodeAngle

        PROCEDURE, PUBLIC, PASS :: Invar_LodeAngle
        GENERIC, PUBLIC :: OPERATOR( .LodeAngle. ) => Invar_LodeAngle
        GENERIC, PUBLIC :: OPERATOR( .theta. ) => Invar_LodeAngle


        ! OperatorOverLoading/Shape.part

        PROCEDURE, PUBLIC, PASS( obj ) :: VoigtVector_1
        GENERIC, PUBLIC :: OPERATOR( .Shape. ) => VoigtVector_1


        ! SpectralDecomposition.part
        PROCEDURE, PUBLIC, PASS :: Eigens

        PROCEDURE, PUBLIC, PASS :: EigenVectors
        GENERIC, PUBLIC :: OPERATOR( .EigenVectors. ) => EigenVectors

        PROCEDURE, PUBLIC, PASS :: EigenValues
        GENERIC, PUBLIC :: OPERATOR( .EigenValues. ) => EigenValues

        PROCEDURE, PUBLIC, PASS :: PrincipalValue
        GENERIC, PUBLIC :: OPERATOR( .PrincipalValue. ) => PrincipalValue

        PROCEDURE, PUBLIC, PASS :: SpectralRadius
        GENERIC, PUBLIC :: OPERATOR( .SpectralRadius. ) => SpectralRadius


        ! TensorDecomposition.part

        PROCEDURE, PUBLIC, PASS :: m_SymmetricPart
        GENERIC, PUBLIC :: SymmetricPart => m_SymmetricPart

        GENERIC, PUBLIC :: OPERATOR( .sym. ) => m_SymmetricPart
        GENERIC, PUBLIC :: OPERATOR( .SymmetricPart. ) => m_SymmetricPart

        PROCEDURE, PUBLIC, PASS :: m_AntiSymmetricPart
        GENERIC, PUBLIC :: AntiSymmetricPart => m_AntiSymmetricPart

        GENERIC, PUBLIC :: OPERATOR( .AntiSym. ) => m_AntiSymmetricPart
        GENERIC, PUBLIC :: OPERATOR( .AntiSymmetricPart. ) => m_AntiSymmetricPart

        PROCEDURE, PUBLIC, PASS :: m_HydrostaticPart
        GENERIC, PUBLIC :: HydrostaticPart => m_HydrostaticPart

        GENERIC, PUBLIC :: OPERATOR( .HydrostaticPart. ) => m_HydrostaticPart
        GENERIC, PUBLIC :: OPERATOR( .Hydro. ) => m_HydrostaticPart

        GENERIC, PUBLIC :: SphericalPart => m_HydrostaticPart


        PROCEDURE, PUBLIC, PASS :: m_DeviatoricPart
        GENERIC, PUBLIC :: DeviatoricPart => m_DeviatoricPart

        GENERIC, PUBLIC :: OPERATOR( .Dev. ) => m_DeviatoricPart
        GENERIC, PUBLIC :: OPERATOR( .DeviatoricPart. ) => m_DeviatoricPart


        ! CauchyStress.part

        PROCEDURE, PUBLIC, PASS( obj ) :: getCauchyStress
        GENERIC, PUBLIC :: OPERATOR( .Cauchy.) => getCauchyStress
        GENERIC, PUBLIC :: OPERATOR( .Sigma.) => getCauchyStress

        ! Pk2Stress.part

        PROCEDURE, PUBLIC, PASS( obj ) :: getPK2Stress
        GENERIC, PUBLIC :: OPERATOR( .pkTWO. ) => getPK2Stress
        GENERIC, PUBLIC :: OPERATOR( .S. ) => getPK2Stress

        ! Pk1Stress.part

        PROCEDURE, PUBLIC, PASS( obj ) :: getPK1Stress
        GENERIC, PUBLIC :: OPERATOR( .pkONE. ) => getPK1Stress
        GENERIC, PUBLIC :: OPERATOR( .PI. ) => getPK1Stress

        ! KirchhoffStress.part

        PROCEDURE, PUBLIC, PASS( obj ) :: getKirchhoffStress
        GENERIC, PUBLIC :: OPERATOR( .Kirchhoff. ) => getKirchhoffStress
        GENERIC, PUBLIC :: OPERATOR( .Tau. ) => getKirchhoffStress

        ! EshelbyStress.part

        PROCEDURE, PUBLIC, PASS( obj ) :: getEshelbyStress
        GENERIC, PUBLIC :: OPERATOR( .Eshelby. ) => getEshelbyStress
        GENERIC, PUBLIC :: OPERATOR( .M. ) => getEshelbyStress

        ! Display.part

        PROCEDURE, PUBLIC, PASS :: Display

 END TYPE Stress_


!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                                 Interfaces
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .


    INTERFACE Stress_Pointer
        MODULE PROCEDURE Constructor_1, Constructor_2, Constructor_3, &
        Constructor_4, Constructor_5, Constructor_6
    END INTERFACE

    INTERFACE Stress
        MODULE PROCEDURE Constructor1, Constructor2, Constructor3, &
        Constructor4, Constructor5, Constructor6
    END INTERFACE


!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!                                                                   Contains
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

 CONTAINS

#include "./Constructor.part"
#include "./StressType.part"
#include "./Display.part"
#include "./getStress.part"

#include "./OperatorOverloading/Assignment.part"
#include "./OperatorOverloading/Addition.part"
#include "./OperatorOverloading/Asterics.part"
#include "./OperatorOverloading/Matmul.part"
#include "./OperatorOverloading/Otimes.part"
#include "./OperatorOverloading/Invariant.part"
#include "./OperatorOverloading/Shape.part"

#include "./SpectralDecomposition.part"
#include "./TensorDecomposition.part"

#include "./CauchyStress.part"
#include "./Pk2Stress.part"
#include "./Pk1Stress.part"
#include "./KirchhoffStress.part"
#include "./EshelbyStress.part"

 END MODULE Stress_Class

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------