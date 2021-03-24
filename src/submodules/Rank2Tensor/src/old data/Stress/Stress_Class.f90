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

        PROCEDURE, PUBLIC, PASS( Obj ) :: getStressType, setStressType
        GENERIC, PUBLIC :: OPERATOR( .StressType. ) => getStressType
        GENERIC, PUBLIC :: ASSIGNMENT( = ) => setStressType


        ! getStress.part

        PROCEDURE, PUBLIC, PASS :: s_getStress_1
        PROCEDURE, PUBLIC, PASS :: s_getStress_2
        PROCEDURE, PUBLIC, PASS :: s_getStress_5
        GENERIC, PUBLIC :: getStress => s_getStress_1, s_getStress_2, &
        s_getStress_5

        PROCEDURE, PUBLIC, PASS( Obj ) :: s_getStress_3, s_getStress_4, &
        s_getStress_6
        GENERIC, PUBLIC :: ASSIGNMENT( = ) => s_getStress_3, s_getStress_4, &
        s_getStress_6

        ! OperatorOverloading/Addition.part

        PROCEDURE, PUBLIC, PASS :: Obj_Add_Obj
        PROCEDURE, PUBLIC, PASS :: Obj_Add_Mat
        PROCEDURE, PUBLIC, PASS :: Obj_Add_Vec
        PROCEDURE, PUBLIC, PASS :: Obj_Add_Scalar
        PROCEDURE, PUBLIC, PASS( Obj ) :: Mat_Add_Obj
        PROCEDURE, PUBLIC, PASS( Obj ) :: Vec_Add_Obj
        PROCEDURE, PUBLIC, PASS( Obj ) :: Scalar_Add_Obj
        
        GENERIC, PUBLIC :: OPERATOR( + ) => Obj_Add_Mat, Obj_Add_Obj, &
        Mat_Add_Obj, Obj_Add_Vec, Vec_Add_Obj, Scalar_Add_Obj, &
        Obj_Add_Scalar

        PROCEDURE, PUBLIC, PASS :: Obj_Minus_Obj
        PROCEDURE, PUBLIC, PASS :: Obj_Minus_Mat
        PROCEDURE, PUBLIC, PASS :: Obj_Minus_Vec
        PROCEDURE, PUBLIC, PASS :: Obj_Minus_Scalar
        PROCEDURE, PUBLIC, PASS( Obj ) :: Mat_Minus_Obj
        PROCEDURE, PUBLIC, PASS( Obj ) :: Vec_Minus_Obj
        PROCEDURE, PUBLIC, PASS( Obj ) :: Scalar_Minus_Obj

        GENERIC, PUBLIC :: OPERATOR( - ) => Obj_Minus_Mat, Obj_Minus_Obj, &
        Mat_Minus_Obj, Obj_Minus_Vec, Vec_Minus_Obj, Scalar_Minus_Obj, &
        Obj_Minus_Scalar


        ! OperatorOverloading/Assignment.part

        PROCEDURE, PUBLIC, PASS( Obj ) :: Obj_From_Tensor, &
        Obj_From_Mat, Obj_From_Vec

        GENERIC, PUBLIC :: ASSIGNMENT( = ) => Obj_From_Tensor, &
        Obj_From_Mat, Obj_From_Vec


        ! OperatorOverLoading/Asterics.part

        PROCEDURE, PUBLIC, PASS :: ObjTimesScalar_1
        PROCEDURE, PUBLIC, PASS :: ObjTimesScalar_2
        PROCEDURE, PUBLIC, PASS :: ObjTimesObj
        PROCEDURE, PUBLIC, PASS :: ObjTimesMat
        PROCEDURE, PUBLIC, PASS :: ObjTimesVector
        PROCEDURE, PUBLIC, PASS :: ObjTimesTensor
        PROCEDURE, PUBLIC, PASS( Obj ) :: ScalarTimesObj_1
        PROCEDURE, PUBLIC, PASS( Obj ) :: ScalarTimesObj_2
        PROCEDURE, PUBLIC, PASS( Obj ) :: MatTimesObj
        PROCEDURE, PUBLIC, PASS( Obj ) :: VectorTimesObj
        PROCEDURE, PUBLIC, PASS( Obj ) :: TensorTimesObj

        GENERIC, PUBLIC :: OPERATOR( * ) => ObjTimesScalar_1, &
        ObjTimesScalar_2, ObjTimesObj, ObjTimesMat, ScalarTimesObj_1, &
        ScalarTimesObj_2, MatTimesObj, ObjTimesVector, VectorTimesObj, &
        TensorTimesObj, ObjTimesTensor


        ! OperatorOverLoading/Matmul.part

        PROCEDURE, PUBLIC, PASS :: Obj_matmul_Obj
        PROCEDURE, PUBLIC, PASS :: Obj_matmul_Mat
        PROCEDURE, PUBLIC, PASS :: Obj_matmul_Tensor
        PROCEDURE, PUBLIC, PASS :: Obj_matmul_Vec
        PROCEDURE, PUBLIC, PASS( Obj ) :: Mat_matmul_Obj
        PROCEDURE, PUBLIC, PASS( Obj ) :: Tensor_matmul_Obj
        PROCEDURE, PUBLIC, PASS( Obj ) :: Vec_matmul_Obj

        GENERIC, PUBLIC :: OPERATOR( .matmul. ) => Obj_matmul_Obj , &
        Obj_matmul_Mat, Obj_matmul_Tensor, Mat_matmul_Obj, &
        Tensor_matmul_Obj, Obj_matmul_Vec, Vec_matmul_Obj


        ! OperatorOverLoading/Otimes.part

        PROCEDURE, PUBLIC, PASS :: Obj_Otimes_Obj
        PROCEDURE, PUBLIC, PASS :: Obj_Otimes_Mat
        PROCEDURE, PUBLIC, PASS :: Obj_Otimes_Tensor
        PROCEDURE, PUBLIC, PASS :: Obj_Otimes_Vec
        PROCEDURE, PUBLIC, PASS :: m_Obj_Otimes_Vec
        PROCEDURE, PUBLIC, PASS( Obj ) :: Mat_Otimes_Obj
        PROCEDURE, PUBLIC, PASS( Obj ) :: Tensor_Otimes_Obj
        PROCEDURE, PUBLIC, PASS( Obj ) :: Vec_Otimes_Obj

        GENERIC, PUBLIC :: OPERATOR( .Otimes. ) => Obj_Otimes_Obj, &
        Obj_Otimes_Mat, Obj_Otimes_Tensor, Mat_Otimes_Obj, &
        Tensor_Otimes_Obj, Vec_Otimes_Obj, Obj_Otimes_Vec

        GENERIC, PUBLIC :: Otimes => Obj_Otimes_Obj, &
        Obj_Otimes_Mat, Obj_Otimes_Tensor, Obj_Otimes_Vec, &
        m_Obj_Otimes_Vec


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

        PROCEDURE, PUBLIC, PASS( Obj ) :: VoigtVector_1
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

        PROCEDURE, PUBLIC, PASS( Obj ) :: getCauchyStress
        GENERIC, PUBLIC :: OPERATOR( .Cauchy.) => getCauchyStress
        GENERIC, PUBLIC :: OPERATOR( .Sigma.) => getCauchyStress

        ! Pk2Stress.part

        PROCEDURE, PUBLIC, PASS( Obj ) :: getPK2Stress
        GENERIC, PUBLIC :: OPERATOR( .pkTWO. ) => getPK2Stress
        GENERIC, PUBLIC :: OPERATOR( .S. ) => getPK2Stress

        ! Pk1Stress.part

        PROCEDURE, PUBLIC, PASS( Obj ) :: getPK1Stress
        GENERIC, PUBLIC :: OPERATOR( .pkONE. ) => getPK1Stress
        GENERIC, PUBLIC :: OPERATOR( .PI. ) => getPK1Stress

        ! KirchhoffStress.part

        PROCEDURE, PUBLIC, PASS( Obj ) :: getKirchhoffStress
        GENERIC, PUBLIC :: OPERATOR( .Kirchhoff. ) => getKirchhoffStress
        GENERIC, PUBLIC :: OPERATOR( .Tau. ) => getKirchhoffStress

        ! EshelbyStress.part

        PROCEDURE, PUBLIC, PASS( Obj ) :: getEshelbyStress
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