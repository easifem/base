!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   MaterialJacobian_Class.f90
!                   Last Update :   Dec-29-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!   TYPE :: Module
!
!   DESCRIPTION
!       -   Rank2Tensor class is extended to Define MaterialJacobian Class
!==============================================================================

 MODULE MaterialJacobian_Class
    USE GlobalData
    USE IO
    USE String_Class
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: MaterialJacobian_, MaterialJacobian, MaterialJacobian_Pointer

!------------------------------------------------------------------------------
!                                                            MaterialJacobian_
!------------------------------------------------------------------------------

 TYPE :: MaterialJacobian_
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   Material Jacobian Class
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    REAL( DFP ), ALLOCATABLE :: C( :, : )
    TYPE( String_ ) :: StressType, StrainType, RateType

    CONTAINS

        ! Constructor.part

            PROCEDURE, PUBLIC, PASS( obj ) :: Initiate1, Initiate2, &
            Initiate3, Initiate4, Initiate5, Initiate6, &
            Initiate7, getSize, DeallocateData, Initiate8

            GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2, &
            Initiate3, Initiate4, Initiate5, Initiate6, &
            Initiate7, Initiate8

            GENERIC, PUBLIC :: ASSIGNMENT( = ) => Initiate3, Initiate8
            GENERIC, PUBLIC :: OPERATOR( .SIZE. ) => getSize

        ! Names.part

            PROCEDURE, PUBLIC, PASS( obj ) :: setStressType1, setStressType2,&
            setStrainType1, setStrainType2, setRateType1, setRateType2, &
            getStressType, getStrainType, getRateType

            GENERIC, PUBLIC :: setStressType => setStressType1, setStressType2
            GENERIC, PUBLIC :: setStrainType => setStrainType1, setStrainType2
            GENERIC, PUBLIC :: setRateType => setRateType1, setRateType2

            GENERIC, PUBLIC :: OPERATOR( .StressType. ) => getStressType
            GENERIC, PUBLIC :: OPERATOR( .StrainType. ) => getStrainType
            GENERIC, PUBLIC :: OPERATOR( .RateType. ) => getRateType


        ! getCijkl.part

            PROCEDURE, PUBLIC, PASS( obj ) :: getCijkl, obj2Mat, &
            getCijkl_Pointer, Cijkl_Pointer

            GENERIC, PUBLIC :: ASSIGNMENT( = ) => obj2Mat


        ! OperatorOverloading/ .Cijkl.

            PROCEDURE, PUBLIC, PASS( obj ) :: Cijkl_1, Cijkl_2, &
            Cijkl_4, Cijkl, Cijkl_5

            GENERIC, PUBLIC :: OPERATOR( .Cijkl. ) => Cijkl_1, Cijkl, &
            Cijkl_5

            GENERIC, PUBLIC :: OPERATOR( .Shape. ) => Cijkl_5

        ! OperatorOverloading/Contraction

            PROCEDURE, PUBLIC, PASS( obj ) :: Contraction_1, Contraction_2, &
            Contraction_3, Contraction_4

            GENERIC, PUBLIC :: OPERATOR( .Contraction. ) => &
            Contraction_1, Contraction_2, Contraction_3, Contraction_4

        ! OperatorOverloading/Asterics

            PROCEDURE, PUBLIC, PASS( obj ) :: obj_Times_Scalar, Scalar_Times_obj

            GENERIC, PUBLIC :: OPERATOR( * ) => obj_Times_Scalar, &
            Scalar_Times_obj


        ! OperatorOverloading/Matmul

            PROCEDURE, PUBLIC, PASS( obj ) :: obj_Matmul_Vec, Vec_Matmul_obj

            GENERIC, PUBLIC :: OPERATOR( .matmul. ) => obj_Matmul_Vec, &
            Vec_Matmul_obj


        ! OperatorOverloading/Addition

            PROCEDURE, PUBLIC, PASS( obj ) :: obj_Add_obj, obj_Add_Mat, &
            Mat_Add_obj, obj_Add_Scalar, Scalar_Add_obj

            GENERIC, PUBLIC :: OPERATOR( + ) => obj_Add_obj, obj_Add_Mat, &
            Mat_Add_obj, obj_Add_Scalar, Scalar_Add_obj


        ! OperatorOverloading/Subtraction

            PROCEDURE, PUBLIC, PASS( obj ) :: obj_Minus_obj, obj_Minus_Mat, &
            Mat_Minus_obj, obj_Minus_Scalar, Scalar_Minus_obj

            GENERIC, PUBLIC :: OPERATOR( - ) => obj_Minus_obj, obj_Minus_Mat, &
            Mat_Minus_obj, obj_Minus_Scalar, Scalar_Minus_obj


        ! Display.part

            PROCEDURE, PUBLIC, PASS :: Display

 END TYPE MaterialJacobian_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    INTERFACE MaterialJacobian
        MODULE PROCEDURE Constructor1, Constructor2, Constructor3, &
        Constructor4, Constructor5, Constructor6, Constructor7
    END INTERFACE

    INTERFACE MaterialJacobian_Pointer
        MODULE PROCEDURE Constructor_1, Constructor_2, Constructor_3, &
        Constructor_4, Constructor_5, Constructor_6, Constructor_7
    END INTERFACE

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

 CONTAINS

    INCLUDE "./Initiate.part"
    INCLUDE "./MaterialJacobian_Pointer.part"
    INCLUDE "./MaterialJacobian.part"
    INCLUDE "./getCijkl.part"
    INCLUDE "./Names.part"

    INCLUDE "./OperatorOverloading/Cijkl.part"
    INCLUDE "./OperatorOverloading/Contraction.part"
    INCLUDE "./OperatorOverloading/Asterics.part"
    INCLUDE "./OperatorOverloading/Matmul.part"
    INCLUDE "./OperatorOverloading/Addition.part"
    INCLUDE "./OperatorOverloading/Subtraction.part"

    INCLUDE "./Display.part"


 END MODULE MaterialJacobian_Class

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

