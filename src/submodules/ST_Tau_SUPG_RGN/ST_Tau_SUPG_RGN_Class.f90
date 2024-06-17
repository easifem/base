!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   ST_Tau_SUPG_RGN_Class.f90
!                   Last Update :   Nov-15-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!  TYPE :: Module
!
!   DESCRIPTION
!       -   STElemShapeData_ Class is extended to define the supg stabilization
!           parameter.
!
!==============================================================================

 MODULE ST_Tau_SUPG_RGN_Class
    USE GlobalData
    USE IO
    USE STElemShapeData_Class
    USE STShapeData_Class
    
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: ST_Tau_SUPG_RGN_, ST_Tau_SUPG_RGN, ST_Tau_SUPG_RGN_Pointer

!------------------------------------------------------------------------------
!                                                        ST_Tau_SUPG_RGN_WTSA_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( STElemShapeData_ ) :: ST_Tau_SUPG_RGN_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!   1.  This class for computation of mass matrix
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    CONTAINS

    PROCEDURE, PUBLIC, PASS( Obj ) :: &
#include "./MethodNamesForScalar.part"
    PROCEDURE, PUBLIC, PASS( Obj ) :: &
#include "./MethodNamesForVector.part"

 END TYPE ST_Tau_SUPG_RGN_

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

    ! INTERFACES
    INTERFACE ST_Tau_SUPG_RGN_Pointer
        MODULE PROCEDURE Constructor_1, Constructor_2
    END INTERFACE

    INTERFACE ST_Tau_SUPG_RGN
        MODULE PROCEDURE Constructor1, Constructor2 
    END INTERFACE

!------------------------------------------------------------------------------
!                                                                      CONTAINS
!------------------------------------------------------------------------------

 CONTAINS

#undef STMat

#include "./Constructor.part"
#include "./SUPG_Scalar_1.part"
#include "./SUPG_Scalar_2.part"
#include "./SUPG_Scalar_3.part"
#include "./SUPG_Scalar_4.part"
#include "./SUPG_Scalar_5.part"
#include "./SUPG_Scalar_6.part"
#include "./SUPG_Scalar_7.part"
#include "./SUPG_Scalar_8.part"
#include "./SUPG_Scalar_9.part"
#include "./SUPG_Scalar_10.part"
#include "./SUPG_Scalar_11.part"
#include "./SUPG_Scalar_12.part"

#include "./SUPG_Vector_1.part"
#include "./SUPG_Vector_2.part"
#include "./SUPG_Vector_3.part"
#include "./SUPG_Vector_4.part"
#include "./SUPG_Vector_5.part"
#include "./SUPG_Vector_6.part"
#include "./SUPG_Vector_7.part"
#include "./SUPG_Vector_8.part"
#include "./SUPG_Vector_9.part"
#include "./SUPG_Vector_10.part"
#include "./SUPG_Vector_11.part"
#include "./SUPG_Vector_12.part"

 END MODULE ST_Tau_SUPG_RGN_Class

