!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   STFextVector_Class.f90
!                   Last Update :   Nov-23-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!  TYPE :: Module
!
!   DESCRIPTION
!       -   STElemShapeData_ Class is extended for computing the STFextVector.
!   Updates
!       -   Nov-23-2017
!       -   Jan-04-2018 Added STFextVector_Pointer
!==============================================================================

 MODULE STFextVector_Class
    USE IO
    USE GlobalData
    USE STElemShapeData_Class
    USE STShapeData_Class
    USE ShapeData_Class
    
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: STFextVector, STFextVector_, STFextVector_Pointer

!------------------------------------------------------------------------------
!                                                                 STFextVector_
!------------------------------------------------------------------------------

 TYPE, EXTENDS( STElemShapeData_ ) :: STFextVector_

!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!       1.  This class computes the Space-Time External Force vector
!           for space-time element.
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    CONTAINS
    
        PROCEDURE, PUBLIC, PASS :: & 
#include "./MethodNames.part"

 END TYPE STFextVector_

!------------------------------------------------------------------------------
!                                                                   INTERFACES
!------------------------------------------------------------------------------

    INTERFACE STFextVector
        MODULE PROCEDURE Constructor1, Constructor2, Constructor3
    END INTERFACE
    
    INTERFACE STFextVector_Pointer
        MODULE PROCEDURE Constructor_1, Constructor_2, Constructor_3
    END INTERFACE

!------------------------------------------------------------------------------
!                                                                      CONTAINS
!------------------------------------------------------------------------------

 CONTAINS

#include "./Constructor.part"
#include "./FextVector_1.part"
#include "./FextVector_2.part"
#include "./FextVector_3.part"
#include "./FextVector_4.part"
#include "./FextVector_5.part"
#include "./FextVector_6.part"
#include "./FextVector_7.part"
#include "./FextVector_8.part"
#include "./FextVector_9.part"
#include "./FextVector_10.part"
#include "./FextVector_11.part"
#include "./FextVector_12.part"
#include "./FextVector_13.part"
#include "./FextVector_14.part"
#include "./FextVector_15.part"
#include "./FextVector_16.part"
#include "./FextVector_17.part"
#include "./FextVector_18.part"
#include "./FextVector_19.part"
#include "./FextVector_20.part"
#include "./FextVector_21.part"
#include "./FextVector_22.part"
#include "./FextVector_23.part"
#include "./FextVector_24.part"
#include "./FextVector_25.part"
#include "./FextVector_26.part"

 END MODULE STFextVector_Class

!------------------------------------------------------------------------------
!                                                                      
!------------------------------------------------------------------------------
