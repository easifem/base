!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma                            
!                   Position    :   Doctral Student                                       
!                   Institute   :   Kyoto Univeristy, Japan                        
!                   Program name:   Stress_Class.f90
!                   Last Update :   March-29-2017
!                                                               
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
! 
!   Type:: Module
!
!   Info::  -   Defines a Stress Class
!
!
!==============================================================================
!
! List of items 
!
!
!------------------------------------------------------------------------------
!
!                                                       -----------------------
!                                                               USE ASSOCIATION
!                                                       -----------------------
!
 MODULE Stress_Class
 USE GlobalData
 USE IO
!
 IMPLICIT NONE
!
!------------------------------------------------------------------------------
!                                                         IterativeLinearSolver
!------------------------------------------------------------------------------
!
 TYPE :: Stress_
!
!   Description
!------------------------------------------------------------------------------
!       1.  - Stress Class is defined
!
!------------------------------------------------------------------------------
!   Instance variable
        REAL(DFP), ALLOCATABLE, DIMENSION(:) :: Sigma
!
!   Type bound procedures
    CONTAINS
!   -------
    PROCEDURE, PUBLIC, PASS ::  Initiate
    PROCEDURE, PUBLIC, PASS ::  setSigma
    PROCEDURE, PUBLIC, PASS ::  getSigma
    PROCEDURE, PUBLIC, PASS ::  getLength
    PROCEDURE, PUBLIC, PASS ::  getSigma_m
    PROCEDURE, PUBLIC, PASS ::  getSigma_bar
    PROCEDURE, PUBLIC, PASS ::  getJ2, getJ3
    PROCEDURE, PUBLIC, PASS ::  getLodeAngle
    PROCEDURE, PUBLIC, PASS ::  getDeviatoricPart
    PROCEDURE, PUBLIC, PASS ::  getHillTensor
    PROCEDURE, PUBLIC, PASS ::  getDsigma_mDsigma
    PROCEDURE, PUBLIC, PASS ::  getDJ2Dsigma
    PROCEDURE, PUBLIC, PASS ::  getDsigma_barDJ2
    PROCEDURE, PUBLIC, PASS ::  getDsigma_barDsigma
    PROCEDURE, PUBLIC, PASS ::  getDJ3Dsigma
    PROCEDURE, PUBLIC, PASS ::  getStressInvariants
!
 END TYPE Stress_
!
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!
!                                                               ---------------
!                                                                PUBLIC/PRIVATE
!                                                               ---------------
     PRIVATE    ::  Initiate
     PRIVATE    ::  setSigma
     PRIVATE    ::  getSigma
     PRIVATE    ::  getLength
     PRIVATE    ::  getSigma_m
     PRIVATE    ::  getSigma_bar
     PRIVATE    ::  getJ2, getJ3
     PRIVATE    ::  getLodeAngle
     PRIVATE    ::  getDeviatoricPart
     PRIVATE    ::  getHillTensor
     PRIVATE    ::  getDsigma_mDsigma
     PRIVATE    ::  getDJ2Dsigma
     PRIVATE    ::  getDsigma_barDJ2
     PRIVATE    ::  getDsigma_barDsigma
     PRIVATE    ::  getDJ3Dsigma
     PRIVATE    ::  getStressInvariants


!                                                                   -----------
!                                                                      CONTAINS
!                                                                   -----------
 CONTAINS
!
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!
 INCLUDE "./Initiate.part"

 INCLUDE "./setSigma.part"

 INCLUDE "./getSigma.part"

 INCLUDE "./getLength.part"

 INCLUDE "./Invariants.part"

 INCLUDE "./StressDecomposition.part"

 INCLUDE "./getHillTensor.part"

 INCLUDE "./StressDerivatives.part"

!
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!
END MODULE Stress_Class
