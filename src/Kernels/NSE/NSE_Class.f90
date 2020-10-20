
!> authors: Dr. Vikas Sharma
!
!  NSE_ data solves the Navier-Stokes flow
! - Uniform mass density
! - Incompressible flow
! - Conservative or non-conservative form
!
! Boundary conditions
! - Dirichlet
! - Neumann
! - Free surface
!
! Main Options
! - Static
! - SemiDiscrete
! - STFEM

MODULE NSE_Class
USE EASIFEM
USE Kernel_Class
IMPLICIT NONE
PRIVATE

#include "../Kernel_Def.inc"

!----------------------------------------------------------------------------
!                                                                       NSE_
!----------------------------------------------------------------------------

TYPE, EXTENDS( Kernel_ ) :: NSE_

END TYPE NSE_

PUBLIC :: NSE_

TYPE :: NSEPOINTER_
  CLASS( NSE_ ), POINTER :: ptr => NULL()
END TYPE NSEPOINTER_

PUBLIC :: NsePointer_

!----------------------------------------------------------------------------
!                                                                       NSE_
!----------------------------------------------------------------------------




END MODULE NSE_Class