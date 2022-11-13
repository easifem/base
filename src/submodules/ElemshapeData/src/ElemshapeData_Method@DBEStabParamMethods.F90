! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE(ElemshapeData_Method) DBEStabParamMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam1a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam1a_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam1a_footer.inc"
END SUBROUTINE DBEStabParam1a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam2a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam1a_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam1a_footer.inc"
END SUBROUTINE DBEStabParam2a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam3a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam1a_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam1a_footer.inc"
END SUBROUTINE DBEStabParam3a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam4a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam4a_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam4a_footer.inc"
END SUBROUTINE DBEStabParam4a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam5a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam5a_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam5a_footer.inc"
END SUBROUTINE DBEStabParam5a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam6a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam5a_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam5a_footer.inc"

END SUBROUTINE DBEStabParam6a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam7a(obj, tau, h, nulsic, c, &
  & val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam5a_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam5a_footer.inc"
END SUBROUTINE DBEStabParam7a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam8a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam8a_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam8a_footer.inc"
END SUBROUTINE DBEStabParam8a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam11a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam11a_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam11a_footer.inc"
END SUBROUTINE DBEStabParam11a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam12a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam11a_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam11a_footer.inc"
END SUBROUTINE DBEStabParam12a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam13a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam11a_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam11a_footer.inc"
END SUBROUTINE DBEStabParam13a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam14a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam14a_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam14a_footer.inc"
END SUBROUTINE DBEStabParam14a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam15a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam15a_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam15a_footer.inc"
END SUBROUTINE DBEStabParam15a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam16a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam15a_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam15a_footer.inc"
END SUBROUTINE DBEStabParam16a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam17a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam15a_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam15a_footer.inc"
END SUBROUTINE DBEStabParam17a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam18a(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam18a_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam18a_footer.inc"
END SUBROUTINE DBEStabParam18a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

SUBROUTINE DBEStabParam_a(obj, tau, h, nulsic, &
  & c, val, nu, k, phi, opt, L0, c4, dt)
  TYPE(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  INTEGER(I4B), INTENT(IN) :: opt
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam_a.inc"
  !!
END SUBROUTINE DBEStabParam_a

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam1b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam1b_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam1b_footer.inc"
END SUBROUTINE DBEStabParam1b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam2b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam1b_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam1b_footer.inc"
END SUBROUTINE DBEStabParam2b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam3b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam1b_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam1b_footer.inc"
END SUBROUTINE DBEStabParam3b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam4b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam4b_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam4b_footer.inc"
END SUBROUTINE DBEStabParam4b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam5b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam5b_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam5b_footer.inc"
END SUBROUTINE DBEStabParam5b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam6b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam5b_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam5b_footer.inc"
END SUBROUTINE DBEStabParam6b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam7b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam5b_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam5b_footer.inc"
END SUBROUTINE DBEStabParam7b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam8b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam8b_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam8b_footer.inc"
END SUBROUTINE DBEStabParam8b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam11b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam11b_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam11b_footer.inc"
END SUBROUTINE DBEStabParam11b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam12b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam11b_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam11b_footer.inc"
END SUBROUTINE DBEStabParam12b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam13b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam11b_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam11b_footer.inc"
END SUBROUTINE DBEStabParam13b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam14b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam14b_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam14b_footer.inc"
END SUBROUTINE DBEStabParam14b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam15b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam15b_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam15b_footer.inc"
END SUBROUTINE DBEStabParam15b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam16b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam15b_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam15b_footer.inc"
END SUBROUTINE DBEStabParam16b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam17b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam15b_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam15b_footer.inc"
END SUBROUTINE DBEStabParam17b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam18b(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam18b_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam18b_footer.inc"
END SUBROUTINE DBEStabParam18b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

SUBROUTINE DBEStabParam_b(obj, tau, h, nulsic, &
  & c, val, nu, k, phi, opt, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  TYPE(FEVariable_), INTENT(IN) :: nu
  TYPE(FEVariable_), INTENT(IN) :: k
  TYPE(FEVariable_), INTENT(IN) :: phi
  INTEGER(I4B), INTENT(IN) :: opt
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam_b.inc"
  !!
END SUBROUTINE DBEStabParam_b

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam1c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam1c_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam1c_footer.inc"
END SUBROUTINE DBEStabParam1c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam2c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam1c_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam1c_footer.inc"
END SUBROUTINE DBEStabParam2c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam3c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam1c_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam1c_footer.inc"
END SUBROUTINE DBEStabParam3c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam4c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam4c_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam4c_footer.inc"
END SUBROUTINE DBEStabParam4c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam5c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam5c_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam5c_footer.inc"
END SUBROUTINE DBEStabParam5c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam6c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam5c_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam5c_footer.inc"
END SUBROUTINE DBEStabParam6c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam7c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam5c_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam5c_footer.inc"
END SUBROUTINE DBEStabParam7c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam8c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam8c_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam8c_footer.inc"
END SUBROUTINE DBEStabParam8c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam11c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam11c_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam11c_footer.inc"
END SUBROUTINE DBEStabParam11c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam12c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam11c_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam11c_footer.inc"
END SUBROUTINE DBEStabParam12c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam13c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam11c_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam11c_footer.inc"
END SUBROUTINE DBEStabParam13c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam14c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam14c_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam14c_footer.inc"
END SUBROUTINE DBEStabParam14c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam15c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam15c_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam15c_footer.inc"
END SUBROUTINE DBEStabParam15c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam16c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam15c_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam15c_footer.inc"
END SUBROUTINE DBEStabParam16c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam17c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam15c_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam15c_footer.inc"
END SUBROUTINE DBEStabParam17c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam18c(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam18c_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam18c_footer.inc"
END SUBROUTINE DBEStabParam18c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

SUBROUTINE DBEStabParam_c(obj, tau, h, nulsic, &
  & c, val, nu, k, phi, opt, L0, c4, dt)
  TYPE(ElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  INTEGER(I4B), INTENT(IN) :: opt
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam_c.inc"
  !!
END SUBROUTINE DBEStabParam_c

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam1d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam1d_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam1d_footer.inc"
END SUBROUTINE DBEStabParam1d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam2d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam1d_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam1d_footer.inc"
END SUBROUTINE DBEStabParam2d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam3d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam1d_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam1d_footer.inc"
END SUBROUTINE DBEStabParam3d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam4d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam4d_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam4d_footer.inc"
END SUBROUTINE DBEStabParam4d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam5d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam5d_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam5d_footer.inc"
END SUBROUTINE DBEStabParam5d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam6d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam5d_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam5d_footer.inc"
END SUBROUTINE DBEStabParam6d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam7d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam5d_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam5d_footer.inc"
END SUBROUTINE DBEStabParam7d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam8d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam8d_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam8d_footer.inc"
END SUBROUTINE DBEStabParam8d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam11d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam11d_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam11d_footer.inc"
END SUBROUTINE DBEStabParam11d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam12d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam11d_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam11d_footer.inc"
END SUBROUTINE DBEStabParam12d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam13d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam11d_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam11d_footer.inc"
END SUBROUTINE DBEStabParam13d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam14d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam14d_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam14d_footer.inc"
END SUBROUTINE DBEStabParam14d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam15d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam15d_head.inc"
  !!
  nulsic = tau * DOT_PRODUCT(c, c)
  !!
#include "./include/DBEStabParam15d_footer.inc"
END SUBROUTINE DBEStabParam15d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam16d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam15d_head.inc"
  !!
  nulsic = (h**2) / tau
  !!
#include "./include/DBEStabParam15d_footer.inc"
END SUBROUTINE DBEStabParam16d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam17d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam15d_head.inc"
  !!
  nulsic = L0 * (h / tau)
  !!
#include "./include/DBEStabParam15d_footer.inc"
END SUBROUTINE DBEStabParam17d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

PURE SUBROUTINE DBEStabParam18d(obj, tau, h, nulsic, &
    & c, val, nu, k, phi, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  REAL(DFP), INTENT(IN) :: L0
  REAL(DFP), INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam18d_head.inc"
  !!
  nulsic = (h**2) / tausups
  !!
#include "./include/DBEStabParam18d_footer.inc"
END SUBROUTINE DBEStabParam18d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

SUBROUTINE DBEStabParam_d(obj, tau, h, nulsic, &
  & c, val, nu, k, phi, opt, L0, c4, dt)
  CLASS(STElemshapeData_), INTENT(IN) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: tau
  TYPE(FEVariable_), INTENT(INOUT) :: h
  TYPE(FEVariable_), INTENT(INOUT) :: nulsic
  TYPE(FEVariable_), INTENT(IN) :: c
  TYPE(FEVariable_), INTENT(IN) :: val
  REAL(DFP), INTENT(IN) :: nu
  REAL(DFP), INTENT(IN) :: k
  REAL(DFP), INTENT(IN) :: phi
  INTEGER(I4B), INTENT(IN) :: opt
  REAL(DFP), OPTIONAL, INTENT(IN) :: L0
  REAL(DFP), OPTIONAL, INTENT(IN) :: c4
  REAL(DFP), OPTIONAL, INTENT(IN) :: dt
  !!
#include "./include/DBEStabParam_d.inc"
  !!
END SUBROUTINE DBEStabParam_d

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetDBEStabParam1
SELECT TYPE (obj)
TYPE IS (ElemshapeData_)
  CALL DBEStabParam_a(obj=obj, tau=tau, h=h, nulsic=nulsic, &
    & c=c, val=val, nu=nu, k=k, phi=phi, opt=opt, L0=L0, c4=c4, dt=dt)
CLASS IS (STElemshapeData_)
  CALL DBEStabParam_b(obj=obj, tau=tau, h=h, nulsic=nulsic, &
    & c=c, val=val, nu=nu, k=k, phi=phi, opt=opt, L0=L0, c4=c4, dt=dt)
CLASS DEFAULT
  CALL Errormsg( &
  & msg="Cannot recognise the type of obj", &
  & file=__FILE__, routine="elemsd_GetDBEStabParam1()", line=__LINE__, &
  & unitno=stderr)
END SELECT
END PROCEDURE elemsd_GetDBEStabParam1

!----------------------------------------------------------------------------
!                                                             GetSUPGParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetDBEStabParam2
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: ans1(:, :)
REAL(DFP), ALLOCATABLE :: ans2(:, :)
REAL(DFP), ALLOCATABLE :: ans3(:, :)
TYPE(FEVariable_) :: a1, a2, a3
!!
!! main
!!
CALL Reallocate(ans1, SIZE(obj(1)%N, 2), SIZE(obj))
CALL Reallocate(ans2, SIZE(obj(1)%N, 2), SIZE(obj))
CALL Reallocate(ans3, SIZE(obj(1)%N, 2), SIZE(obj))
!!
DO ii = 1, SIZE(obj)
  !!
  CALL elemsd_GetDBEStabParam1( &
    & obj=obj(ii), &
    & tau=a1, &
    & h=a2, &
    & nulsic=a3, &
    & c=c, &
    & val=val, &
    & nu=nu, &
    & k=k, &
    & phi=phi, &
    & opt=opt, &
    & L0=L0, &
    & c4=c4, &
    & dt=dt)
  !!
  ans1(:, ii) = Get(a1, TypeFEVariableScalar, TypeFEVariableSpace)
  ans2(:, ii) = Get(a2, TypeFEVariableScalar, TypeFEVariableSpace)
  ans3(:, ii) = Get(a3, TypeFEVariableScalar, TypeFEVariableSpace)
  !!
END DO
!!
tau = QuadratureVariable(ans1, TypeFEVariableScalar, TypeFEVariableSpaceTime)
h = QuadratureVariable(ans2, TypeFEVariableScalar, TypeFEVariableSpaceTime)
nulsic = QuadratureVariable(ans3, TypeFEVariableScalar,  &
  & TypeFEVariableSpaceTime)
!!
CALL DEALLOCATE (a1)
CALL DEALLOCATE (a2)
CALL DEALLOCATE (a3)
!!
DEALLOCATE (ans1, ans2, ans3)
END PROCEDURE elemsd_GetDBEStabParam2

!----------------------------------------------------------------------------
!                                                               getSUPGParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetDBEStabParam3
SELECT TYPE (obj)
TYPE IS (ElemshapeData_)
  CALL DBEStabParam_c(obj=obj, tau=tau, h=h, nulsic=nulsic, &
    & c=c, val=val, nu=nu, k=k, phi=phi, opt=opt, L0=L0, c4=c4, dt=dt)
CLASS IS (STElemshapeData_)
  CALL DBEStabParam_d(obj=obj, tau=tau, h=h, nulsic=nulsic, &
    & c=c, val=val, nu=nu, k=k, phi=phi, opt=opt, L0=L0, c4=c4, dt=dt)
CLASS DEFAULT
  CALL Errormsg( &
  & msg="Cannot recognise the type of obj", &
  & file=__FILE__, routine="elemsd_GetDBEStabParam3()", line=__LINE__, &
  & unitno=stderr)
END SELECT
END PROCEDURE elemsd_GetDBEStabParam3

!----------------------------------------------------------------------------
!                                                             GetSUPGParam
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetDBEStabParam4
INTEGER(I4B) :: ii
REAL(DFP), ALLOCATABLE :: ans1(:, :)
REAL(DFP), ALLOCATABLE :: ans2(:, :)
REAL(DFP), ALLOCATABLE :: ans3(:, :)
TYPE(FEVariable_) :: a1, a2, a3
!!
!! main
!!
CALL Reallocate(ans1, SIZE(obj(1)%N, 2), SIZE(obj))
CALL Reallocate(ans2, SIZE(obj(1)%N, 2), SIZE(obj))
CALL Reallocate(ans3, SIZE(obj(1)%N, 2), SIZE(obj))
!!
DO ii = 1, SIZE(obj)
  !!
  CALL elemsd_GetDBEStabParam3( &
    & obj=obj(ii), &
    & tau=a1, &
    & h=a2, &
    & nulsic=a3, &
    & c=c, &
    & val=val, &
    & nu=nu, &
    & k=k, &
    & phi=phi, &
    & opt=opt, &
    & L0=L0, &
    & c4=c4, &
    & dt=dt)
  !!
  ans1(:, ii) = Get(a1, TypeFEVariableScalar, TypeFEVariableSpace)
  ans2(:, ii) = Get(a2, TypeFEVariableScalar, TypeFEVariableSpace)
  ans3(:, ii) = Get(a3, TypeFEVariableScalar, TypeFEVariableSpace)
  !!
END DO
!!
tau = QuadratureVariable(ans1, TypeFEVariableScalar, TypeFEVariableSpaceTime)
h = QuadratureVariable(ans2, TypeFEVariableScalar, TypeFEVariableSpaceTime)
nulsic = QuadratureVariable(ans3, TypeFEVariableScalar,  &
  & TypeFEVariableSpaceTime)
!!
CALL DEALLOCATE (a1)
CALL DEALLOCATE (a2)
CALL DEALLOCATE (a3)
!!
DEALLOCATE (ans1, ans2, ans3)
END PROCEDURE elemsd_GetDBEStabParam4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE DBEStabParamMethods
