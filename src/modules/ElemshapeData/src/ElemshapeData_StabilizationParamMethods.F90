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

MODULE ElemshapeData_StabilizationParamMethods
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: STElemshapeData_
USE BaseType, ONLY: FEVariable_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE
PUBLIC :: GetSUPGParam
PUBLIC :: getSUGN3Param
PUBLIC :: getSUGN3Param_Takizawa2018

!----------------------------------------------------------------------------
!                                                       getSUGN3Param@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the SUGN3 stabilization parameter in space
!
! Computes the SUGN3 stabilization parameter `tau` at the space
! quadrature points of one element.
! The parameter is `tau = h**2 / (4 * nu)`.
! Here `h` is the element length scale along the gradient of `val`.
! The routine first interpolates `nu` to the quadrature points.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSUGN3Param_test_1.F90)
!
! See also: [[elemsd_GetSUGN3Param_2]], [[elemsd_GetSUGN3Param_3]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_1(obj, tau, val, nu, h, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! `tau`, a scalar variable on the space quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, a scalar or a vector variable
    TYPE(FEVariable_), INTENT(IN) :: nu
    !! kinematic viscosity or diffusivity, a scalar variable
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: h
    !! if present, returns `h`, a scalar variable on the space
    !! quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used at present
  END SUBROUTINE elemsd_GetSUGN3Param_1
END INTERFACE

INTERFACE getSUGN3Param
  MODULE PROCEDURE elemsd_GetSUGN3Param_1
END INTERFACE getSUGN3Param

!----------------------------------------------------------------------------
!                                                       getSUGN3Param@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-03
! summary: Returns the SUGN3 stabilization parameter in space-time
!
! Computes the SUGN3 stabilization parameter `tau` at the space-time
! quadrature points of one space-time element.
! The parameter is `tau = h**2 / (4 * nu)`.
! Here `h` is the element length scale along the gradient of `val`.
! The routine first interpolates `nu` to the quadrature points.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSUGN3Param_test_2.F90)
!
! See also: [[elemsd_GetSUGN3Param_1]], [[elemsd_GetSUGN3Param_4]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_2(obj, tau, val, nu, h, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data, one entry for each time quadrature point
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! `tau`, a scalar variable on the space-time quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, a scalar or a vector variable
    TYPE(FEVariable_), INTENT(IN) :: nu
    !! kinematic viscosity or diffusivity, a scalar variable
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: h
    !! if present, returns `h`, a scalar variable on the space-time
    !! quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used at present
  END SUBROUTINE elemsd_GetSUGN3Param_2
END INTERFACE

INTERFACE getSUGN3Param
  MODULE PROCEDURE elemsd_GetSUGN3Param_2
END INTERFACE getSUGN3Param

!----------------------------------------------------------------------------
!                                                       getSUGN3Param@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the SUGN3 stabilization parameter in space
!
! Computes the SUGN3 stabilization parameter `tau` at the space
! quadrature points of one element.
! The parameter is `tau = h**2 / (4 * nu)`.
! Here `h` is the element length scale along the gradient of `val`.
! The diffusivity `nu` is a constant real number.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSUGN3Param_test_1.F90)
!
! See also: [[elemsd_GetSUGN3Param_1]], [[elemsd_GetSUGN3Param_4]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_3(obj, tau, val, nu, h, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! `tau`, a scalar variable on the space quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, a scalar or a vector variable
    REAL(DFP), INTENT(IN) :: nu
    !! kinematic viscosity or diffusivity, a constant
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: h
    !! if present, returns `h`, a scalar variable on the space
    !! quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used at present
  END SUBROUTINE elemsd_GetSUGN3Param_3
END INTERFACE

INTERFACE getSUGN3Param
  MODULE PROCEDURE elemsd_GetSUGN3Param_3
END INTERFACE getSUGN3Param

!----------------------------------------------------------------------------
!                                                       getSUGN3Param@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-03
! summary: Returns the SUGN3 stabilization parameter in space-time
!
! Computes the SUGN3 stabilization parameter `tau` at the space-time
! quadrature points of one space-time element.
! The parameter is `tau = h**2 / (4 * nu)`.
! Here `h` is the element length scale along the gradient of `val`.
! The diffusivity `nu` is a constant real number.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSUGN3Param_test_2.F90)
!
! See also: [[elemsd_GetSUGN3Param_2]], [[elemsd_GetSUGN3Param_3]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_4(obj, tau, val, nu, h, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data, one entry for each time quadrature point
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! `tau`, a scalar variable on the space-time quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, a scalar or a vector variable
    REAL(DFP), INTENT(IN) :: nu
    !! kinematic viscosity or diffusivity, a constant
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: h
    !! if present, returns `h`, a scalar variable on the space-time
    !! quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used at present
  END SUBROUTINE elemsd_GetSUGN3Param_4
END INTERFACE

INTERFACE getSUGN3Param
  MODULE PROCEDURE elemsd_GetSUGN3Param_4
END INTERFACE getSUGN3Param

!----------------------------------------------------------------------------
!                                          getSUGN3Param_Takizawa2018@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the SUGN3 parameter of Takizawa (2018) in space
!
! Computes the SUGN3 stabilization parameter `tau` at the space
! quadrature points of one element, using the definition of Takizawa
! (2018).
!
! The parameter is
! `tau = 1 / (4 * nu * ((1 - r2) / hmin**2 + 1 / h**2))`.
! Here `r2` is the squared length of the unit vector along the gradient
! of `val`. The lengths `h`, `hmin` and `hmax` are element length
! scales. The routine interpolates `nu` to the quadrature points.
!
! If `h` is zero at a point, the routine drops the term `1 / h**2` there.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSUGN3Param_Takizawa2018_test_1.F90)
!
! See also: [[elemsd_GetSUGN3Param_Takizawa2018_2]],
! [[elemsd_GetSUGN3Param_Takizawa2018_3]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_1(obj, tau, val, &
                                                             nu, h, hmax, &
                                                             hmin, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! `tau`, a scalar variable on the space quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, a scalar or a vector variable
    TYPE(FEVariable_), INTENT(IN) :: nu
    !! kinematic viscosity or diffusivity, a scalar variable
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: h
    !! if present, returns `h`, a scalar variable on the space
    !! quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmax
    !! if present, returns `hmax`, a scalar variable on the space
    !! quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmin
    !! if present, returns `hmin`, a scalar variable on the space
    !! quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used at present
  END SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_1
END INTERFACE

INTERFACE getSUGN3Param_Takizawa2018
  MODULE PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_1
END INTERFACE getSUGN3Param_Takizawa2018

!----------------------------------------------------------------------------
!                                          getSUGN3Param_Takizawa2018@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-03
! summary: Returns the SUGN3 parameter of Takizawa (2018) in space-time
!
! Computes the SUGN3 stabilization parameter `tau` at the space-time
! quadrature points of one space-time element.
! It uses the definition of Takizawa (2018).
!
! The parameter is
! `tau = 1 / (4 * nu * ((1 - r2) / hmin**2 + 1 / h**2))`.
! Here `r2` is the squared length of the unit vector along the gradient
! of `val`. The lengths `h`, `hmin` and `hmax` are element length
! scales. The routine interpolates `nu` to the quadrature points.
!
! If `h` is zero at a point, the routine drops the term `1 / h**2` there.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSUGN3Param_Takizawa2018_test_2.F90)
!
! See also: [[elemsd_GetSUGN3Param_Takizawa2018_1]],
! [[elemsd_GetSUGN3Param_Takizawa2018_4]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_2(obj, tau, val, &
                                                             nu, h, hmax, &
                                                             hmin, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data, one entry for each time quadrature point
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! `tau`, a scalar variable on the space-time quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, a scalar or a vector variable
    TYPE(FEVariable_), INTENT(IN) :: nu
    !! kinematic viscosity or diffusivity, a scalar variable
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: h
    !! if present, returns `h`, a scalar variable on the space-time
    !! quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmax
    !! if present, returns `hmax`, a scalar variable on the space-time
    !! quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmin
    !! if present, returns `hmin`, a scalar variable on the space-time
    !! quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used at present
  END SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_2
END INTERFACE

INTERFACE getSUGN3Param_Takizawa2018
  MODULE PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_2
END INTERFACE getSUGN3Param_Takizawa2018

!----------------------------------------------------------------------------
!                                          getSUGN3Param_Takizawa2018@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the SUGN3 parameter of Takizawa (2018) in space
!
! Computes the SUGN3 stabilization parameter `tau` at the space
! quadrature points of one element, using the definition of Takizawa
! (2018). The diffusivity `nu` is a constant real number.
!
! The parameter is
! `tau = 1 / (4 * nu * ((1 - r2) / hmin**2 + 1 / h**2))`.
! Here `r2` is the squared length of the unit vector along the gradient
! of `val`. The lengths `h`, `hmin` and `hmax` are element length
! scales.
!
! If `h` is zero at a point, the routine drops the term `1 / h**2` there.
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSUGN3Param_Takizawa2018_test_1.F90)
!
! See also: [[elemsd_GetSUGN3Param_Takizawa2018_1]],
! [[elemsd_GetSUGN3Param_Takizawa2018_4]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_3(obj, tau, val, &
                                                             nu, h, hmax, &
                                                             hmin, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! `tau`, a scalar variable on the space quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, a scalar or a vector variable
    REAL(DFP), INTENT(IN) :: nu
    !! kinematic viscosity or diffusivity, a constant
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: h
    !! if present, returns `h`, a scalar variable on the space
    !! quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmax
    !! if present, returns `hmax`, a scalar variable on the space
    !! quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmin
    !! if present, returns `hmin`, a scalar variable on the space
    !! quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used at present
  END SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_3
END INTERFACE

INTERFACE getSUGN3Param_Takizawa2018
  MODULE PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_3
END INTERFACE getSUGN3Param_Takizawa2018

!----------------------------------------------------------------------------
!                                          getSUGN3Param_Takizawa2018@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-03
! summary: Returns the SUGN3 parameter of Takizawa (2018) in space-time
!
! Computes the SUGN3 stabilization parameter `tau` at the space-time
! quadrature points of one space-time element.
! It uses the definition of Takizawa (2018).
! The diffusivity `nu` is a constant real number.
!
! The parameter is
! `tau = 1 / (4 * nu * ((1 - r2) / hmin**2 + 1 / h**2))`.
! Here `r2` is the squared length of the unit vector along the gradient
! of `val`. The lengths `h`, `hmin` and `hmax` are element length
! scales.
!
! If `h` is zero at a point, the routine drops the term `1 / h**2` there.
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSUGN3Param_Takizawa2018_test_2.F90)
!
! See also: [[elemsd_GetSUGN3Param_Takizawa2018_2]],
! [[elemsd_GetSUGN3Param_Takizawa2018_3]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_4(obj, tau, val, &
                                                             nu, h, hmax, &
                                                             hmin, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data, one entry for each time quadrature point
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! `tau`, a scalar variable on the space-time quadrature points
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, a scalar or a vector variable
    REAL(DFP), INTENT(IN) :: nu
    !! kinematic viscosity or diffusivity, a constant
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: h
    !! if present, returns `h`, a scalar variable on the space-time
    !! quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmax
    !! if present, returns `hmax`, a scalar variable on the space-time
    !! quadrature points
    TYPE(FEVariable_), OPTIONAL, INTENT(INOUT) :: hmin
    !! if present, returns `hmin`, a scalar variable on the space-time
    !! quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used at present
  END SUBROUTINE elemsd_GetSUGN3Param_Takizawa2018_4
END INTERFACE

INTERFACE getSUGN3Param_Takizawa2018
  MODULE PROCEDURE elemsd_GetSUGN3Param_Takizawa2018_4
END INTERFACE getSUGN3Param_Takizawa2018

!----------------------------------------------------------------------------
!                                                        GetSUPGParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the SUPG stabilization parameter in space
!
! Computes the SUPG stabilization parameter `tau` at the space
! quadrature points of one element.
!
! At each quadrature point, the routine forms four terms:
!
! 1. `t1` is the convection term, built from `c` and the shape
!    function gradients.
! 2. `t2` is the time term, `2 / dt`. It is zero if `dt` is absent or
!    not positive.
! 3. `t3` is the diffusion term, built from `nu` and the shape function
!    gradients along the gradient of `val`.
! 4. `t4` is the Darcy term, `2 * phi * nu / k`.
!
! The terms combine as follows.
!
! - If `opt` is 1, then `tau = SQRT(1 / (t1**2 + t2**2 + t3**2 + t4**2))`.
! - If `opt` is 2, then `tau = 1 / (t1 + t2 + t3 + t4)`.
!
! If `k` is absent, the Darcy term is zero.
!
! @note
! If you pass `k`, you must also pass `phi`.
! @endnote
!
! @note
! If `obj` is an [[STElemshapeData_]], the call uses the space-time
! form of the terms. In that case, the routine ignores `dt`.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSUPGParam_test_1.F90)
!
! See also: [[elemsd_GetSUPGParam2]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUPGParam1(obj, tau, c, val, nu, k, phi, &
                                              dt, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data, or a single [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! `tau`, a scalar variable on the space quadrature points
    TYPE(FEVariable_), INTENT(IN) :: c
    !! convective velocity, a vector variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, a scalar or a vector variable
    TYPE(FEVariable_), INTENT(IN) :: nu
    !! diffusivity, a scalar variable
    !! For the Navier-Stokes equations, use `mu / rho`.
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: k
    !! permeability, a scalar variable
    !! If absent, the Darcy term is zero.
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: phi
    !! porosity, a scalar variable
    !! Required when `k` is present.
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
    !! If absent or not positive, the time term is zero.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! how to combine the terms, 1 or 2
    !! default = 1
  END SUBROUTINE elemsd_GetSUPGParam1
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE elemsd_GetSUPGParam1
END INTERFACE GetSUPGParam

!----------------------------------------------------------------------------
!                                                        GetSUPGParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the SUPG stabilization parameter in space-time
!
! Computes the SUPG stabilization parameter `tau` at the space-time
! quadrature points of one space-time element.
! It evaluates the parameter at each time quadrature point in turn.
!
! At each quadrature point, the routine forms three terms:
!
! 1. `t12` combines convection and the time derivative of the shape
!    functions.
! 2. `t3` is the diffusion term, built from `nu` and the shape function
!    gradients along the gradient of `val`.
! 3. `t4` is the Darcy term, `2 * phi * nu / k`.
!
! The terms combine as follows.
!
! - If `opt` is 1, then `tau = SQRT(1 / (t12**2 + t3**2 + t4**2))`.
! - If `opt` is 2, then `tau = 1 / (t12 + t3 + t4)`.
!
! If `k` is absent, the Darcy term is zero.
!
! @note
! If you pass `k`, you must also pass `phi`.
! @endnote
!
! @note
! The routine ignores `dt`. The time derivative of the shape functions
! already carries the time scale.
! @endnote
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSUPGParam_test_2.F90)
!
! See also: [[elemsd_GetSUPGParam1]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUPGParam2(obj, tau, c, val, nu, k, phi, &
                                              dt, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data, one entry for each time quadrature point
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! `tau`, a scalar variable on the space-time quadrature points
    TYPE(FEVariable_), INTENT(IN) :: c
    !! convective velocity, a vector variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, a scalar or a vector variable
    TYPE(FEVariable_), INTENT(IN) :: nu
    !! kinematic diffusivity, a scalar variable
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: k
    !! permeability, a scalar variable
    !! If absent, the Darcy term is zero.
    TYPE(FEVariable_), OPTIONAL, INTENT(IN) :: phi
    !! porosity, a scalar variable
    !! Required when `k` is present.
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size, not used at present
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! how to combine the terms, 1 or 2
    !! default = 1
  END SUBROUTINE elemsd_GetSUPGParam2
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE elemsd_GetSUPGParam2
END INTERFACE GetSUPGParam

!----------------------------------------------------------------------------
!                                                        GetSUPGParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the SUPG stabilization parameter in space, constant `nu`
!
! Computes the SUPG stabilization parameter `tau` at the space
! quadrature points of one element.
! The diffusivity `nu`, the permeability `k` and the porosity `phi` are
! constant real numbers.
!
! At each quadrature point, the routine forms four terms:
!
! 1. `t1` is the convection term, built from `c` and the shape
!    function gradients.
! 2. `t2` is the time term, `2 / dt`. It is zero if `dt` is absent.
! 3. `t3` is the diffusion term, built from `nu` and the shape function
!    gradients along the gradient of `val`.
! 4. `t4` is the Darcy term, `2 * phi * nu / k`.
!
! The terms combine as follows.
!
! - If `opt` is 1, then `tau = SQRT(1 / (t1**2 + t2**2 + t3**2 + t4**2))`.
! - If `opt` is 2, then `tau = 1 / (t1 + t2 + t3 + t4)`.
!
! If `k` is absent, the Darcy term is zero.
!
! @note
! If you pass `k`, you must also pass `phi`.
! @endnote
!
! @note
! If `obj` is an [[STElemshapeData_]], the routine uses the space-time
! form of the terms and ignores `dt`.
! @endnote
!
! Examples
!
! - [test 1](|base|ElemshapeData/examples/GetSUPGParam_test_1.F90)
! - [test 2](|base|ElemshapeData/examples/GetSUPGParam_test_2.F90) with
!   one space-time shape data
!
! See also: [[elemsd_GetSUPGParam1]], [[elemsd_GetSUPGParam4]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUPGParam3(obj, tau, c, val, nu, k, phi, &
                                              dt, opt)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    !! element shape data, or a single [[STElemshapeData_]]
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! `tau`, a scalar variable on the space quadrature points
    TYPE(FEVariable_), INTENT(IN) :: c
    !! convective velocity, a vector variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, a scalar or a vector variable
    REAL(DFP), INTENT(IN) :: nu
    !! diffusivity, a constant
    !! For the Navier-Stokes equations, use `mu / rho`.
    REAL(DFP), OPTIONAL, INTENT(IN) :: k
    !! permeability, a constant
    !! If absent, the Darcy term is zero.
    REAL(DFP), OPTIONAL, INTENT(IN) :: phi
    !! porosity, a constant
    !! Required when `k` is present.
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size
    !! If absent, the time term is zero.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! how to combine the terms, 1 or 2
    !! default = 1
  END SUBROUTINE elemsd_GetSUPGParam3
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE elemsd_GetSUPGParam3
END INTERFACE GetSUPGParam

!----------------------------------------------------------------------------
!                                                        GetSUPGParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-27
! summary: Returns the SUPG stabilization parameter in space-time,
!   constant `nu`
!
! Computes the SUPG stabilization parameter `tau` at the space-time
! quadrature points of one space-time element.
! It evaluates the parameter at each time quadrature point in turn.
! The diffusivity `nu`, the permeability `k` and the porosity `phi` are
! constant real numbers.
!
! At each quadrature point, the routine forms three terms:
!
! 1. `t12` combines convection and the time derivative of the shape
!    functions.
! 2. `t3` is the diffusion term, built from `nu` and the shape function
!    gradients along the gradient of `val`.
! 3. `t4` is the Darcy term, `2 * phi * nu / k`.
!
! The terms combine as follows.
!
! - If `opt` is 1, then `tau = SQRT(1 / (t12**2 + t3**2 + t4**2))`.
! - If `opt` is 2, then `tau = 1 / (t12 + t3 + t4)`.
!
! If `k` is absent, the Darcy term is zero.
!
! @note
! If you pass `k`, you must also pass `phi`.
! @endnote
!
! @note
! The routine ignores `dt`. The time derivative of the shape functions
! already carries the time scale.
! @endnote
!
! Examples
!
! - [test 2](|base|ElemshapeData/examples/GetSUPGParam_test_2.F90)
!
! See also: [[elemsd_GetSUPGParam2]], [[elemsd_GetSUPGParam3]]

INTERFACE
  MODULE PURE SUBROUTINE elemsd_GetSUPGParam4(obj, tau, c, val, nu, k, phi, &
                                              dt, opt)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    !! space-time shape data, one entry for each time quadrature point
    TYPE(FEVariable_), INTENT(INOUT) :: tau
    !! `tau`, a scalar variable on the space-time quadrature points
    TYPE(FEVariable_), INTENT(IN) :: c
    !! convective velocity, a vector variable
    TYPE(FEVariable_), INTENT(IN) :: val
    !! solution, a scalar or a vector variable
    REAL(DFP), INTENT(IN) :: nu
    !! kinematic diffusivity, a constant
    REAL(DFP), OPTIONAL, INTENT(IN) :: k
    !! permeability, a constant
    !! If absent, the Darcy term is zero.
    REAL(DFP), OPTIONAL, INTENT(IN) :: phi
    !! porosity, a constant
    !! Required when `k` is present.
    REAL(DFP), OPTIONAL, INTENT(IN) :: dt
    !! time step size, not used at present
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! how to combine the terms, 1 or 2
    !! default = 1
  END SUBROUTINE elemsd_GetSUPGParam4
END INTERFACE

INTERFACE GetSUPGParam
  MODULE PROCEDURE elemsd_GetSUPGParam4
END INTERFACE GetSUPGParam

END MODULE ElemshapeData_StabilizationParamMethods
