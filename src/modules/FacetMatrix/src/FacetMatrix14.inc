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

PUBLIC :: FacetMatrix14

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \begin{aligned}\int_{\Gamma_{e}}\tau\left(\mu\nabla\delta{\bf v}\cdot{\bf n}
! \right)\cdot\left(\bar{p}{\bf n}\right)dS & =\int_{\Gamma_{e}}\tau\mu\frac
! {\partial\delta v_{i}}{\partial x_{p}}n_{p}n_{i}\bar{p}dS\\ & =\int_{\Gamma_
! {e}}\tau\mu\frac{\partial\delta v_{iI}N^{I}}{\partial x_{p}}n_{p}n_{i}\bar
! {p}_{J}N^{J}dS\\ & =\delta v_{iI}\left[\int_{\Gamma_{e}}\tau\mu\frac
! {\partial N^{I}}{\partial x_{p}}n_{p}n_{i}N^{J}dS\right]\bar{p}_{J} \end
! {aligned}
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix14_1( elemsd, opt ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
  INTEGER( I4B ), INTENT( IN ) :: opt
END FUNCTION FacetMatrix14_1
END INTERFACE

INTERFACE FacetMatrix14
  MODULE PROCEDURE FacetMatrix14_1
END INTERFACE FacetMatrix14

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \begin{aligned}\int_{\Gamma_{e}}\tau\left(\mu\nabla\delta{\bf v}\cdot{\bf n}
! \right)\cdot\left(\bar{p}{\bf n}\right)dS & =\int_{\Gamma_{e}}\tau\mu\frac
! {\partial\delta v_{i}}{\partial x_{p}}n_{p}n_{i}\bar{p}dS\\ & =\int_{\Gamma_
! {e}}\tau\mu\frac{\partial\delta v_{iI}N^{I}}{\partial x_{p}}n_{p}n_{i}\bar
! {p}_{J}N^{J}dS\\ & =\delta v_{iI}\left[\int_{\Gamma_{e}}\tau\mu\frac
! {\partial N^{I}}{\partial x_{p}}n_{p}n_{i}N^{J}dS\right]\bar{p}_{J} \end
! {aligned}
! $$


INTERFACE
MODULE PURE FUNCTION FacetMatrix14_2( elemsd, mu, opt ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
  INTEGER( I4B ), INTENT( IN ) :: opt
END FUNCTION FacetMatrix14_2
END INTERFACE

INTERFACE FacetMatrix14
  MODULE PROCEDURE FacetMatrix14_2
END INTERFACE FacetMatrix14

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \begin{aligned}\int_{\Gamma_{e}}\tau\left(\mu\nabla\delta{\bf v}\cdot{\bf n}
! \right)\cdot\left(\bar{p}{\bf n}\right)dS & =\int_{\Gamma_{e}}\tau\mu\frac
! {\partial\delta v_{i}}{\partial x_{p}}n_{p}n_{i}\bar{p}dS\\ & =\int_{\Gamma_
! {e}}\tau\mu\frac{\partial\delta v_{iI}N^{I}}{\partial x_{p}}n_{p}n_{i}\bar
! {p}_{J}N^{J}dS\\ & =\delta v_{iI}\left[\int_{\Gamma_{e}}\tau\mu\frac
! {\partial N^{I}}{\partial x_{p}}n_{p}n_{i}N^{J}dS\right]\bar{p}_{J} \end
! {aligned}
! $$


INTERFACE
MODULE PURE FUNCTION FacetMatrix14_3( elemsd, mu, tauvar, opt ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
  INTEGER( I4B ), INTENT( IN ) :: opt
END FUNCTION FacetMatrix14_3
END INTERFACE

INTERFACE FacetMatrix14
  MODULE PROCEDURE FacetMatrix14_3
END INTERFACE FacetMatrix14

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \begin{aligned}\int_{\Gamma_{e}}\tau\left(\mu\nabla\delta{\bf v}\cdot{\bf n}
! \right)\cdot\left(\bar{p}{\bf n}\right)dS & =\int_{\Gamma_{e}}\tau\mu\frac
! {\partial\delta v_{i}}{\partial x_{p}}n_{p}n_{i}\bar{p}dS\\ & =\int_{\Gamma_
! {e}}\tau\mu\frac{\partial\delta v_{iI}N^{I}}{\partial x_{p}}n_{p}n_{i}\bar
! {p}_{J}N^{J}dS\\ & =\delta v_{iI}\left[\int_{\Gamma_{e}}\tau\mu\frac
! {\partial N^{I}}{\partial x_{p}}n_{p}n_{i}N^{J}dS\right]\bar{p}_{J} \end
! {aligned}
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix14_4( elemsd, mu, opt ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  INTEGER( I4B ), INTENT( IN ) :: opt
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix14_4
END INTERFACE

INTERFACE FacetMatrix14
  MODULE PROCEDURE FacetMatrix14_4
END INTERFACE FacetMatrix14

!----------------------------------------------------------------------------
!                                                              FacetMatrix14
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \begin{aligned}\int_{\Gamma_{e}}\tau\left(\mu\nabla\delta{\bf v}\cdot{\bf n}
! \right)\cdot\left(\bar{p}{\bf n}\right)dS & =\int_{\Gamma_{e}}\tau\mu\frac
! {\partial\delta v_{i}}{\partial x_{p}}n_{p}n_{i}\bar{p}dS\\ & =\int_{\Gamma_
! {e}}\tau\mu\frac{\partial\delta v_{iI}N^{I}}{\partial x_{p}}n_{p}n_{i}\bar
! {p}_{J}N^{J}dS\\ & =\delta v_{iI}\left[\int_{\Gamma_{e}}\tau\mu\frac
! {\partial N^{I}}{\partial x_{p}}n_{p}n_{i}N^{J}dS\right]\bar{p}_{J} \end
! {aligned}
! $$


INTERFACE
MODULE PURE FUNCTION FacetMatrix14_5( elemsd, mu, tauvar, opt ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  INTEGER( I4B ), INTENT( IN ) :: opt
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix14_5
END INTERFACE

INTERFACE FacetMatrix14
  MODULE PROCEDURE FacetMatrix14_5
END INTERFACE FacetMatrix14
