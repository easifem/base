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

PUBLIC :: FacetMatrix22

!----------------------------------------------------------------------------
!                                                              FacetMatrix22
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \begin{aligned}\int_{\Gamma_{e}}{\bf n}\cdot\nabla\delta p\tau pdS &
! =\delta p_{I}\left(\int_{\Gamma_{e}}\tau{\bf n}\cdot\nabla N^{I}N^{J}
! dS\right)p_{J}\end{aligned}
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix22_1( elemsd ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix22_1
END INTERFACE

INTERFACE FacetMatrix22
  MODULE PROCEDURE FacetMatrix22_1
END INTERFACE FacetMatrix22

!----------------------------------------------------------------------------
!                                                              FacetMatrix22
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \begin{aligned}\int_{\Gamma_{e}}{\bf n}\cdot\nabla\delta p\tau pdS &
! =\delta p_{I}\left(\int_{\Gamma_{e}}\tau{\bf n}\cdot\nabla N^{I}N^{J}
! dS\right)p_{J}\end{aligned}
! $$


INTERFACE
MODULE PURE FUNCTION FacetMatrix22_2( elemsd, tauvar ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix22_2
END INTERFACE

INTERFACE FacetMatrix22
  MODULE PROCEDURE FacetMatrix22_2
END INTERFACE FacetMatrix22

!----------------------------------------------------------------------------
!                                                              FacetMatrix22
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \begin{aligned}\int_{\Gamma_{e}}{\bf n}\cdot\nabla\delta p\tau pdS &
! =\delta p_{I}\left(\int_{\Gamma_{e}}\tau{\bf n}\cdot\nabla N^{I}N^{J}
! dS\right)p_{J}\end{aligned}
! $$


INTERFACE
MODULE PURE FUNCTION FacetMatrix22_3( elemsd, tauvar ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix22_3
END INTERFACE

INTERFACE FacetMatrix22
  MODULE PROCEDURE FacetMatrix22_3
END INTERFACE FacetMatrix22
