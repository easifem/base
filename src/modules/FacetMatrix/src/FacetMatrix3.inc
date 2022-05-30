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

PUBLIC :: FacetMatrix3

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}\bigcap\Gamma^{h}}\tau_{E1}G_{12}(I,j,i)n_{j}N^{J}dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix3_1( elemsd, opt ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
  INTEGER( I4B ), INTENT( IN ) :: opt
END FUNCTION FacetMatrix3_1
END INTERFACE

INTERFACE FacetMatrix3
  MODULE PROCEDURE FacetMatrix3_1
END INTERFACE FacetMatrix3

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}\bigcap\Gamma^{h}}\tau_{E1}G_{12}(I,j,i)n_{j}N^{J}dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix3_2( elemsd, mu, opt ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
  INTEGER( I4B ), INTENT( IN ) :: opt
END FUNCTION FacetMatrix3_2
END INTERFACE

INTERFACE FacetMatrix3
  MODULE PROCEDURE FacetMatrix3_2
END INTERFACE FacetMatrix3

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}\bigcap\Gamma^{h}}\tau_{E1}G_{12}(I,j,i)n_{j}N^{J}dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix3_3( elemsd, mu, tauvar, opt ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
  INTEGER( I4B ), INTENT( IN ) :: opt
END FUNCTION FacetMatrix3_3
END INTERFACE

INTERFACE FacetMatrix3
  MODULE PROCEDURE FacetMatrix3_3
END INTERFACE FacetMatrix3

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}\bigcap\Gamma^{h}}\tau_{E1}G_{12}(I,j,i)n_{j}N^{J}dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix3_4( elemsd, mu, opt ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  INTEGER( I4B ), INTENT( IN ) :: opt
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix3_4
END INTERFACE

INTERFACE FacetMatrix3
  MODULE PROCEDURE FacetMatrix3_4
END INTERFACE FacetMatrix3

!----------------------------------------------------------------------------
!                                                              FacetMatrix3
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}\bigcap\Gamma^{h}}\tau_{E1}G_{12}(I,j,i)n_{j}N^{J}dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix3_5( elemsd, mu, tauvar, opt ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  INTEGER( I4B ), INTENT( IN ) :: opt
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix3_5
END INTERFACE

INTERFACE FacetMatrix3
  MODULE PROCEDURE FacetMatrix3_5
END INTERFACE FacetMatrix3
