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

PUBLIC :: FacetMatrix6

!----------------------------------------------------------------------------
!                                                              FacetMatrix6
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}}\frac{\partial\delta{\bf \bar{v}}}{\partial{\bf x}}\cdot
! {\bf n}\cdot\frac{\partial\delta{\bf \bar{v}}}{\partial{\bf x}}\cdot{\bf n}
! dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix6_1( elemsd ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix6_1
END INTERFACE

INTERFACE FacetMatrix6
  MODULE PROCEDURE FacetMatrix6_1
END INTERFACE FacetMatrix6

!----------------------------------------------------------------------------
!                                                              FacetMatrix6
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}}\frac{\partial\delta{\bf \bar{v}}}{\partial{\bf x}}\cdot
! {\bf n}\cdot\frac{\partial\delta{\bf \bar{v}}}{\partial{\bf x}}\cdot{\bf n}
! dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix6_2( elemsd, mu ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix6_2
END INTERFACE

INTERFACE FacetMatrix6
  MODULE PROCEDURE FacetMatrix6_2
END INTERFACE FacetMatrix6

!----------------------------------------------------------------------------
!                                                              FacetMatrix6
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}}\frac{\partial\delta{\bf \bar{v}}}{\partial{\bf x}}\cdot
! {\bf n}\cdot\frac{\partial\delta{\bf \bar{v}}}{\partial{\bf x}}\cdot{\bf n}
! dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix6_3( elemsd, mu, tauvar ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix6_3
END INTERFACE

INTERFACE FacetMatrix6
  MODULE PROCEDURE FacetMatrix6_3
END INTERFACE FacetMatrix6

!----------------------------------------------------------------------------
!                                                              FacetMatrix6
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}}\frac{\partial\delta{\bf \bar{v}}}{\partial{\bf x}}\cdot
! {\bf n}\cdot\frac{\partial\delta{\bf \bar{v}}}{\partial{\bf x}}\cdot{\bf n}
! dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix6_4( elemsd, mu ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix6_4
END INTERFACE

INTERFACE FacetMatrix6
  MODULE PROCEDURE FacetMatrix6_4
END INTERFACE FacetMatrix6

!----------------------------------------------------------------------------
!                                                              FacetMatrix6
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}}\frac{\partial\delta{\bf \bar{v}}}{\partial{\bf x}}\cdot
! {\bf n}\cdot\frac{\partial\delta{\bf \bar{v}}}{\partial{\bf x}}\cdot{\bf n}
! dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix6_5( elemsd, mu, tauvar ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix6_5
END INTERFACE

INTERFACE FacetMatrix6
  MODULE PROCEDURE FacetMatrix6_5
END INTERFACE FacetMatrix6
