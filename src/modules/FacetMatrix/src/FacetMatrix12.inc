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

PUBLIC :: FacetMatrix12

!----------------------------------------------------------------------------
!                                                              FacetMatrix12
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
MODULE PURE FUNCTION FacetMatrix12_1( elemsd, nCopy ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nCopy
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix12_1
END INTERFACE

INTERFACE FacetMatrix12
  MODULE PROCEDURE FacetMatrix12_1
END INTERFACE FacetMatrix12

!----------------------------------------------------------------------------
!                                                              FacetMatrix12
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
MODULE PURE FUNCTION FacetMatrix12_2( elemsd, mu, nCopy ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nCopy
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix12_2
END INTERFACE

INTERFACE FacetMatrix12
  MODULE PROCEDURE FacetMatrix12_2
END INTERFACE FacetMatrix12

!----------------------------------------------------------------------------
!                                                              FacetMatrix12
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
MODULE PURE FUNCTION FacetMatrix12_3( elemsd, mu, tauvar, nCopy ) &
  & RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nCopy
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix12_3
END INTERFACE

INTERFACE FacetMatrix12
  MODULE PROCEDURE FacetMatrix12_3
END INTERFACE FacetMatrix12

!----------------------------------------------------------------------------
!                                                              FacetMatrix12
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
MODULE PURE FUNCTION FacetMatrix12_4( elemsd, mu, nCopy ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nCopy
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix12_4
END INTERFACE

INTERFACE FacetMatrix12
  MODULE PROCEDURE FacetMatrix12_4
END INTERFACE FacetMatrix12

!----------------------------------------------------------------------------
!                                                              FacetMatrix12
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
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
MODULE PURE FUNCTION FacetMatrix12_5( elemsd, mu, tauvar, nCopy ) &
  & RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nCopy
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix12_5
END INTERFACE

INTERFACE FacetMatrix12
  MODULE PROCEDURE FacetMatrix12_5
END INTERFACE FacetMatrix12
