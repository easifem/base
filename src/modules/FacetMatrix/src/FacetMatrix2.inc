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

PUBLIC :: FacetMatrix2

!----------------------------------------------------------------------------
!                                                              FacetMatrix2
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}\bigcap\Gamma^{h}}\tau_{E1}\left(2\mu{\bf d}\left(\delta\bar
! {{\bf v}}\right)\cdot{\bf n}\right)\cdot\left(2\mu{\bf d}\left(\bar{{\bf v}}
! \right)\cdot{\bf n}\right)dS&=\delta V_{iI}\left[\int_{\Gamma_{e}
! \bigcap\Gamma^{h}}\tau_{E1}G_{12}^{Ipi}G_{12}^{Jpj}dS\right]V_{jJ}
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix2_1( elemsd ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix2_1
END INTERFACE

INTERFACE FacetMatrix2
  MODULE PROCEDURE FacetMatrix2_1
END INTERFACE FacetMatrix2

!----------------------------------------------------------------------------
!                                                              FacetMatrix2
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}\bigcap\Gamma^{h}}\tau_{E1}\left(2\mu{\bf d}\left(\delta\bar
! {{\bf v}}\right)\cdot{\bf n}\right)\cdot\left(2\mu{\bf d}\left(\bar{{\bf v}}
! \right)\cdot{\bf n}\right)dS&=\delta V_{iI}\left[\int_{\Gamma_{e}
! \bigcap\Gamma^{h}}\tau_{E1}G_{12}^{Ipi}G_{12}^{Jpj}dS\right]V_{jJ}
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix2_2( elemsd, mu ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix2_2
END INTERFACE

INTERFACE FacetMatrix2
  MODULE PROCEDURE FacetMatrix2_2
END INTERFACE FacetMatrix2

!----------------------------------------------------------------------------
!                                                              FacetMatrix2
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}\bigcap\Gamma^{h}}\tau_{E1}\left(2\mu{\bf d}\left(\delta\bar
! {{\bf v}}\right)\cdot{\bf n}\right)\cdot\left(2\mu{\bf d}\left(\bar{{\bf v}}
! \right)\cdot{\bf n}\right)dS&=\delta V_{iI}\left[\int_{\Gamma_{e}
! \bigcap\Gamma^{h}}\tau_{E1}G_{12}^{Ipi}G_{12}^{Jpj}dS\right]V_{jJ}
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix2_3( elemsd, mu, tauvar ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  REAL( DFP ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix2_3
END INTERFACE

INTERFACE FacetMatrix2
  MODULE PROCEDURE FacetMatrix2_3
END INTERFACE FacetMatrix2

!----------------------------------------------------------------------------
!                                                              FacetMatrix2
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}\bigcap\Gamma^{h}}\tau_{E1}\left(2\mu{\bf d}\left(\delta\bar
! {{\bf v}}\right)\cdot{\bf n}\right)\cdot\left(2\mu{\bf d}\left(\bar{{\bf v}}
! \right)\cdot{\bf n}\right)dS&=\delta V_{iI}\left[\int_{\Gamma_{e}
! \bigcap\Gamma^{h}}\tau_{E1}G_{12}^{Ipi}G_{12}^{Jpj}dS\right]V_{jJ}
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix2_4( elemsd, mu ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix2_4
END INTERFACE

INTERFACE FacetMatrix2
  MODULE PROCEDURE FacetMatrix2_4
END INTERFACE FacetMatrix2

!----------------------------------------------------------------------------
!                                                              FacetMatrix2
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}\bigcap\Gamma^{h}}\tau_{E1}\left(2\mu{\bf d}\left(\delta\bar
! {{\bf v}}\right)\cdot{\bf n}\right)\cdot\left(2\mu{\bf d}\left(\bar{{\bf v}}
! \right)\cdot{\bf n}\right)dS&=\delta V_{iI}\left[\int_{\Gamma_{e}
! \bigcap\Gamma^{h}}\tau_{E1}G_{12}^{Ipi}G_{12}^{Jpj}dS\right]V_{jJ}
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix2_5( elemsd, mu, tauvar ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: elemsd
  TYPE( FEVariable_ ), INTENT( IN ) :: mu
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix2_5
END INTERFACE

INTERFACE FacetMatrix2
  MODULE PROCEDURE FacetMatrix2_5
END INTERFACE FacetMatrix2
