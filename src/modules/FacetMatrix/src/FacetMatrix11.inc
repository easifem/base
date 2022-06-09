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

PUBLIC :: FacetMatrix11

!----------------------------------------------------------------------------
!                                                              FacetMatrix11
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}}[[{\bf d}\left(\delta\bar{{\bf v}}\right)
! \cdot{\bf n}]]\cdot[[{\bf d}\left(\bar{{\bf v}}\right)\cdot{\bf n}]]dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix11_1( masterElemSD, slaveElemSD, quadMap, &
  & nCopy ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: masterElemSD
  CLASS( ElemshapeData_ ), INTENT( IN ) :: slaveElemSD
  INTEGER( I4B ), INTENT( IN ) :: quadMap( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nCopy
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix11_1
END INTERFACE

INTERFACE FacetMatrix11
  MODULE PROCEDURE FacetMatrix11_1
END INTERFACE FacetMatrix11

!----------------------------------------------------------------------------
!                                                              FacetMatrix11
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}}[[2\mu{\bf d}\left(\delta\bar{{\bf v}}\right)
! \cdot{\bf n}]]\cdot[[2\mu{\bf d}\left(\bar{{\bf v}}\right)\cdot{\bf n}]]dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix11_2( masterElemSD, slaveElemSD, &
  & muMaster, muSlave, quadMap, nCopy ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: masterElemSD
  CLASS( ElemshapeData_ ), INTENT( IN ) :: slaveElemSD
  REAL( DFP ), INTENT( IN ) :: muMaster
  REAL( DFP ), INTENT( IN ) :: muSlave
  INTEGER( I4B ), INTENT( IN ) :: quadMap( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nCopy
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix11_2
END INTERFACE

INTERFACE FacetMatrix11
  MODULE PROCEDURE FacetMatrix11_2
END INTERFACE FacetMatrix11

!----------------------------------------------------------------------------
!                                                              FacetMatrix11
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}}\tau_{E1}[[2\mu{\bf d}\left(\delta\bar{{\bf v}}\right)
! \cdot{\bf n}]]\cdot[[2\mu{\bf d}\left(\bar{{\bf v}}\right)\cdot{\bf n}]]dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix11_3( masterElemSD, slaveElemSD, &
  & muMaster, muSlave, tauvar, quadMap, nCopy ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: masterElemSD
  CLASS( ElemshapeData_ ), INTENT( IN ) :: slaveElemSD
  REAL( DFP ), INTENT( IN ) :: muMaster
  REAL( DFP ), INTENT( IN ) :: muSlave
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  INTEGER( I4B ), INTENT( IN ) :: quadMap( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nCopy
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix11_3
END INTERFACE

INTERFACE FacetMatrix11
  MODULE PROCEDURE FacetMatrix11_3
END INTERFACE FacetMatrix11

!----------------------------------------------------------------------------
!                                                              FacetMatrix11
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}}[[2\mu{\bf d}\left(\delta\bar{{\bf v}}\right)
! \cdot{\bf n}]]\cdot[[2\mu{\bf d}\left(\bar{{\bf v}}\right)\cdot{\bf n}]]dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix11_4( masterElemSD, slaveElemSD, &
  & muMaster, muSlave, quadMap, nCopy ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: masterElemSD
  CLASS( ElemshapeData_ ), INTENT( IN ) :: slaveElemSD
  TYPE( FEVariable_ ), INTENT( IN ) :: muMaster
  TYPE( FEVariable_ ), INTENT( IN ) :: muSlave
  INTEGER( I4B ), INTENT( IN ) :: quadMap( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nCopy
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix11_4
END INTERFACE

INTERFACE FacetMatrix11
  MODULE PROCEDURE FacetMatrix11_4
END INTERFACE FacetMatrix11

!----------------------------------------------------------------------------
!                                                              FacetMatrix11
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: FacetMatrix  for VMS-FEM for CFD
!
!# Introduction
!
! $$
! \int_{\Gamma_{e}}\tau_{E1}[[2\mu{\bf d}\left(\delta\bar{{\bf v}}\right)
! \cdot{\bf n}]]\cdot[[2\mu{\bf d}\left(\bar{{\bf v}}\right)\cdot{\bf n}]]dS
! $$

INTERFACE
MODULE PURE FUNCTION FacetMatrix11_5( masterElemSD, slaveElemSD, &
  & muMaster, muSlave, tauvar, quadMap, nCopy ) RESULT( ans )
  CLASS( ElemshapeData_ ), INTENT( IN ) :: masterElemSD
  CLASS( ElemshapeData_ ), INTENT( IN ) :: slaveElemSD
  TYPE( FEVariable_ ), INTENT( IN ) :: muMaster
  TYPE( FEVariable_ ), INTENT( IN ) :: muSlave
  TYPE( FEVariable_ ), INTENT( IN ) :: tauvar
  INTEGER( I4B ), INTENT( IN ) :: quadMap( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nCopy
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION FacetMatrix11_5
END INTERFACE

INTERFACE FacetMatrix11
  MODULE PROCEDURE FacetMatrix11_5
END INTERFACE FacetMatrix11
