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

MODULE MetisInterface
USE GlobalData
USE ISO_C_BINDING
USE CInterface
USE ErrorHandling
IMPLICIT NONE
PRIVATE
PUBLIC :: MetisSetDefaultOptions
PUBLIC :: MetisNodeND
PUBLIC :: METISPartGraphRecursive
PUBLIC :: METISPartGraphKway
PUBLIC :: METISPartMeshDual
PUBLIC :: METISPartMeshNodal
PUBLIC :: METISMeshToDual
PUBLIC :: METISMeshToNodal
#include "./MetisInterface.inc"
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE MetisSetDefaultOptions(Options)
  INTEGER(I4B), INTENT(OUT) :: Options(:)
  INTEGER(I4B) :: IERR
  IERR = METIS_SetDefaultOptions(Options)
  IF (IERR .NE. METIS_OK) THEN
    CALL ErrorMSG( &
      & Msg="Error while setting default options", &
      & File="MetisInterface.F90", &
      & Routine="MetisSetDefaultOptions()", &
      & Line=__LINE__, &
      & UnitNo=stdout)
    STOP
  END IF
END SUBROUTINE MetisSetDefaultOptions

!----------------------------------------------------------------------------
!                                                              MetisNodeND
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Jul 2021
! summary: This function computes fill reducing orderings of sparse matrices using the multilevel nested dissection algorithm.
!
!# Introduction
!
! This function computes fill reducing orderings of sparse matrices using the multilevel nested dissection algorithm.
!
! - Let $A$ be the original matrix and $A*$ be the permuted matrix.
! - The arrays `perm` and `iperm` are defined as follows.
! - Row (column) `i` of $A*$ is the `perm(i)` row (column) of $A$
! - Row (column) `i` of $A$ is the `iperm(i)` row (column) of $A*$.
! - The numbering of this vector starts from either 0 or 1, depending on the value of `options(METIS_OPTION_NUMBERING)`.
!
! If the graph is weighted, meaning `vgwt` was provided, the nested dissection ordering computes vertex separators that minimize the sum of the weights of the vertices on the separators.
!
! GRAPH DATA STRUCTURE
!
! The adjacency structure of the graph is stored using the compressed storage format (CSR). The CSR format is a widely used scheme for storing sparse graphs. In this format the adjacency structure of a graph with n vertices and m edges is represented using two arrays xadj and adjncy. The xadj array is of size n + 1 whereas the adjncy array is of size 2m (this is because for each edge between vertices v and u we actually store both (v; u) and (u; v))
!
! The adjacency structure of the graph is stored as follows. Assuming that vertex numbering starts from 0 (C style), then the adjacency list of vertex i is stored in array adjncy starting at index xadj[i] and ending at (but not including) index xadj[i + 1] (i.e., adjncy[xadj[i]] through and including adjncy[xadj[i + 1]-1]). That is, for each vertex i, its adjacency list is stored in consecutive locations in the array adjncy, and the array xadj is used to point to where it begins and where it ends. Figure 3(b) illustrates the CSR format for the 15-vertex graph shown in Figure 3(a)
!
! The weights of the vertices (if any) are stored in an additional array called vwgt. If ncon is the number of weights associated with each vertex, the array vwgt contains n ∗ ncon elements (recall that n is the number of vertices). The weights of the ith vertex are stored in ncon consecutive entries starting at location vwgt[i ∗ ncon]. Note that if each vertex has only a single weight, then vwgt will contain n elements, and vwgt[i] will store the weight of the ith vertex. The vertex-weights must be integers greater or equal to zero. If all the vertices of the graph have the same weight (i.e., the graph is unweighted), then the vwgt can be set to NULL.
!
!
!### Usage
!
!```fortran
! INTEGER( I4B ), PARAMETER :: n = 15
!   !! number of vertices
! INTEGER( I4B ), PARAMETER :: m = 22
!   !! number of edges
! INTEGER( I4B ) :: XADJ(n+1), ADJNCY(2*m)
!   !! graph adjacency structure
!   !! XADJ, ADJNCY
! INTEGER( I4B ) :: PERM(n), IPERM(n)
!   !! fill-reducing permutation andinverse permutatio
! CALL Display( "TESTING METISNodeND" )
! XADJ = [1,3,6,9,12,14,17,21,25,29,32,34,37,40,43,45]
! ADJNCY = [2,6,1,3,7,2,4,8,3,5,9,4,10,1,7,11,2,6, &
!   & 8,12,3,7,9,13,4,8,10,14,5,9,15,6,12,7,11,13, &
!   & 8,12,14,9,13,15,10,14]
! CALL METISNodeND(XADJ=XADJ, ADJNCY=ADJNCY, PERM=PERM, IPERM=IPERM )
! CALL DISP( x=PERM,  title= " PERM = " )
! CALL DISP( x=IPERM,  title= " IPERM = " )
!```

SUBROUTINE MetisNodeND(XADJ, ADJNCY, PERM, IPERM, OPTIONS, VWGT)
  INTEGER(I4B), INTENT(IN) :: XADJ(:)
  INTEGER(I4B), INTENT(IN) :: ADJNCY(:)
  INTEGER(I4B), INTENT(OUT) :: PERM(:)
  INTEGER(I4B), INTENT(OUT) :: IPERM(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: OPTIONS(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: VWGT(:)
  ! Internal variables
  INTEGER(I4B) :: NVTXS, IERR
  INTEGER(I4B), TARGET :: OPT(0:MAX_METIS_OPTIONS - 1)
  TYPE(C_PTR) :: C_OPTIONS, C_VWGT
  !
  NVTXS = SIZE(PERM)
  IF (PRESENT(OPTIONS)) THEN
    C_OPTIONS = C_LOC(OPTIONS)
  ELSE
    IERR = METIS_SetDefaultOptions(OPT)
    IF (IERR .NE. METIS_OK) THEN
      CALL ErrorMSG( &
        & Msg="Error while setting default options", &
        & File="MetisInterface.F90", &
        & Routine="MetisNodeND()", &
        & Line=__LINE__, &
        & UnitNo=stdout)
      STOP
    END IF
    OPT(METIS_OPTION_NUMBERING) = METIS_NUMBERING_DEFAULT
    C_OPTIONS = C_LOC(OPT)
  END IF

  IF (PRESENT(VWGT)) THEN
    C_VWGT = C_LOC(VWGT)
  ELSE
    C_VWGT = C_NULL_PTR
  END IF

  IERR = METIS_NodeND(nvtxs=NVTXS, xadj=XADJ, adjncy=ADJNCY,&
    & perm=PERM, iperm=IPERM, options=C_OPTIONS, vwgt=C_VWGT)
  IF (ierr .NE. METIS_OK) THEN
    CALL ErrorMSG( &
        & Msg="Error in METIS_NodeND()", &
        & File="MetisInterface.F90", &
        & Routine="MetisNodeND()", &
        & Line=__LINE__, &
        & UnitNo=stdout)
    STOP
  END IF

  C_OPTIONS = C_NULL_PTR
  C_VWGT = C_NULL_PTR
END SUBROUTINE MetisNodeND

!----------------------------------------------------------------------------
!                                                   METISPartGraphRecursive
!----------------------------------------------------------------------------

SUBROUTINE METISPartGraphRecursive(NCON, NPARTS, OBJVAL, PART, XADJ, &
  & ADJNCY, OPTIONS, VWGT, VSIZE, ADJWGT, TPWGTS, UBVEC)
  INTEGER(I4B), INTENT(IN) :: NCON
  INTEGER(I4B), INTENT(IN) :: NPARTS
  INTEGER(I4B), INTENT(OUT) :: OBJVAL
  INTEGER(I4B), INTENT(OUT) :: PART(:)
  INTEGER(I4B), INTENT(IN) :: XADJ(:)
  INTEGER(I4B), INTENT(IN) :: ADJNCY(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: OPTIONS(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: VWGT(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: VSIZE(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: ADJWGT(:)
  REAL(DFP), OPTIONAL, TARGET, INTENT(IN) :: TPWGTS(:)
  REAL(DFP), OPTIONAL, TARGET, INTENT(IN) :: UBVEC(:)
  ! Internal variables
  INTEGER(I4B) :: NVTXS, IERR
  INTEGER(I4B), TARGET :: OPT(0:MAX_METIS_OPTIONS - 1)
  TYPE(C_PTR) :: C_OPTIONS, C_VWGT, C_VSIZE, C_ADJWGT, C_TPWGTS, C_UBVEC
  !
  NVTXS = SIZE(PART)

  IF (PRESENT(OPTIONS)) THEN
    C_OPTIONS = C_LOC(OPTIONS)
  ELSE
    IERR = METIS_SetDefaultOptions(OPT)
    IF (IERR .NE. METIS_OK) THEN
      CALL ErrorMSG( &
        & Msg="Error while setting default options", &
        & File="MetisInterface.F90", &
        & Routine="METISPartGraphRecursive()", &
        & Line=__LINE__, &
        & UnitNo=stdout)
      STOP
    END IF
    OPT(METIS_OPTION_NUMBERING) = METIS_NUMBERING_DEFAULT
    C_OPTIONS = C_LOC(OPT)
  END IF

  IF (PRESENT(VWGT)) THEN
    C_VWGT = C_LOC(VWGT)
  ELSE
    C_VWGT = C_NULL_PTR
  END IF

  IF (PRESENT(VSIZE)) THEN
    C_VSIZE = C_LOC(VSIZE)
  ELSE
    C_VSIZE = C_NULL_PTR
  END IF

  IF (PRESENT(ADJWGT)) THEN
    C_ADJWGT = C_LOC(ADJWGT)
  ELSE
    C_ADJWGT = C_NULL_PTR
  END IF

  IF (PRESENT(TPWGTS)) THEN
    C_TPWGTS = C_LOC(TPWGTS)
  ELSE
    C_TPWGTS = C_NULL_PTR
  END IF

  IF (PRESENT(UBVEC)) THEN
    C_UBVEC = C_LOC(UBVEC)
  ELSE
    C_UBVEC = C_NULL_PTR
  END IF

  IERR = METIS_PartGraphRecursive(NCON=NCON, NVTXS=NVTXS, XADJ=XADJ,&
    & ADJNCY=ADJNCY, NPARTS=NPARTS, OBJVAL=OBJVAL, PART=PART, &
    & VWGT=C_VWGT, VSIZE=C_VSIZE, ADJWGT=C_ADJWGT, &
    & TPWGTS=C_TPWGTS, UBVEC=C_UBVEC, OPTIONS=C_OPTIONS)

  IF (IERR .NE. METIS_OK) THEN
    CALL ErrorMSG( &
      & Msg="Error in METIS_PartGraphRecursive()", &
      & File="MetisInterface.F90", &
      & Routine="METISPartGraphRecursive()", &
      & Line=__LINE__, &
      & UnitNo=stdout)
    STOP
  END IF

  C_VWGT = C_NULL_PTR
  C_VSIZE = C_NULL_PTR
  C_ADJWGT = C_NULL_PTR
  C_TPWGTS = C_NULL_PTR
  C_UBVEC = C_NULL_PTR
  C_OPTIONS = C_NULL_PTR
END SUBROUTINE METISPartGraphRecursive

!----------------------------------------------------------------------------
!                                                   METISPartGraphKway
!----------------------------------------------------------------------------

SUBROUTINE METISPartGraphKway(NCON, NPARTS, OBJVAL, PART, XADJ, &
  & ADJNCY, OPTIONS, VWGT, VSIZE, ADJWGT, TPWGTS, UBVEC)
  INTEGER(I4B), INTENT(IN) :: NCON
  INTEGER(I4B), INTENT(IN) :: NPARTS
  INTEGER(I4B), INTENT(OUT) :: OBJVAL
  INTEGER(I4B), INTENT(OUT) :: PART(:)
  INTEGER(I4B), INTENT(IN) :: XADJ(:)
  INTEGER(I4B), INTENT(IN) :: ADJNCY(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: OPTIONS(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: VWGT(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: VSIZE(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: ADJWGT(:)
  REAL(DFP), OPTIONAL, TARGET, INTENT(IN) :: TPWGTS(:)
  REAL(DFP), OPTIONAL, TARGET, INTENT(IN) :: UBVEC(:)
  ! Internal variables
  INTEGER(I4B) :: NVTXS, IERR
  INTEGER(I4B), TARGET :: OPT(0:MAX_METIS_OPTIONS - 1)
  TYPE(C_PTR) :: C_OPTIONS, C_VWGT, C_VSIZE, C_ADJWGT, C_TPWGTS, C_UBVEC
  !
  NVTXS = SIZE(PART)

  IF (PRESENT(OPTIONS)) THEN
    C_OPTIONS = C_LOC(OPTIONS)
  ELSE
    IERR = METIS_SetDefaultOptions(OPT)
    IF (IERR .NE. METIS_OK) THEN
      CALL ErrorMSG( &
        & Msg="Error while setting default options", &
        & File="MetisInterface.F90", &
        & Routine="METISPartGraphKway()", &
        & Line=__LINE__, &
        & UnitNo=stdout)
      STOP
    END IF
    OPT(METIS_OPTION_NUMBERING) = METIS_NUMBERING_DEFAULT
    C_OPTIONS = C_LOC(OPT)
  END IF

  IF (PRESENT(VWGT)) THEN
    C_VWGT = C_LOC(VWGT)
  ELSE
    C_VWGT = C_NULL_PTR
  END IF

  IF (PRESENT(VSIZE)) THEN
    C_VSIZE = C_LOC(VSIZE)
  ELSE
    C_VSIZE = C_NULL_PTR
  END IF

  IF (PRESENT(ADJWGT)) THEN
    C_ADJWGT = C_LOC(ADJWGT)
  ELSE
    C_ADJWGT = C_NULL_PTR
  END IF

  IF (PRESENT(TPWGTS)) THEN
    C_TPWGTS = C_LOC(TPWGTS)
  ELSE
    C_TPWGTS = C_NULL_PTR
  END IF

  IF (PRESENT(UBVEC)) THEN
    C_UBVEC = C_LOC(UBVEC)
  ELSE
    C_UBVEC = C_NULL_PTR
  END IF

  IERR = METIS_PartGraphKway(NCON=NCON, NVTXS=NVTXS, XADJ=XADJ,&
    & ADJNCY=ADJNCY, NPARTS=NPARTS, OBJVAL=OBJVAL, PART=PART, &
    & VWGT=C_VWGT, VSIZE=C_VSIZE, ADJWGT=C_ADJWGT, &
    & TPWGTS=C_TPWGTS, UBVEC=C_UBVEC, OPTIONS=C_OPTIONS)

  IF (IERR .NE. METIS_OK) THEN
    CALL ErrorMSG( &
      & Msg="Error in METIS_PartGraphKway()", &
      & File="MetisInterface.F90", &
      & Routine="METISPartGraphKway()", &
      & Line=__LINE__, &
      & UnitNo=stdout)
    STOP
  END IF

  C_VWGT = C_NULL_PTR
  C_VSIZE = C_NULL_PTR
  C_ADJWGT = C_NULL_PTR
  C_TPWGTS = C_NULL_PTR
  C_UBVEC = C_NULL_PTR
  C_OPTIONS = C_NULL_PTR
END SUBROUTINE METISPartGraphKway

!----------------------------------------------------------------------------
!                                                         METISPartMeshDual
!----------------------------------------------------------------------------

SUBROUTINE METISPartMeshDual(NCOMMON, NPARTS, OBJVAL, EPART, NPART, &
  & EPTR, EIND, OPTIONS, VWGT, VSIZE, TPWGTS)
  INTEGER(I4B), INTENT(IN) :: NCOMMON
  INTEGER(I4B), INTENT(IN) :: NPARTS
  INTEGER(I4B), INTENT(OUT) :: OBJVAL
  INTEGER(I4B), INTENT(OUT) :: EPART(:)
  INTEGER(I4B), INTENT(OUT) :: NPART(:)
  INTEGER(I4B), INTENT(IN) :: EPTR(:)
  INTEGER(I4B), INTENT(IN) :: EIND(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: OPTIONS(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: VWGT(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: VSIZE(:)
  REAL(DFP), OPTIONAL, TARGET, INTENT(IN) :: TPWGTS(:)
  ! Internal variables
  INTEGER(I4B) :: NE, NN, IERR
  INTEGER(I4B), TARGET :: OPT(0:MAX_METIS_OPTIONS - 1)
  TYPE(C_PTR) :: C_OPTIONS, C_VWGT, C_VSIZE, C_TPWGTS
  !
  NE = SIZE(EPART)
  NN = SIZE(NPART)

  IF (PRESENT(OPTIONS)) THEN
    C_OPTIONS = C_LOC(OPTIONS)
  ELSE
    IERR = METIS_SetDefaultOptions(OPT)
    IF (IERR .NE. METIS_OK) THEN
      CALL ErrorMSG( &
        & Msg="Error while setting default options", &
        & File="MetisInterface.F90", &
        & Routine="METISPartMeshDual()", &
        & Line=__LINE__, &
        & UnitNo=stdout)
      STOP
    END IF
    OPT(METIS_OPTION_NUMBERING) = METIS_NUMBERING_DEFAULT
    C_OPTIONS = C_LOC(OPT)
  END IF

  IF (PRESENT(VWGT)) THEN
    C_VWGT = C_LOC(VWGT)
  ELSE
    C_VWGT = C_NULL_PTR
  END IF

  IF (PRESENT(VSIZE)) THEN
    C_VSIZE = C_LOC(VSIZE)
  ELSE
    C_VSIZE = C_NULL_PTR
  END IF

  IF (PRESENT(TPWGTS)) THEN
    C_TPWGTS = C_LOC(TPWGTS)
  ELSE
    C_TPWGTS = C_NULL_PTR
  END IF

  IERR = METIS_PartMeshDual(NE=NE, NN=NN, EPTR=EPTR, EIND=EIND, &
    & VWGT=C_VWGT, VSIZE=C_VSIZE, NCOMMON=NCOMMON, NPARTS=NPARTS, &
    & TPWGTS=C_TPWGTS, OPTIONS=C_OPTIONS, OBJVAL=OBJVAL, EPART=EPART, &
    & NPART=NPART)

  IF (IERR .NE. METIS_OK) THEN
    CALL ErrorMSG( &
      & Msg="Error in METIS_PartMeshDual()", &
      & File="MetisInterface.F90", &
      & Routine="METISPartMeshDual()", &
      & Line=__LINE__, &
      & UnitNo=stdout)
    STOP
  END IF

  C_VWGT = C_NULL_PTR
  C_VSIZE = C_NULL_PTR
  C_TPWGTS = C_NULL_PTR
  C_OPTIONS = C_NULL_PTR
END SUBROUTINE METISPartMeshDual

!----------------------------------------------------------------------------
!                                                         METISPartMeshNodal
!----------------------------------------------------------------------------

SUBROUTINE METISPartMeshNodal(NPARTS, OBJVAL, EPART, NPART, &
  & EPTR, EIND, OPTIONS, VWGT, VSIZE, TPWGTS)
  INTEGER(I4B), INTENT(IN) :: NPARTS
  INTEGER(I4B), INTENT(OUT) :: OBJVAL
  INTEGER(I4B), INTENT(OUT) :: EPART(:)
  INTEGER(I4B), INTENT(OUT) :: NPART(:)
  INTEGER(I4B), INTENT(IN) :: EPTR(:)
  INTEGER(I4B), INTENT(IN) :: EIND(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: OPTIONS(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: VWGT(:)
  INTEGER(I4B), OPTIONAL, TARGET, INTENT(IN) :: VSIZE(:)
  REAL(DFP), OPTIONAL, TARGET, INTENT(IN) :: TPWGTS(:)
  ! Internal variables
  INTEGER(I4B) :: NE, NN, IERR
  INTEGER(I4B), TARGET :: OPT(0:MAX_METIS_OPTIONS - 1)
  TYPE(C_PTR) :: C_OPTIONS, C_VWGT, C_VSIZE, C_TPWGTS
  !
  NE = SIZE(EPART)
  NN = SIZE(NPART)

  IF (PRESENT(OPTIONS)) THEN
    C_OPTIONS = C_LOC(OPTIONS)
  ELSE
    IERR = METIS_SetDefaultOptions(OPT)
    IF (IERR .NE. METIS_OK) THEN
      CALL ErrorMSG( &
        & Msg="Error while setting default options", &
        & File="MetisInterface.F90", &
        & Routine="METISPartMeshNodal()", &
        & Line=__LINE__, &
        & UnitNo=stdout)
      STOP
    END IF
    OPT(METIS_OPTION_NUMBERING) = METIS_NUMBERING_DEFAULT
    C_OPTIONS = C_LOC(OPT)
  END IF

  IF (PRESENT(VWGT)) THEN
    C_VWGT = C_LOC(VWGT)
  ELSE
    C_VWGT = C_NULL_PTR
  END IF

  IF (PRESENT(VSIZE)) THEN
    C_VSIZE = C_LOC(VSIZE)
  ELSE
    C_VSIZE = C_NULL_PTR
  END IF

  IF (PRESENT(TPWGTS)) THEN
    C_TPWGTS = C_LOC(TPWGTS)
  ELSE
    C_TPWGTS = C_NULL_PTR
  END IF

  IERR = METIS_PartMeshNodal(NE=NE, NN=NN, EPTR=EPTR, EIND=EIND, &
    & VWGT=C_VWGT, VSIZE=C_VSIZE, NPARTS=NPARTS, &
    & TPWGTS=C_TPWGTS, OPTIONS=C_OPTIONS, OBJVAL=OBJVAL, EPART=EPART, &
    & NPART=NPART)

  IF (IERR .NE. METIS_OK) THEN
    CALL ErrorMSG( &
      & Msg="Error in METIS_PartMeshNodal()", &
      & File="MetisInterface.F90", &
      & Routine="METISPartMeshNodal()", &
      & Line=__LINE__, &
      & UnitNo=stdout)
    STOP
  END IF

  C_VWGT = C_NULL_PTR
  C_VSIZE = C_NULL_PTR
  C_TPWGTS = C_NULL_PTR
  C_OPTIONS = C_NULL_PTR
END SUBROUTINE METISPartMeshNodal

!----------------------------------------------------------------------------
!                                                            METISMeshToDual
!----------------------------------------------------------------------------

SUBROUTINE METISMeshToDual(NE, NN, NCOMMON, EPTR, EIND, XADJ, ADJNCY, &
  & NUMFLAG)
  INTEGER(I4B), INTENT(IN) :: NE
  INTEGER(I4B), INTENT(IN) :: NN
  INTEGER(I4B), INTENT(IN) :: NCOMMON
  INTEGER(I4B), INTENT(IN) :: EPTR(:)
  INTEGER(I4B), INTENT(IN) :: EIND(:)
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: XADJ(:)
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: ADJNCY(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: NUMFLAG
  !
  INTEGER(I4B) :: NUM_FLAG, IERR
  TYPE(C_PTR) :: C_XADJ, C_ADJNCY
  INTEGER(I4B), POINTER :: F_XADJ(:), F_ADJNCY(:)

  IF (PRESENT(NUMFLAG)) THEN
    NUM_FLAG = NUMFLAG
  ELSE
    NUM_FLAG = METIS_NUMBERING_FORTRAN
  END IF

  IERR = METIS_MeshToDual(NE=NE, NN=NN, EPTR=EPTR, EIND=EIND, &
    & NCOMMON=NCOMMON, NUMFLAG=NUM_FLAG, XADJ=C_XADJ, ADJNCY=C_ADJNCY)

  IF (IERR .NE. METIS_OK) THEN
    CALL ErrorMSG( &
      & Msg="Error in METIS_MeshToDual()", &
      & File="MetisInterface.F90", &
      & Routine="METISMeshToDual()", &
      & Line=__LINE__, &
      & UnitNo=stdout)
    STOP
  END IF

  IF (C_ASSOCIATED(C_XADJ)) THEN
    CALL C_F_POINTER(CPTR=C_XADJ, FPTR=F_XADJ, shape=[NN + 1])
    XADJ = F_XADJ(1:NN + 1)
  ELSE
    CALL ErrorMSG( &
      & Msg="XADJ IS NOT ASSOCIATED", &
      & File="MetisInterface.F90", &
      & Routine="METISMeshToDual()", &
      & Line=__LINE__, &
      & UnitNo=stdout)
    STOP
  END IF

  IF (C_ASSOCIATED(C_ADJNCY)) THEN
    CALL C_F_POINTER(CPTR=C_ADJNCY, FPTR=F_ADJNCY, shape=[F_XADJ(nn + 1) - 1])
    ADJNCY = F_ADJNCY(1:F_XADJ(nn + 1) - 1)
  ELSE
    CALL ErrorMSG( &
      & Msg="ADJNCY IS NOT ASSOCIATED", &
      & File="MetisInterface.F90", &
      & Routine="METISMeshToDual()", &
      & Line=__LINE__, &
      & UnitNo=stdout)
    STOP
  END IF

  ierr = METIS_FREE(C_ADJNCY)
  ierr = METIS_FREE(C_XADJ)
  NULLIFY (F_ADJNCY, F_XADJ)

END SUBROUTINE METISMeshToDual

!----------------------------------------------------------------------------
!                                                            METISMeshToDual
!----------------------------------------------------------------------------

SUBROUTINE METISMeshToNodal(NE, NN, EPTR, EIND, XADJ, ADJNCY, &
  & NUMFLAG)
  INTEGER(I4B), INTENT(IN) :: NE
  INTEGER(I4B), INTENT(IN) :: NN
  INTEGER(I4B), INTENT(IN) :: EPTR(:)
  INTEGER(I4B), INTENT(IN) :: EIND(:)
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: XADJ(:)
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: ADJNCY(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: NUMFLAG
  !
  INTEGER(I4B) :: NUM_FLAG, IERR
  TYPE(C_PTR) :: C_XADJ, C_ADJNCY
  INTEGER(I4B), POINTER :: F_XADJ(:), F_ADJNCY(:)

  IF (PRESENT(NUMFLAG)) THEN
    NUM_FLAG = NUMFLAG
  ELSE
    NUM_FLAG = METIS_NUMBERING_FORTRAN
  END IF

  IERR = METIS_MeshToNodal(NE=NE, NN=NN, EPTR=EPTR, EIND=EIND, &
    & NUMFLAG=NUM_FLAG, XADJ=C_XADJ, ADJNCY=C_ADJNCY)

  IF (IERR .NE. METIS_OK) THEN
    CALL ErrorMSG( &
      & Msg="Error in METIS_MeshToNodal()", &
      & File="MetisInterface.F90", &
      & Routine="METISMeshToNodal()", &
      & Line=__LINE__, &
      & UnitNo=stdout)
    STOP
  END IF

  IF (C_ASSOCIATED(C_XADJ)) THEN
    CALL C_F_POINTER(CPTR=C_XADJ, FPTR=F_XADJ, shape=[NN + 1])
    XADJ = F_XADJ(1:NN + 1)
  ELSE
    CALL ErrorMSG( &
      & Msg="XADJ IS NOT ASSOCIATED", &
      & File="MetisInterface.F90", &
      & Routine="METISMeshToNodal()", &
      & Line=__LINE__, &
      & UnitNo=stdout)
    STOP
  END IF

  IF (C_ASSOCIATED(C_ADJNCY)) THEN
    CALL C_F_POINTER(CPTR=C_ADJNCY, FPTR=F_ADJNCY, shape=[F_XADJ(nn + 1) - 1])
    ADJNCY = F_ADJNCY(1:F_XADJ(nn + 1) - 1)
  ELSE
    CALL ErrorMSG( &
      & Msg="ADJNCY IS NOT ASSOCIATED", &
      & File="MetisInterface.F90", &
      & Routine="METISMeshToNodal()", &
      & Line=__LINE__, &
      & UnitNo=stdout)
    STOP
  END IF

  ierr = METIS_FREE(C_ADJNCY)
  ierr = METIS_FREE(C_XADJ)
  NULLIFY (F_ADJNCY, F_XADJ)
END SUBROUTINE METISMeshToNodal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MetisInterface
