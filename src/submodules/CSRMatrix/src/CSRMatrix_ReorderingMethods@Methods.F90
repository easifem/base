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

SUBMODULE(CSRMatrix_ReorderingMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              NestedDissect
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_NestedDissect
#ifdef USE_METIS
INTEGER(I4B) :: nrow, ncol, nnz, ii, jj, kk, nbr, ll
INTEGER(I4B), ALLOCATABLE :: XADJ(:), ADJNCY(:)
!
nrow = SIZE(csrMat, 1)
ncol = SIZE(csrMat, 2)
nnz = getNNZ(csrMat)
CALL REALLOCATE(XADJ, nrow + 1, ADJNCY, nnz)
CALL REALLOCATE(reorder%PERM, nrow, reorder%IPERM, nrow)
reorder%name = 'NodeND'
XADJ(1) = 1
kk = 0
DO ii = 1, nrow
  nbr = 0
  DO jj = csrMat%csr%IA(ii), csrMat%csr%IA(ii + 1) - 1
    ll = csrMat%csr%JA(jj)
    IF (ll .NE. ii) THEN
      nbr = nbr + 1
      kk = kk + 1
      ADJNCY(kk) = ll
    END IF
  END DO
  XADJ(ii + 1) = XADJ(ii) + nbr
END DO
ll = XADJ(SIZE(xadj)) - 1
CALL MetisNodeND(XADJ=XADJ, ADJNCY=ADJNCY(1:ll), PERM=reorder%PERM, &
  & IPERM=reorder%IPERM)
IF (ALLOCATED(XADJ)) DEALLOCATE (XADJ)
IF (ALLOCATED(ADJNCY)) DEALLOCATE (ADJNCY)
#else
CALL ErrorMSG( &
  & Msg="Metis library not installed!", &
  & File="CSRMatrix_ReorderingMethods@Methods.F90", &
  & Routine="csrMat_NestedDissect()", &
  & Line=__LINE__, &
  & UnitNo=stdout)
STOP
#endif
END PROCEDURE csrMat_NestedDissect

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_reorderDisplay
INTEGER(I4B) :: I
I = INPUT(Default=stdout, Option=unitNo)
CALL Display(obj%name, "# NAME : ")
CALL DISP(x=obj%PERM, title="PERM=", advance="NO", unit=I, &
  & style='left')
CALL DISP(x=obj%IPERM, title="IPERM=", advance="DOUBLE", &
  & unit=I, style='left')
END PROCEDURE csrMat_reorderDisplay

!----------------------------------------------------------------------------
!                                                                 Permute
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_Permute2
ans = Permute(obj=obj, rowPERM=rowPERM%IPERM, colPERM=colPERM%IPERM, &
  & isValues=.TRUE.)
END PROCEDURE csrMat_Permute2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
