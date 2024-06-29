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

!> author: Vikas Sharma, Ph. D.
! date:         26 Feb 2021
! summary:

MODULE F95_BLAS
USE F77_BLAS
IMPLICIT NONE
PRIVATE

PUBLIC :: IAMAX
PUBLIC :: SWAP
PUBLIC :: SCAL
PUBLIC :: ROTMG
PUBLIC :: ROTM
PUBLIC :: ROTG
PUBLIC :: ROT
PUBLIC :: NRM2
PUBLIC :: DOTU
PUBLIC :: DOT
PUBLIC :: DOTC
PUBLIC :: SDOT
PUBLIC :: COPY
PUBLIC :: AXPY
PUBLIC :: ASUM
PUBLIC :: GEMV
PUBLIC :: GEMM

#ifndef USE_NativeBLAS
PUBLIC :: IAMIN
#endif

#ifndef USE_APPLE_NativeBLAS
PUBLIC :: CABS1
#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE ASUM
  MODULE PROCEDURE SASUM_F95, SCASUM_F95, DASUM_F95, DZASUM_F95
END INTERFACE ASUM

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE AXPY
  MODULE PROCEDURE SAXPY_F95, DAXPY_F95, CAXPY_F95, ZAXPY_F95
END INTERFACE AXPY

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE COPY
  MODULE PROCEDURE SCOPY_F95, DCOPY_F95, CCOPY_F95, ZCOPY_F95
END INTERFACE COPY

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE DOT
  MODULE PROCEDURE SDOT_F95, DDOT_F95
END INTERFACE DOT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE SDOT
  MODULE PROCEDURE SDSDOT_F95, DSDOT_F95
END INTERFACE SDOT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE DOTC
  MODULE PROCEDURE CDOTC_F95, ZDOTC_F95
END INTERFACE DOTC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE DOTU
  MODULE PROCEDURE CDOTU_F95, ZDOTU_F95
END INTERFACE DOTU

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE NRM2
  MODULE PROCEDURE SNRM2_F95, DNRM2_F95, SCNRM2_F95, DZNRM2_F95
END INTERFACE NRM2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE ROT
  MODULE PROCEDURE SROT_F95, DROT_F95, CSROT_F95, ZDROT_F95
END INTERFACE ROT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE ROTG
  MODULE PROCEDURE SROTG_F95, DROTG_F95, CROTG_F95, ZROTG_F95
END INTERFACE ROTG

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE ROTM
  MODULE PROCEDURE SROTM_F95, DROTM_F95
END INTERFACE ROTM

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE ROTMG
  MODULE PROCEDURE SROTMG_F95, DROTMG_F95
END INTERFACE ROTMG

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE SCAL
  MODULE PROCEDURE SSCAL_F95, DSCAL_F95, CSCAL_F95, ZSCAL_F95, CSSCAL_F95,&
    & ZDSCAL_F95
END INTERFACE SCAL

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE SWAP
  MODULE PROCEDURE SSWAP_F95, DSWAP_F95, CSWAP_F95, ZSWAP_F95
END INTERFACE SWAP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE IAMAX
  MODULE PROCEDURE ISAMAX_F95, IDAMAX_F95, ICAMAX_F95, IZAMAX_F95
END INTERFACE IAMAX

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifndef USE_NativeBLAS
INTERFACE IAMIN
  MODULE PROCEDURE ISAMIN_F95, IDAMIN_F95, ICAMIN_F95, IZAMIN_F95
END INTERFACE IAMIN
#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifndef USE_APPLE_NativeBLAS
INTERFACE CABS1
  MODULE PROCEDURE SCABS1_F95, DCABS1_F95
END INTERFACE CABS1
#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE GEMV
  MODULE PROCEDURE SGEMV_F95, DGEMV_F95, CGEMV_F95, ZGEMV_F95
END INTERFACE GEMV

#ifdef USE_INTEL_MKL
INTERFACE GEMV
  MODULE PROCEDURE SCGEMV_F95, DZGEMV_F95
END INTERFACE GEMV
#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE GEMM
  MODULE PROCEDURE SGEMM_F95, DGEMM_F95, CGEMM_F95, ZGEMM_F95
END INTERFACE GEMM

! #ifdef USE_INTEL_MKL
! INTERFACE GEMV
!   MODULE PROCEDURE SCGEMV_F95, DZGEMV_F95
! END INTERFACE GEMV
! #endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

#ifndef USE_APPLE_NativeBLAS
#include "./blas95_src/dcabs1.F90"
#include "./blas95_src/scabs1.F90"
#include "./blas95_src/cgemm3m.F90"
#include "./blas95_src/saxpby.F90"
#include "./blas95_src/daxpby.F90"
#include "./blas95_src/caxpby.F90"
#include "./blas95_src/zaxpby.F90"
#include "./blas95_src/zgemm3m.F90"
#endif

#include "./blas95_src/caxpy.F90"
#include "./blas95_src/ccopy.F90"
#include "./blas95_src/cdotc.F90"
#include "./blas95_src/cdotu.F90"
#include "./blas95_src/cgbmv.F90"
#include "./blas95_src/cgemm.F90"
#include "./blas95_src/cgemv.F90"
#include "./blas95_src/cgerc.F90"
#include "./blas95_src/cgeru.F90"
#include "./blas95_src/chbmv.F90"
#include "./blas95_src/chemm.F90"
#include "./blas95_src/chemv.F90"
#include "./blas95_src/cher.F90"
#include "./blas95_src/cher2.F90"
#include "./blas95_src/cher2k.F90"
#include "./blas95_src/cherk.F90"
#include "./blas95_src/chpmv.F90"
#include "./blas95_src/chpr.F90"
#include "./blas95_src/chpr2.F90"
#include "./blas95_src/crotg.F90"
#include "./blas95_src/cscal.F90"
#include "./blas95_src/csrot.F90"
#include "./blas95_src/csscal.F90"
#include "./blas95_src/cswap.F90"
#include "./blas95_src/zswap.F90"
#include "./blas95_src/csymm.F90"
#include "./blas95_src/csyr2k.F90"
#include "./blas95_src/csyrk.F90"
#include "./blas95_src/ctbmv.F90"
#include "./blas95_src/ctbsv.F90"
#include "./blas95_src/ctpmv.F90"
#include "./blas95_src/ctpsv.F90"
#include "./blas95_src/ctrmm.F90"
#include "./blas95_src/ctrmv.F90"
#include "./blas95_src/ctrsm.F90"
#include "./blas95_src/ctrsv.F90"
#include "./blas95_src/dasum.F90"
#include "./blas95_src/daxpy.F90"
#include "./blas95_src/dcopy.F90"
#include "./blas95_src/ddot.F90"
#include "./blas95_src/dgbmv.F90"
#include "./blas95_src/dgemm.F90"
#include "./blas95_src/dgemv.F90"
#include "./blas95_src/dger.F90"
#include "./blas95_src/dnrm2.F90"
#include "./blas95_src/drot.F90"
#include "./blas95_src/drotm.F90"
#include "./blas95_src/drotmg.F90"
#include "./blas95_src/drotg.F90"
#include "./blas95_src/dsbmv.F90"
#include "./blas95_src/dscal.F90"
#include "./blas95_src/dsdot.F90"
#include "./blas95_src/dspmv.F90"
#include "./blas95_src/dspr.F90"
#include "./blas95_src/dspr2.F90"
#include "./blas95_src/dswap.F90"
#include "./blas95_src/dsymm.F90"
#include "./blas95_src/dsymv.F90"
#include "./blas95_src/dsyr.F90"
#include "./blas95_src/dsyr2.F90"
#include "./blas95_src/dsyr2k.F90"
#include "./blas95_src/dsyrk.F90"
#include "./blas95_src/dtbmv.F90"
#include "./blas95_src/dtbsv.F90"
#include "./blas95_src/dtpmv.F90"
#include "./blas95_src/dtpsv.F90"
#include "./blas95_src/dtrmm.F90"
#include "./blas95_src/dtrmv.F90"
#include "./blas95_src/dtrsm.F90"
#include "./blas95_src/dtrsv.F90"
#include "./blas95_src/dzasum.F90"
#include "./blas95_src/dznrm2.F90"
#include "./blas95_src/icamax.F90"
#include "./blas95_src/idamax.F90"
#include "./blas95_src/isamax.F90"
#include "./blas95_src/izamax.F90"
#include "./blas95_src/sasum.F90"
#include "./blas95_src/saxpy.F90"
#include "./blas95_src/scasum.F90"
#include "./blas95_src/scnrm2.F90"
#include "./blas95_src/scopy.F90"
#include "./blas95_src/sdot.F90"
#include "./blas95_src/sdsdot.F90"
#include "./blas95_src/sgbmv.F90"
#include "./blas95_src/sgemm.F90"
#include "./blas95_src/sgemv.F90"
#include "./blas95_src/sger.F90"
#include "./blas95_src/snrm2.F90"
#include "./blas95_src/srot.F90"
#include "./blas95_src/srotm.F90"
#include "./blas95_src/srotmg.F90"
#include "./blas95_src/srotg.F90"
#include "./blas95_src/ssbmv.F90"
#include "./blas95_src/sscal.F90"
#include "./blas95_src/sspmv.F90"
#include "./blas95_src/sspr.F90"
#include "./blas95_src/sspr2.F90"
#include "./blas95_src/sswap.F90"
#include "./blas95_src/ssymm.F90"
#include "./blas95_src/ssymv.F90"
#include "./blas95_src/ssyr.F90"
#include "./blas95_src/ssyr2.F90"
#include "./blas95_src/ssyr2k.F90"
#include "./blas95_src/ssyrk.F90"
#include "./blas95_src/stbmv.F90"
#include "./blas95_src/stbsv.F90"
#include "./blas95_src/stpmv.F90"
#include "./blas95_src/stpsv.F90"
#include "./blas95_src/strmm.F90"
#include "./blas95_src/strmv.F90"
#include "./blas95_src/strsm.F90"
#include "./blas95_src/strsv.F90"
#include "./blas95_src/zaxpy.F90"
#include "./blas95_src/zcopy.F90"
#include "./blas95_src/zdotc.F90"
#include "./blas95_src/zdotu.F90"
#include "./blas95_src/zdrot.F90"
#include "./blas95_src/zdscal.F90"
#include "./blas95_src/zgbmv.F90"
#include "./blas95_src/zgemm.F90"
#include "./blas95_src/zgemv.F90"
#include "./blas95_src/zgerc.F90"
#include "./blas95_src/zgeru.F90"
#include "./blas95_src/zhbmv.F90"
#include "./blas95_src/zhemm.F90"
#include "./blas95_src/zhemv.F90"
#include "./blas95_src/zher.F90"
#include "./blas95_src/zher2.F90"
#include "./blas95_src/zher2k.F90"
#include "./blas95_src/zherk.F90"
#include "./blas95_src/zhpmv.F90"
#include "./blas95_src/zhpr.F90"
#include "./blas95_src/zhpr2.F90"
#include "./blas95_src/zrotg.F90"
#include "./blas95_src/zscal.F90"
#include "./blas95_src/zsymm.F90"
#include "./blas95_src/zsyr2k.F90"
#include "./blas95_src/zsyrk.F90"
#include "./blas95_src/ztbmv.F90"
#include "./blas95_src/ztbsv.F90"
#include "./blas95_src/ztpmv.F90"
#include "./blas95_src/ztpsv.F90"
#include "./blas95_src/ztrmm.F90"
#include "./blas95_src/ztrmv.F90"
#include "./blas95_src/ztrsm.F90"
#include "./blas95_src/ztrsv.F90"

#ifndef USE_NativeBLAS
#include "./blas95_src/icamin.F90"
#include "./blas95_src/idamin.F90"
#include "./blas95_src/isamin.F90"
#include "./blas95_src/izamin.F90"
#endif

#ifdef USE_INTEL_MKL
#include "./blas95_src/droti.F90"
#include "./blas95_src/sroti.F90"
#include "./blas95_src/zgem2vc.F90"
#include "./blas95_src/cgem2vc.F90"
#include "./blas95_src/dgem2vu.F90"
#include "./blas95_src/sgem2vu.F90"
#include "./blas95_src/caxpyi.F90"
#include "./blas95_src/daxpyi.F90"
#include "./blas95_src/saxpyi.F90"
#include "./blas95_src/zaxpyi.F90"
#include "./blas95_src/ddoti.F90"
#include "./blas95_src/sdoti.F90"
#include "./blas95_src/cdotci.F90"
#include "./blas95_src/zdotci.F90"
#include "./blas95_src/cdotui.F90"
#include "./blas95_src/zdotui.F90"
#include "./blas95_src/cgemm_batch.F90"
#include "./blas95_src/cgemm3m_batch.F90"
#include "./blas95_src/ctrsm_batch.F90"
#include "./blas95_src/dgemm_batch.F90"
#include "./blas95_src/dtrsm_batch.F90"
#include "./blas95_src/sgemm_batch.F90"
#include "./blas95_src/strsm_batch.F90"
#include "./blas95_src/zgemm_batch.F90"
#include "./blas95_src/zgemm3m_batch.F90"
#include "./blas95_src/ztrsm_batch.F90"
#include "./blas95_src/cgemmt.F90"
#include "./blas95_src/dgemmt.F90"
#include "./blas95_src/sgemmt.F90"
#include "./blas95_src/zgemmt.F90"
#include "./blas95_src/cgthr.F90"
#include "./blas95_src/cgthrz.F90"
#include "./blas95_src/dgthr.F90"
#include "./blas95_src/dgthrz.F90"
#include "./blas95_src/sgthr.F90"
#include "./blas95_src/sgthrz.F90"
#include "./blas95_src/zgthr.F90"
#include "./blas95_src/zgthrz.F90"
#include "./blas95_src/dsctr.F90"
#include "./blas95_src/ssctr.F90"
#include "./blas95_src/csctr.F90"
#include "./blas95_src/dzgemm.F90"
#include "./blas95_src/dzgemv.F90"
#include "./blas95_src/scgemm.F90"
#include "./blas95_src/scgemv.F90"
#endif

END MODULE F95_BLAS
