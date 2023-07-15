INTERFACE ASUM
  MODULE PROCEDURE SASUM_F95, SCASUM_F95, DASUM_F95, DZASUM_F95
END INTERFACE ASUM

PUBLIC :: ASUM

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE AXPY
  MODULE PROCEDURE SAXPY_F95, DAXPY_F95, CAXPY_F95, ZAXPY_F95
END INTERFACE AXPY

PUBLIC :: AXPY

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE COPY
  MODULE PROCEDURE SCOPY_F95, DCOPY_F95, CCOPY_F95, ZCOPY_F95
END INTERFACE COPY

PUBLIC :: COPY

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE DOT
  MODULE PROCEDURE SDOT_F95, DDOT_F95
END INTERFACE DOT

PUBLIC :: DOT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE SDOT
  MODULE PROCEDURE SDSDOT_F95, DSDOT_F95
END INTERFACE SDOT

PUBLIC :: SDOT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE DOTC
  MODULE PROCEDURE CDOTC_F95, ZDOTC_F95
END INTERFACE DOTC

PUBLIC :: DOTC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE DOTU
  MODULE PROCEDURE CDOTU_F95, ZDOTU_F95
END INTERFACE DOTU

PUBLIC :: DOTU

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE NRM2
  MODULE PROCEDURE SNRM2_F95, DNRM2_F95, SCNRM2_F95, DZNRM2_F95
END INTERFACE NRM2

PUBLIC :: NRM2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE ROT
  MODULE PROCEDURE SROT_F95, DROT_F95, CSROT_F95, ZDROT_F95
END INTERFACE ROT

PUBLIC :: ROT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE ROTG
  MODULE PROCEDURE SROTG_F95, DROTG_F95, CROTG_F95, ZROTG_F95
END INTERFACE ROTG

PUBLIC :: ROTG

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE ROTM
  MODULE PROCEDURE SROTM_F95, DROTM_F95
END INTERFACE ROTM

PUBLIC :: ROTM

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE ROTMG
  MODULE PROCEDURE SROTMG_F95, DROTMG_F95
END INTERFACE ROTMG

PUBLIC :: ROTMG

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE SCAL
  MODULE PROCEDURE SSCAL_F95, DSCAL_F95, CSCAL_F95, ZSCAL_F95, CSSCAL_F95,&
    & ZDSCAL_F95
END INTERFACE SCAL

PUBLIC :: SCAL

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE SWAP
  MODULE PROCEDURE SSWAP_F95, DSWAP_F95, CSWAP_F95, ZSWAP_F95
END INTERFACE SWAP

PUBLIC :: SWAP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE IAMAX
  MODULE PROCEDURE ISAMAX_F95, IDAMAX_F95, ICAMAX_F95, IZAMAX_F95
END INTERFACE IAMAX

PUBLIC :: IAMAX

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifndef USE_NativeBLAS
INTERFACE IAMIN
  MODULE PROCEDURE ISAMIN_F95, IDAMIN_F95, ICAMIN_F95, IZAMIN_F95
END INTERFACE IAMIN

PUBLIC :: IAMIN

#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifndef USE_APPLE_NativeBLAS
INTERFACE CABS1
  MODULE PROCEDURE SCABS1_F95, DCABS1_F95
END INTERFACE CABS1

PUBLIC :: CABS1
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

PUBLIC :: GEMV
