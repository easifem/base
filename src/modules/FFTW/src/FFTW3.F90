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

#ifdef USE_FFTW
MODULE FFTW3
USE, INTRINSIC :: ISO_C_BINDING
INTEGER, PARAMETER :: C_FFTW_R2R_KIND = C_INT32_T
INTEGER(C_INT), PARAMETER :: FFTW_R2HC = 0
INTEGER(C_INT), PARAMETER :: FFTW_HC2R = 1
INTEGER(C_INT), PARAMETER :: FFTW_DHT = 2
INTEGER(C_INT), PARAMETER :: FFTW_REDFT00 = 3
INTEGER(C_INT), PARAMETER :: FFTW_REDFT01 = 4
INTEGER(C_INT), PARAMETER :: FFTW_REDFT10 = 5
INTEGER(C_INT), PARAMETER :: FFTW_REDFT11 = 6
INTEGER(C_INT), PARAMETER :: FFTW_RODFT00 = 7
INTEGER(C_INT), PARAMETER :: FFTW_RODFT01 = 8
INTEGER(C_INT), PARAMETER :: FFTW_RODFT10 = 9
INTEGER(C_INT), PARAMETER :: FFTW_RODFT11 = 10
INTEGER(C_INT), PARAMETER :: FFTW_FORWARD = -1
INTEGER(C_INT), PARAMETER :: FFTW_BACKWARD = +1
INTEGER(C_INT), PARAMETER :: FFTW_MEASURE = 0
INTEGER(C_INT), PARAMETER :: FFTW_DESTROY_INPUT = 1
INTEGER(C_INT), PARAMETER :: FFTW_UNALIGNED = 2
INTEGER(C_INT), PARAMETER :: FFTW_CONSERVE_MEMORY = 4
INTEGER(C_INT), PARAMETER :: FFTW_EXHAUSTIVE = 8
INTEGER(C_INT), PARAMETER :: FFTW_PRESERVE_INPUT = 16
INTEGER(C_INT), PARAMETER :: FFTW_PATIENT = 32
INTEGER(C_INT), PARAMETER :: FFTW_ESTIMATE = 64
INTEGER(C_INT), PARAMETER :: FFTW_WISDOM_ONLY = 2097152
INTEGER(C_INT), PARAMETER :: FFTW_ESTIMATE_PATIENT = 128
INTEGER(C_INT), PARAMETER :: FFTW_BELIEVE_PCOST = 256
INTEGER(C_INT), PARAMETER :: FFTW_NO_DFT_R2HC = 512
INTEGER(C_INT), PARAMETER :: FFTW_NO_NONTHREADED = 1024
INTEGER(C_INT), PARAMETER :: FFTW_NO_BUFFERING = 2048
INTEGER(C_INT), PARAMETER :: FFTW_NO_INDIRECT_OP = 4096
INTEGER(C_INT), PARAMETER :: FFTW_ALLOW_LARGE_GENERIC = 8192
INTEGER(C_INT), PARAMETER :: FFTW_NO_RANK_SPLITS = 16384
INTEGER(C_INT), PARAMETER :: FFTW_NO_VRANK_SPLITS = 32768
INTEGER(C_INT), PARAMETER :: FFTW_NO_VRECURSE = 65536
INTEGER(C_INT), PARAMETER :: FFTW_NO_SIMD = 131072
INTEGER(C_INT), PARAMETER :: FFTW_NO_SLOW = 262144
INTEGER(C_INT), PARAMETER :: FFTW_NO_FIXED_RADIX_LARGE_N = 524288
INTEGER(C_INT), PARAMETER :: FFTW_ALLOW_PRUNING = 1048576

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE, BIND(C) :: fftw_iodim
  INTEGER(C_INT) :: n, is, os
END TYPE fftw_iodim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE, BIND(C) :: fftw_iodim64
  INTEGER(C_INTPTR_T) n, is, os
END TYPE fftw_iodim64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE, BIND(C) :: fftwf_iodim
  INTEGER(C_INT) n, is, os
END TYPE fftwf_iodim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE, BIND(C) :: fftwf_iodim64
  INTEGER(C_INTPTR_T) n, is, os
END TYPE fftwf_iodim64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_dft(rank,n,in,out,sign,flags) &
    & BIND(C, name='fftw_plan_dft')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_dft_1d(n,in,out,sign,flags) &
    & BIND(C, name='fftw_plan_dft_1d')
    IMPORT
    INTEGER(C_INT), VALUE :: n
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_dft_1d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_dft_2d(n0,n1,in,out,sign,flags) &
    & BIND(C, name='fftw_plan_dft_2d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_dft_2d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_dft_3d(n0,n1,n2,in,out,sign,flags) &
    & BIND(C, name='fftw_plan_dft_3d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    INTEGER(C_INT), VALUE :: n2
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_dft_3d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_many_dft(rank,n,howmany,in,inembed, &
    & istride,idist,out,onembed,ostride,odist,sign,flags) &
    & BIND(C, name='fftw_plan_many_dft')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    INTEGER(C_INT), VALUE :: howmany
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: inembed
    INTEGER(C_INT), VALUE :: istride
    INTEGER(C_INT), VALUE :: idist
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: onembed
    INTEGER(C_INT), VALUE :: ostride
    INTEGER(C_INT), VALUE :: odist
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_many_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru_dft(rank,dims,howmany_rank,&
    & howmany_dims,in,out,sign,flags) &
    & BIND(C, name='fftw_plan_guru_dft')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru_split_dft(rank,dims,howmany_rank,&
    & howmany_dims,ri,ii,ro,io,flags) &
    & BIND(C, name='fftw_plan_guru_split_dft')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ri
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ii
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ro
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: io
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru_split_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru64_dft(rank,dims,howmany_rank,&
    & howmany_dims,in,out,sign,flags) &
    & BIND(C, name='fftw_plan_guru64_dft')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru64_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru64_split_dft(rank,dims,howmany_rank,&
    & howmany_dims,ri,ii,ro,io,flags) &
    & BIND(C, name='fftw_plan_guru64_split_dft')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ri
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ii
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ro
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: io
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru64_split_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_execute_dft(p,in,out) BIND(C, name='fftw_execute_dft')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(INOUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
  END SUBROUTINE fftw_execute_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_execute_split_dft(p,ri,ii,ro,io) &
    & BIND(C, name='fftw_execute_split_dft')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    REAL(C_DOUBLE), DIMENSION(*), INTENT(INOUT) :: ri
    REAL(C_DOUBLE), DIMENSION(*), INTENT(INOUT) :: ii
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ro
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: io
  END SUBROUTINE fftw_execute_split_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_many_dft_r2c(rank,n,howmany,in,inembed,&
    & istride,idist,out,onembed,ostride,odist,flags) &
    & BIND(C, name='fftw_plan_many_dft_r2c')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    INTEGER(C_INT), VALUE :: howmany
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: inembed
    INTEGER(C_INT), VALUE :: istride
    INTEGER(C_INT), VALUE :: idist
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: onembed
    INTEGER(C_INT), VALUE :: ostride
    INTEGER(C_INT), VALUE :: odist
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_many_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_dft_r2c(rank,n,in,out,flags) &
    & BIND(C, name='fftw_plan_dft_r2c')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_dft_r2c_1d(n,in,out,flags) &
    & BIND(C, name='fftw_plan_dft_r2c_1d')
    IMPORT
    INTEGER(C_INT), VALUE :: n
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_dft_r2c_1d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_dft_r2c_2d(n0,n1,in,out,flags) &
    & BIND(C, name='fftw_plan_dft_r2c_2d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_dft_r2c_2d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_dft_r2c_3d(n0,n1,n2,in,out,flags) &
    & BIND(C, name='fftw_plan_dft_r2c_3d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    INTEGER(C_INT), VALUE :: n2
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_dft_r2c_3d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_many_dft_c2r(rank,n,howmany,in,inembed,&
    & istride,idist,out,onembed,ostride,odist,flags) &
    & BIND(C, name='fftw_plan_many_dft_c2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    INTEGER(C_INT), VALUE :: howmany
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: inembed
    INTEGER(C_INT), VALUE :: istride
    INTEGER(C_INT), VALUE :: idist
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: onembed
    INTEGER(C_INT), VALUE :: ostride
    INTEGER(C_INT), VALUE :: odist
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_many_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_dft_c2r(rank,n,in,out,flags) &
    & BIND(C, name='fftw_plan_dft_c2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_dft_c2r_1d(n,in,out,flags)  &
    & BIND(C, name='fftw_plan_dft_c2r_1d')
    IMPORT
    INTEGER(C_INT), VALUE :: n
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_dft_c2r_1d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_dft_c2r_2d(n0,n1,in,out,flags)  &
    & BIND(C, name='fftw_plan_dft_c2r_2d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_dft_c2r_2d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_dft_c2r_3d(n0,n1,n2,in,out,flags) &
    & BIND(C, name='fftw_plan_dft_c2r_3d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    INTEGER(C_INT), VALUE :: n2
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_dft_c2r_3d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru_dft_r2c(rank,dims,howmany_rank, &
    & howmany_dims,in,out,flags)  &
    & BIND(C, name='fftw_plan_guru_dft_r2c')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru_dft_c2r(rank,dims,howmany_rank, &
    & howmany_dims,in,out,flags) &
    & BIND(C, name='fftw_plan_guru_dft_c2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru_split_dft_r2c(rank,dims,howmany_rank, &
    & howmany_dims,in,ro,io,flags) &
    & BIND(C, name='fftw_plan_guru_split_dft_r2c')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ro
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: io
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru_split_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru_split_dft_c2r(rank,dims,howmany_rank, &
    & howmany_dims,ri,ii,out,flags) &
    & BIND(C, name='fftw_plan_guru_split_dft_c2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ri
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ii
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru_split_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru64_dft_r2c(rank,dims,howmany_rank,&
    & howmany_dims,in,out,flags) &
    & BIND(C, name='fftw_plan_guru64_dft_r2c')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru64_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru64_dft_c2r(rank,dims,howmany_rank,&
    & howmany_dims,in,out,flags) &
    & BIND(C, name='fftw_plan_guru64_dft_c2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru64_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru64_split_dft_r2c(rank,dims,howmany_rank,&
    & howmany_dims,in,ro,io,flags) &
    & BIND(C, name='fftw_plan_guru64_split_dft_r2c')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ro
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: io
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru64_split_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru64_split_dft_c2r(rank,dims,howmany_rank,&
    & howmany_dims,ri,ii,out,flags) &
    & BIND(C, name='fftw_plan_guru64_split_dft_c2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ri
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ii
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru64_split_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_execute_dft_r2c(p,in,out) &
    & BIND(C, name='fftw_execute_dft_r2c')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    REAL(C_DOUBLE), DIMENSION(*), INTENT(INOUT) :: in
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
  END SUBROUTINE fftw_execute_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_execute_dft_c2r(p,in,out) &
    & BIND(C, name='fftw_execute_dft_c2r')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(*), INTENT(INOUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
  END SUBROUTINE fftw_execute_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_execute_split_dft_r2c(p,in,ro,io) &
    & BIND(C, name='fftw_execute_split_dft_r2c')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    REAL(C_DOUBLE), DIMENSION(*), INTENT(INOUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: ro
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: io
  END SUBROUTINE fftw_execute_split_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_execute_split_dft_c2r(p,ri,ii,out) &
    & BIND(C, name='fftw_execute_split_dft_c2r')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    REAL(C_DOUBLE), DIMENSION(*), INTENT(INOUT) :: ri
    REAL(C_DOUBLE), DIMENSION(*), INTENT(INOUT) :: ii
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
  END SUBROUTINE fftw_execute_split_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_many_r2r(rank,n,howmany,in,inembed,&
    & istride, idist,out,onembed,ostride,odist,kind,flags) &
    & BIND(C, name='fftw_plan_many_r2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    INTEGER(C_INT), VALUE :: howmany
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: inembed
    INTEGER(C_INT), VALUE :: istride
    INTEGER(C_INT), VALUE :: idist
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: onembed
    INTEGER(C_INT), VALUE :: ostride
    INTEGER(C_INT), VALUE :: odist
    INTEGER(C_FFTW_R2R_KIND), DIMENSION(*), INTENT(IN) :: kind
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_many_r2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_r2r(rank,n,in,out,kind,flags)  &
    & BIND(C, name='fftw_plan_r2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_FFTW_R2R_KIND), DIMENSION(*), INTENT(IN) :: kind
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_r2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_r2r_1d(n,in,out,kind,flags)  &
    & BIND(C, name='fftw_plan_r2r_1d')
    IMPORT
    INTEGER(C_INT), VALUE :: n
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_FFTW_R2R_KIND), VALUE :: kind
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_r2r_1d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_r2r_2d(n0,n1,in,out,kind0,kind1,flags) &
    & BIND(C, name='fftw_plan_r2r_2d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_FFTW_R2R_KIND), VALUE :: kind0
    INTEGER(C_FFTW_R2R_KIND), VALUE :: kind1
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_r2r_2d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_r2r_3d(n0,n1,n2,in,out,kind0,kind1,kind2, &
    & flags) BIND(C, name='fftw_plan_r2r_3d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    INTEGER(C_INT), VALUE :: n2
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_FFTW_R2R_KIND), VALUE :: kind0
    INTEGER(C_FFTW_R2R_KIND), VALUE :: kind1
    INTEGER(C_FFTW_R2R_KIND), VALUE :: kind2
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_r2r_3d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru_r2r(rank,dims,howmany_rank, &
    & howmany_dims,in,out,kind,flags) &
    & BIND(C, name='fftw_plan_guru_r2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_FFTW_R2R_KIND), DIMENSION(*), INTENT(IN) :: kind
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru_r2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_plan_guru64_r2r(rank,dims,howmany_rank,&
    & howmany_dims,in,out,kind,flags) &
    & BIND(C, name='fftw_plan_guru64_r2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftw_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_FFTW_R2R_KIND), DIMENSION(*), INTENT(IN) :: kind
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftw_plan_guru64_r2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_execute_r2r(p,in,out) BIND(C, name='fftw_execute_r2r')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    REAL(C_DOUBLE), DIMENSION(*), INTENT(INOUT) :: in
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: out
  END SUBROUTINE fftw_execute_r2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_destroy_plan(p) BIND(C, name='fftw_destroy_plan')
    IMPORT
    TYPE(C_PTR), VALUE :: p
  END SUBROUTINE fftw_destroy_plan
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_forget_wisdom() BIND(C, name='fftw_forget_wisdom')
    IMPORT
  END SUBROUTINE fftw_forget_wisdom
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_cleanup() BIND(C, name='fftw_cleanup')
    IMPORT
  END SUBROUTINE fftw_cleanup
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_set_timelimit(t) BIND(C, name='fftw_set_timelimit')
    IMPORT
    REAL(C_DOUBLE), VALUE :: t
  END SUBROUTINE fftw_set_timelimit
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_plan_with_nthreads(nthreads) &
    & BIND(C, name='fftw_plan_with_nthreads')
    IMPORT
    INTEGER(C_INT), VALUE :: nthreads
  END SUBROUTINE fftw_plan_with_nthreads
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  INTEGER(C_INT) FUNCTION fftw_planner_nthreads() &
    & BIND(C, name='fftw_planner_nthreads')
    IMPORT
  END FUNCTION fftw_planner_nthreads
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  INTEGER(C_INT) FUNCTION fftw_init_threads() &
    &  BIND(C, name='fftw_init_threads')
    IMPORT
  END FUNCTION fftw_init_threads
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_cleanup_threads() BIND(C, name='fftw_cleanup_threads')
    IMPORT
  END SUBROUTINE fftw_cleanup_threads
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
! Unable to generate Fortran interface for fftw_threads_set_callback
  SUBROUTINE fftw_make_planner_thread_safe() &
    & BIND(C, name='fftw_make_planner_thread_safe')
    IMPORT
  END SUBROUTINE fftw_make_planner_thread_safe
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  INTEGER(C_INT) FUNCTION fftw_export_wisdom_to_filename(filename) &
    & BIND(C, name='fftw_export_wisdom_to_filename')
    IMPORT
    CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: filename
  END FUNCTION fftw_export_wisdom_to_filename
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_export_wisdom_to_file(output_file) &
    & BIND(C, name='fftw_export_wisdom_to_file')
    IMPORT
    TYPE(C_PTR), VALUE :: output_file
  END SUBROUTINE fftw_export_wisdom_to_file
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_export_wisdom_to_string() &
    & BIND(C, name='fftw_export_wisdom_to_string')
    IMPORT
  END FUNCTION fftw_export_wisdom_to_string
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_export_wisdom(write_char,data) &
    & BIND(C, name='fftw_export_wisdom')
    IMPORT
    TYPE(C_FUNPTR), VALUE :: write_char
    TYPE(C_PTR), VALUE :: data
  END SUBROUTINE fftw_export_wisdom
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  INTEGER(C_INT) FUNCTION fftw_IMPORT_system_wisdom() &
    & BIND(C, name='fftw_IMPORT_system_wisdom')
    IMPORT
  END FUNCTION fftw_IMPORT_system_wisdom
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  INTEGER(C_INT) FUNCTION fftw_IMPORT_wisdom_from_filename(filename) &
    &  BIND(C, name='fftw_IMPORT_wisdom_from_filename')
    IMPORT
    CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: filename
  END FUNCTION fftw_IMPORT_wisdom_from_filename
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  INTEGER(C_INT) FUNCTION fftw_IMPORT_wisdom_from_file(input_file) &
    & BIND(C, name='fftw_IMPORT_wisdom_from_file')
    IMPORT
    TYPE(C_PTR), VALUE :: input_file
  END FUNCTION fftw_IMPORT_wisdom_from_file
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  INTEGER(C_INT) FUNCTION fftw_IMPORT_wisdom_from_string(input_string) &
    & BIND(C, name='fftw_IMPORT_wisdom_from_string')
    IMPORT
    CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: input_string
  END FUNCTION fftw_IMPORT_wisdom_from_string
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  INTEGER(C_INT) FUNCTION fftw_IMPORT_wisdom(read_char,data) &
    & BIND(C, name='fftw_IMPORT_wisdom')
    IMPORT
    TYPE(C_FUNPTR), VALUE :: read_char
    TYPE(C_PTR), VALUE :: data
  END FUNCTION fftw_IMPORT_wisdom
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_fprint_plan(p,output_file) BIND(C, name='fftw_fprint_plan')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    TYPE(C_PTR), VALUE :: output_file
  END SUBROUTINE fftw_fprint_plan
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_print_plan(p) BIND(C, name='fftw_print_plan')
    IMPORT
    TYPE(C_PTR), VALUE :: p
  END SUBROUTINE fftw_print_plan
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_sprint_plan(p) BIND(C, name='fftw_sprint_plan')
    IMPORT
    TYPE(C_PTR), VALUE :: p
  END FUNCTION fftw_sprint_plan
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_malloc(n) BIND(C, name='fftw_malloc')
    IMPORT
    INTEGER(C_SIZE_T), VALUE :: n
  END FUNCTION fftw_malloc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_alloc_real(n) BIND(C, name='fftw_alloc_real')
    IMPORT
    INTEGER(C_SIZE_T), VALUE :: n
  END FUNCTION fftw_alloc_real
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftw_alloc_complex(n) &
    & BIND(C, name='fftw_alloc_complex')
    IMPORT
    INTEGER(C_SIZE_T), VALUE :: n
  END FUNCTION fftw_alloc_complex
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_free(p) BIND(C, name='fftw_free')
    IMPORT
    TYPE(C_PTR), VALUE :: p
  END SUBROUTINE fftw_free
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftw_flops(p,add,mul,fmas) BIND(C, name='fftw_flops')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    REAL(C_DOUBLE), INTENT(OUT) :: add
    REAL(C_DOUBLE), INTENT(OUT) :: mul
    REAL(C_DOUBLE), INTENT(OUT) :: fmas
  END SUBROUTINE fftw_flops
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  REAL(C_DOUBLE) FUNCTION fftw_estimate_cost(p) &
    & BIND(C, name='fftw_estimate_cost')
    IMPORT
    TYPE(C_PTR), VALUE :: p
  END FUNCTION fftw_estimate_cost
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  REAL(C_DOUBLE) FUNCTION fftw_cost(p) BIND(C, name='fftw_cost')
    IMPORT
    TYPE(C_PTR), VALUE :: p
  END FUNCTION fftw_cost
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  INTEGER(C_INT) FUNCTION fftw_alignment_of(p) &
    & BIND(C, name='fftw_alignment_of')
    IMPORT
    REAL(C_DOUBLE), DIMENSION(*), INTENT(OUT) :: p
  END FUNCTION fftw_alignment_of
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_dft(rank,n,in,out,sign,flags) &
    & BIND(C, name='fftwf_plan_dft')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_dft_1d(n,in,out,sign,flags) &
    & BIND(C, name='fftwf_plan_dft_1d')
    IMPORT
    INTEGER(C_INT), VALUE :: n
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_dft_1d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_dft_2d(n0,n1,in,out,sign,flags)  &
    & BIND(C, name='fftwf_plan_dft_2d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_dft_2d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_dft_3d(n0,n1,n2,in,out,sign,flags) &
    & BIND(C, name='fftwf_plan_dft_3d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    INTEGER(C_INT), VALUE :: n2
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_dft_3d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_many_dft(rank,n,howmany,in,inembed,istride,&
    & idist,out,onembed,ostride,odist,sign,flags) &
    & BIND(C, name='fftwf_plan_many_dft')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    INTEGER(C_INT), VALUE :: howmany
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: inembed
    INTEGER(C_INT), VALUE :: istride
    INTEGER(C_INT), VALUE :: idist
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: onembed
    INTEGER(C_INT), VALUE :: ostride
    INTEGER(C_INT), VALUE :: odist
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_many_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_guru_dft(rank,dims,howmany_rank,&
    & howmany_dims,in,out,sign,flags) &
    & BIND(C, name='fftwf_plan_guru_dft')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_guru_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_guru_split_dft(rank,dims,howmany_rank,&
    & howmany_dims,ri,ii,ro,io,flags) &
    & BIND(C, name='fftwf_plan_guru_split_dft')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ri
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ii
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ro
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: io
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_guru_split_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_guru64_dft(rank,dims,howmany_rank,&
    & howmany_dims,in,out,sign,flags) &
    & BIND(C, name='fftwf_plan_guru64_dft')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: sign
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_guru64_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_guru64_split_dft(rank,dims,howmany_rank,&
    & howmany_dims,ri,ii,ro,io,flags) &
    & BIND(C, name='fftwf_plan_guru64_split_dft')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ri
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ii
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ro
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: io
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_guru64_split_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftwf_execute_dft(p,in,out) BIND(C, name='fftwf_execute_dft')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(INOUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
  END SUBROUTINE fftwf_execute_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftwf_execute_split_dft(p,ri,ii,ro,io) &
    & BIND(C, name='fftwf_execute_split_dft')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    REAL(C_FLOAT), DIMENSION(*), INTENT(INOUT) :: ri
    REAL(C_FLOAT), DIMENSION(*), INTENT(INOUT) :: ii
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ro
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: io
  END SUBROUTINE fftwf_execute_split_dft
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_many_dft_r2c(rank,n,howmany,in,&
    & inembed,istride,idist,out,onembed,ostride,odist,flags) &
    & BIND(C, name='fftwf_plan_many_dft_r2c')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    INTEGER(C_INT), VALUE :: howmany
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: inembed
    INTEGER(C_INT), VALUE :: istride
    INTEGER(C_INT), VALUE :: idist
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: onembed
    INTEGER(C_INT), VALUE :: ostride
    INTEGER(C_INT), VALUE :: odist
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_many_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_dft_r2c(rank,n,in,out,flags) &
    & BIND(C, name='fftwf_plan_dft_r2c')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_dft_r2c_1d(n,in,out,flags) &
    & BIND(C, name='fftwf_plan_dft_r2c_1d')
    IMPORT
    INTEGER(C_INT), VALUE :: n
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_dft_r2c_1d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_dft_r2c_2d(n0,n1,in,out,flags) &
    & BIND(C, name='fftwf_plan_dft_r2c_2d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_dft_r2c_2d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_dft_r2c_3d(n0,n1,n2,in,out,flags) &
    & BIND(C, name='fftwf_plan_dft_r2c_3d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    INTEGER(C_INT), VALUE :: n2
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_dft_r2c_3d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_many_dft_c2r(rank,n,howmany,in,inembed,&
    & istride,idist,out,onembed,ostride,odist,flags) &
    & BIND(C, name='fftwf_plan_many_dft_c2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    INTEGER(C_INT), VALUE :: howmany
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: inembed
    INTEGER(C_INT), VALUE :: istride
    INTEGER(C_INT), VALUE :: idist
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: onembed
    INTEGER(C_INT), VALUE :: ostride
    INTEGER(C_INT), VALUE :: odist
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_many_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_dft_c2r(rank,n,in,out,flags) &
    & BIND(C, name='fftwf_plan_dft_c2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_dft_c2r_1d(n,in,out,flags) &
    & BIND(C, name='fftwf_plan_dft_c2r_1d')
    IMPORT
    INTEGER(C_INT), VALUE :: n
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_dft_c2r_1d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_dft_c2r_2d(n0,n1,in,out,flags) &
    & BIND(C, name='fftwf_plan_dft_c2r_2d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_dft_c2r_2d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_dft_c2r_3d(n0,n1,n2,in,out,flags) &
    & BIND(C, name='fftwf_plan_dft_c2r_3d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    INTEGER(C_INT), VALUE :: n2
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_dft_c2r_3d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_guru_dft_r2c(rank,dims,howmany_rank,&
    & howmany_dims,in,out,flags) &
    & BIND(C, name='fftwf_plan_guru_dft_r2c')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_guru_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_guru_dft_c2r(rank,dims,howmany_rank, &
    & howmany_dims,in,out,flags) &
    & BIND(C, name='fftwf_plan_guru_dft_c2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_guru_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_guru_split_dft_r2c(rank,dims,howmany_rank,&
    & howmany_dims,in,ro,io,flags) &
    & BIND(C, name='fftwf_plan_guru_split_dft_r2c')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ro
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: io
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_guru_split_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_guru_split_dft_c2r(rank,dims,&
    & howmany_rank,howmany_dims,ri,ii,out,flags) &
    & BIND(C, name='fftwf_plan_guru_split_dft_c2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ri
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ii
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_guru_split_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_guru64_dft_r2c(rank,dims,howmany_rank,&
    & howmany_dims,in,out,flags) &
    & BIND(C, name='fftwf_plan_guru64_dft_r2c')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_guru64_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_guru64_dft_c2r(rank,dims,howmany_rank,&
    & howmany_dims,in,out,flags) &
    & BIND(C, name='fftwf_plan_guru64_dft_c2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_guru64_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_guru64_split_dft_r2c(rank,dims, &
    & howmany_rank,howmany_dims,in,ro,io,flags) &
    & BIND(C, name='fftwf_plan_guru64_split_dft_r2c')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ro
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: io
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_guru64_split_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_guru64_split_dft_c2r(rank,dims,&
    & howmany_rank,howmany_dims,ri,ii,out,flags) &
    & BIND(C, name='fftwf_plan_guru64_split_dft_c2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: dims
    INTEGER(C_INT), VALUE :: howmany_rank
    TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ri
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ii
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_guru64_split_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftwf_execute_dft_r2c(p,in,out) &
    & BIND(C, name='fftwf_execute_dft_r2c')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    REAL(C_FLOAT), DIMENSION(*), INTENT(INOUT) :: in
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(OUT) :: out
  END SUBROUTINE fftwf_execute_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftwf_execute_dft_c2r(p,in,out) &
    & BIND(C, name='fftwf_execute_dft_c2r')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    COMPLEX(C_FLOAT_COMPLEX), DIMENSION(*), INTENT(INOUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
  END SUBROUTINE fftwf_execute_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftwf_execute_split_dft_r2c(p,in,ro,io) &
    & BIND(C, name='fftwf_execute_split_dft_r2c')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    REAL(C_FLOAT), DIMENSION(*), INTENT(INOUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: ro
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: io
  END SUBROUTINE fftwf_execute_split_dft_r2c
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE fftwf_execute_split_dft_c2r(p,ri,ii,out) &
    & BIND(C, name='fftwf_execute_split_dft_c2r')
    IMPORT
    TYPE(C_PTR), VALUE :: p
    REAL(C_FLOAT), DIMENSION(*), INTENT(INOUT) :: ri
    REAL(C_FLOAT), DIMENSION(*), INTENT(INOUT) :: ii
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
  END SUBROUTINE fftwf_execute_split_dft_c2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_many_r2r(rank,n,howmany,in,inembed,&
    & istride,idist,out,onembed,ostride,odist,kind,flags) &
    & BIND(C, name='fftwf_plan_many_r2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    INTEGER(C_INT), VALUE :: howmany
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: inembed
    INTEGER(C_INT), VALUE :: istride
    INTEGER(C_INT), VALUE :: idist
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: onembed
    INTEGER(C_INT), VALUE :: ostride
    INTEGER(C_INT), VALUE :: odist
    INTEGER(C_FFTW_R2R_KIND), DIMENSION(*), INTENT(IN) :: kind
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_many_r2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_r2r(rank,n,in,out,kind,flags) &
    & BIND(C, name='fftwf_plan_r2r')
    IMPORT
    INTEGER(C_INT), VALUE :: rank
    INTEGER(C_INT), DIMENSION(*), INTENT(IN) :: n
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_FFTW_R2R_KIND), DIMENSION(*), INTENT(IN) :: kind
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_r2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_r2r_1d(n,in,out,kind,flags) &
    & BIND(C, name='fftwf_plan_r2r_1d')
    IMPORT
    INTEGER(C_INT), VALUE :: n
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_FFTW_R2R_KIND), VALUE :: kind
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_r2r_1d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_r2r_2d(n0,n1,in,out,kind0,kind1,flags) &
    & BIND(C, name='fftwf_plan_r2r_2d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_FFTW_R2R_KIND), VALUE :: kind0
    INTEGER(C_FFTW_R2R_KIND), VALUE :: kind1
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_r2r_2d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  TYPE(C_PTR) FUNCTION fftwf_plan_r2r_3d(n0,n1,n2,in,out,kind0, &
    & kind1,kind2,flags) BIND(C, name='fftwf_plan_r2r_3d')
    IMPORT
    INTEGER(C_INT), VALUE :: n0
    INTEGER(C_INT), VALUE :: n1
    INTEGER(C_INT), VALUE :: n2
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
    REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    INTEGER(C_FFTW_R2R_KIND), VALUE :: kind0
    INTEGER(C_FFTW_R2R_KIND), VALUE :: kind1
    INTEGER(C_FFTW_R2R_KIND), VALUE :: kind2
    INTEGER(C_INT), VALUE :: flags
  END FUNCTION fftwf_plan_r2r_3d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    TYPE(C_PTR) FUNCTION fftwf_plan_guru_r2r(rank,dims,howmany_rank,howmany_dims,in,out,kind,flags) &
                         BIND(C, name='fftwf_plan_guru_r2r')
      IMPORT
      INTEGER(C_INT), VALUE :: rank
      TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: dims
      INTEGER(C_INT), VALUE :: howmany_rank
      TYPE(fftwf_iodim), DIMENSION(*), INTENT(IN) :: howmany_dims
      REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
      REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
      INTEGER(C_FFTW_R2R_KIND), DIMENSION(*), INTENT(IN) :: kind
      INTEGER(C_INT), VALUE :: flags
    END FUNCTION fftwf_plan_guru_r2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    TYPE(C_PTR) FUNCTION fftwf_plan_guru64_r2r(rank,dims,howmany_rank,howmany_dims,in,out,kind,flags) &
                         BIND(C, name='fftwf_plan_guru64_r2r')
      IMPORT
      INTEGER(C_INT), VALUE :: rank
      TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: dims
      INTEGER(C_INT), VALUE :: howmany_rank
      TYPE(fftwf_iodim64), DIMENSION(*), INTENT(IN) :: howmany_dims
      REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: in
      REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
      INTEGER(C_FFTW_R2R_KIND), DIMENSION(*), INTENT(IN) :: kind
      INTEGER(C_INT), VALUE :: flags
    END FUNCTION fftwf_plan_guru64_r2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    SUBROUTINE fftwf_execute_r2r(p,in,out) BIND(C, name='fftwf_execute_r2r')
      IMPORT
      TYPE(C_PTR), VALUE :: p
      REAL(C_FLOAT), DIMENSION(*), INTENT(INOUT) :: in
      REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: out
    END SUBROUTINE fftwf_execute_r2r
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    SUBROUTINE fftwf_destroy_plan(p) BIND(C, name='fftwf_destroy_plan')
      IMPORT
      TYPE(C_PTR), VALUE :: p
    END SUBROUTINE fftwf_destroy_plan
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    SUBROUTINE fftwf_forget_wisdom() BIND(C, name='fftwf_forget_wisdom')
      IMPORT
    END SUBROUTINE fftwf_forget_wisdom
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    SUBROUTINE fftwf_cleanup() BIND(C, name='fftwf_cleanup')
      IMPORT
    END SUBROUTINE fftwf_cleanup
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    SUBROUTINE fftwf_set_timelimit(t) BIND(C, name='fftwf_set_timelimit')
      IMPORT
      REAL(C_DOUBLE), VALUE :: t
    END SUBROUTINE fftwf_set_timelimit
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    SUBROUTINE fftwf_plan_with_nthreads(nthreads) BIND(C, name='fftwf_plan_with_nthreads')
      IMPORT
      INTEGER(C_INT), VALUE :: nthreads
    END SUBROUTINE fftwf_plan_with_nthreads
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    INTEGER(C_INT) FUNCTION fftwf_planner_nthreads() BIND(C, name='fftwf_planner_nthreads')
      IMPORT
    END FUNCTION fftwf_planner_nthreads
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    INTEGER(C_INT) FUNCTION fftwf_init_threads() BIND(C, name='fftwf_init_threads')
      IMPORT
    END FUNCTION fftwf_init_threads
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    SUBROUTINE fftwf_cleanup_threads() BIND(C, name='fftwf_cleanup_threads')
      IMPORT
    END SUBROUTINE fftwf_cleanup_threads
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    ! Unable to generate Fortran interface for fftwf_threads_set_callback
    SUBROUTINE fftwf_make_planner_thread_safe() BIND(C, name='fftwf_make_planner_thread_safe')
      IMPORT
    END SUBROUTINE fftwf_make_planner_thread_safe
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    INTEGER(C_INT) FUNCTION fftwf_export_wisdom_to_filename(filename) BIND(C, name='fftwf_export_wisdom_to_filename')
      IMPORT
      CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: filename
    END FUNCTION fftwf_export_wisdom_to_filename
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    SUBROUTINE fftwf_export_wisdom_to_file(output_file) BIND(C, name='fftwf_export_wisdom_to_file')
      IMPORT
      TYPE(C_PTR), VALUE :: output_file
    END SUBROUTINE fftwf_export_wisdom_to_file
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    TYPE(C_PTR) FUNCTION fftwf_export_wisdom_to_string() BIND(C, name='fftwf_export_wisdom_to_string')
      IMPORT
    END FUNCTION fftwf_export_wisdom_to_string
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    SUBROUTINE fftwf_export_wisdom(write_char,data) BIND(C, name='fftwf_export_wisdom')
      IMPORT
      TYPE(C_FUNPTR), VALUE :: write_char
      TYPE(C_PTR), VALUE :: data
    END SUBROUTINE fftwf_export_wisdom
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    INTEGER(C_INT) FUNCTION fftwf_IMPORT_system_wisdom() BIND(C, name='fftwf_IMPORT_system_wisdom')
      IMPORT
    END FUNCTION fftwf_IMPORT_system_wisdom
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    INTEGER(C_INT) FUNCTION fftwf_IMPORT_wisdom_from_filename(filename) BIND(C, name='fftwf_IMPORT_wisdom_from_filename')
      IMPORT
      CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: filename
    END FUNCTION fftwf_IMPORT_wisdom_from_filename
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    INTEGER(C_INT) FUNCTION fftwf_IMPORT_wisdom_from_file(input_file) BIND(C, name='fftwf_IMPORT_wisdom_from_file')
      IMPORT
      TYPE(C_PTR), VALUE :: input_file
    END FUNCTION fftwf_IMPORT_wisdom_from_file
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    INTEGER(C_INT) FUNCTION fftwf_IMPORT_wisdom_from_string(input_string) BIND(C, name='fftwf_IMPORT_wisdom_from_string')
      IMPORT
      CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: input_string
    END FUNCTION fftwf_IMPORT_wisdom_from_string
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    INTEGER(C_INT) FUNCTION fftwf_IMPORT_wisdom(read_char,data) BIND(C, name='fftwf_IMPORT_wisdom')
      IMPORT
      TYPE(C_FUNPTR), VALUE :: read_char
      TYPE(C_PTR), VALUE :: data
    END FUNCTION fftwf_IMPORT_wisdom
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    SUBROUTINE fftwf_fprint_plan(p,output_file) BIND(C, name='fftwf_fprint_plan')
      IMPORT
      TYPE(C_PTR), VALUE :: p
      TYPE(C_PTR), VALUE :: output_file
    END SUBROUTINE fftwf_fprint_plan
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    SUBROUTINE fftwf_print_plan(p) BIND(C, name='fftwf_print_plan')
      IMPORT
      TYPE(C_PTR), VALUE :: p
    END SUBROUTINE fftwf_print_plan
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    TYPE(C_PTR) FUNCTION fftwf_sprint_plan(p) BIND(C, name='fftwf_sprint_plan')
      IMPORT
      TYPE(C_PTR), VALUE :: p
    END FUNCTION fftwf_sprint_plan
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    TYPE(C_PTR) FUNCTION fftwf_malloc(n) BIND(C, name='fftwf_malloc')
      IMPORT
      INTEGER(C_SIZE_T), VALUE :: n
    END FUNCTION fftwf_malloc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    TYPE(C_PTR) FUNCTION fftwf_alloc_real(n) BIND(C, name='fftwf_alloc_real')
      IMPORT
      INTEGER(C_SIZE_T), VALUE :: n
    END FUNCTION fftwf_alloc_real
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    TYPE(C_PTR) FUNCTION fftwf_alloc_complex(n) BIND(C, name='fftwf_alloc_complex')
      IMPORT
      INTEGER(C_SIZE_T), VALUE :: n
    END FUNCTION fftwf_alloc_complex
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    SUBROUTINE fftwf_free(p) BIND(C, name='fftwf_free')
      IMPORT
      TYPE(C_PTR), VALUE :: p
    END SUBROUTINE fftwf_free
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    SUBROUTINE fftwf_flops(p,add,mul,fmas) BIND(C, name='fftwf_flops')
      IMPORT
      TYPE(C_PTR), VALUE :: p
      REAL(C_DOUBLE), INTENT(OUT) :: add
      REAL(C_DOUBLE), INTENT(OUT) :: mul
      REAL(C_DOUBLE), INTENT(OUT) :: fmas
    END SUBROUTINE fftwf_flops
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    REAL(C_DOUBLE) FUNCTION fftwf_estimate_cost(p) BIND(C, name='fftwf_estimate_cost')
      IMPORT
      TYPE(C_PTR), VALUE :: p
    END FUNCTION fftwf_estimate_cost
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    REAL(C_DOUBLE) FUNCTION fftwf_cost(p) BIND(C, name='fftwf_cost')
      IMPORT
      TYPE(C_PTR), VALUE :: p
    END FUNCTION fftwf_cost
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
    INTEGER(C_INT) FUNCTION fftwf_alignment_of(p) BIND(C, name='fftwf_alignment_of')
      IMPORT
      REAL(C_FLOAT), DIMENSION(*), INTENT(OUT) :: p
    END FUNCTION fftwf_alignment_of
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FFTW3
#endif