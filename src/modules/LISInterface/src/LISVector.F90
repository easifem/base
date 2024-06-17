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

MODULE LISVector
USE ISO_C_BINDING
USE GlobalData, ONLY: I4B, DFP, LGT
USE Display_Method, ONLY: Display
IMPLICIT NONE
PRIVATE

! struct LIS_VECTOR_STRUCT
! {
!         LIS_INT label;
!         LIS_INT status;
!         LIS_INT precision;
!         LIS_INT gn;
!         LIS_INT n;
!         LIS_INT np;
!         LIS_INT pad;
!         LIS_INT origin;
!         LIS_INT is_copy;
!         LIS_INT is_destroy;
!         LIS_INT is_scaled;
!         LIS_INT my_rank;
!         LIS_INT nprocs;
!         LIS_Comm comm;
!         LIS_INT is;
!         LIS_INT ie;
!         LIS_INT *ranges;
!         LIS_SCALAR *value;
!         LIS_SCALAR *value_lo;
!         LIS_SCALAR *work;
!         LIS_INT intvalue;
! };
! typedef struct LIS_VECTOR_STRUCT *LIS_VECTOR;

TYPE, BIND(C) :: LIS_VECTOR
  INTEGER(I4B) :: label; 
  INTEGER(I4B) :: status; 
  INTEGER(I4B) :: PRECISION; 
  INTEGER(I4B) :: gn; 
  INTEGER(I4B) :: n; 
  INTEGER(I4B) :: np; 
  INTEGER(I4B) :: pad; 
  INTEGER(I4B) :: origin; 
  INTEGER(I4B) :: is_copy; 
  INTEGER(I4B) :: is_destroy; 
  INTEGER(I4B) :: is_scaled; 
  INTEGER(I4B) :: my_rank; 
  INTEGER(I4B) :: nprocs; 
  INTEGER(I4B) :: comm; 
  INTEGER(I4B) :: is; 
  INTEGER(I4B) :: ie; 
  TYPE(C_PTR) :: ranges; 
  TYPE(C_PTR) :: VALUE; 
  TYPE(C_PTR) :: value_lo; 
  TYPE(C_PTR) :: work; 
  INTEGER(I4B) :: intvalue; 
END TYPE LIS_VECTOR

PUBLIC :: LIS_VECTOR

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Display
  MODULE PROCEDURE display_lisvector
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_create(LIS_Comm comm, LIS_VECTOR *vec);

INTERFACE
  FUNCTION lis_vector_create(comm, vec) BIND(C, name="lis_vector_create")
    IMPORT :: LIS_VECTOR, I4B
    INTEGER(I4B), VALUE, INTENT(in) :: comm
    TYPE(LIS_VECTOR), INTENT(INOUT) :: vec
    INTEGER(I4B) :: lis_vector_create
  END FUNCTION lis_vector_create
END INTERFACE

PUBLIC :: lis_vector_create

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_set_size(LIS_VECTOR vec, LIS_INT local_n, &
! LIS_INT global_n);

INTERFACE
  FUNCTION lis_vector_set_size(vec, local_n, global_n) &
    & BIND(C, name="lis_vector_set_size")
    IMPORT :: LIS_VECTOR, I4B, C_PTR
    TYPE(LIS_VECTOR), INTENT(INOUT) :: vec
    INTEGER(I4B), VALUE, INTENT(IN) :: local_n
    INTEGER(I4B), VALUE, INTENT(IN) :: global_n
    INTEGER(I4B) :: lis_vector_set_size
  END FUNCTION lis_vector_set_size
END INTERFACE

PUBLIC :: lis_vector_set_size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_psd_reset_scale(LIS_VECTOR vec);

INTERFACE
  FUNCTION lis_vector_psd_reset_scale(vec) &
    & BIND(C, name="lis_vector_psd_reset_scale")
    IMPORT :: LIS_VECTOR, I4B
    TYPE(LIS_VECTOR), INTENT(INOUT) :: vec
    INTEGER(I4B) :: lis_vector_psd_reset_scale
  END FUNCTION lis_vector_psd_reset_scale
END INTERFACE

PUBLIC :: lis_vector_psd_reset_scale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_destroy(LIS_VECTOR vec);

INTERFACE
  FUNCTION lis_vector_destroy(vec) &
    & BIND(C, name="lis_vector_destroy")
    IMPORT :: LIS_VECTOR, I4B
    TYPE(LIS_VECTOR), INTENT(INOUT) :: vec
    INTEGER(I4B) :: lis_vector_destroy
  END FUNCTION lis_vector_destroy
END INTERFACE

PUBLIC :: lis_vector_destroy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_duplicate(void *vin, LIS_VECTOR *vout);

INTERFACE
 FUNCTION lis_vector_duplicate(vin, vout) BIND(C, name="lis_vector_duplicate")
    IMPORT :: LIS_VECTOR, C_PTR, I4B
    TYPE(C_PTR), INTENT(IN) :: vin
    TYPE(LIS_VECTOR), INTENT(INOUT) :: vout
    INTEGER(I4B) :: lis_vector_duplicate
  END FUNCTION lis_vector_duplicate
END INTERFACE

PUBLIC :: lis_vector_duplicate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_get_size(LIS_VECTOR v, LIS_INT *local_n, LIS_INT *global_n);

INTERFACE
  FUNCTION lis_vector_get_size(v, local_n, global_n) &
    & BIND(C, name="lis_vector_get_size")
    IMPORT :: LIS_VECTOR, I4B
    TYPE(LIS_VECTOR), INTENT(INOUT) :: v
    INTEGER(I4B), INTENT(INOUT) :: local_n
    INTEGER(I4B), INTENT(INOUT) :: global_n
    INTEGER(I4B) :: lis_vector_get_size
  END FUNCTION lis_vector_get_size
END INTERFACE

PUBLIC :: lis_vector_get_size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_get_range(LIS_VECTOR v, LIS_INT *is, LIS_INT *ie);

INTERFACE
  FUNCTION lis_vector_get_range(v, is, ie) &
    & BIND(C, name="lis_vector_get_range")
    IMPORT :: LIS_VECTOR, I4B
    TYPE(LIS_VECTOR), INTENT(INOUT) :: v
    INTEGER(I4B), INTENT(INOUT) :: is
    INTEGER(I4B), INTENT(INOUT) :: ie
    INTEGER(I4B) :: lis_vector_get_range
  END FUNCTION lis_vector_get_range
END INTERFACE

PUBLIC :: lis_vector_get_range

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_get_value(LIS_VECTOR v, LIS_INT i, LIS_SCALAR *value);

INTERFACE
  FUNCTION lis_vector_get_value(v, i, VALUE) &
    & BIND(C, name="lis_vector_get_value")
    IMPORT :: LIS_VECTOR, I4B, DFP
    TYPE(LIS_VECTOR), INTENT(INOUT) :: v
    INTEGER(I4B), VALUE, INTENT(IN) :: i
    REAL(DFP), INTENT(INOUT) :: VALUE
    INTEGER(I4B) :: lis_vector_get_value
  END FUNCTION lis_vector_get_value
END INTERFACE

PUBLIC :: lis_vector_get_value

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_get_values(LIS_VECTOR v, LIS_INT start, LIS_INT count, LIS_SCALAR value[]);

INTERFACE
  FUNCTION lis_vector_get_values(v, start, count, VALUE) &
    & BIND(C, name="lis_vector_get_values")
    IMPORT :: LIS_VECTOR, I4B, DFP
    TYPE(LIS_VECTOR), INTENT(INOUT) :: v
    INTEGER(I4B), VALUE, INTENT(IN) :: start
    INTEGER(I4B), VALUE, INTENT(IN) :: count
    REAL(DFP), INTENT(INOUT) :: VALUE(*)
    INTEGER(I4B) :: lis_vector_get_values
  END FUNCTION lis_vector_get_values
END INTERFACE

PUBLIC :: lis_vector_get_values

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_set_value(LIS_INT flag, LIS_INT i, LIS_SCALAR value, LIS_VECTOR v);

INTERFACE
  FUNCTION lis_vector_set_value(flag, i, VALUE, v) &
    & BIND(C, name="lis_vector_set_value")
    IMPORT :: LIS_VECTOR, I4B, DFP
    INTEGER(I4B), VALUE, INTENT(IN) :: flag
    INTEGER(I4B), VALUE, INTENT(IN) :: i
    REAL(DFP), VALUE, INTENT(IN) :: VALUE
    TYPE(LIS_VECTOR), INTENT(INOUT) :: v
    INTEGER(I4B) :: lis_vector_set_value
  END FUNCTION lis_vector_set_value
END INTERFACE

PUBLIC :: lis_vector_set_value

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_set_values(LIS_INT flag, LIS_INT count, LIS_INT index[], LIS_SCALAR value[], LIS_VECTOR v);

INTERFACE
  FUNCTION lis_vector_set_values(flag, count, index, VALUE, v) &
    & BIND(C, name="lis_vector_set_values")
    IMPORT :: LIS_VECTOR, I4B, DFP
    INTEGER(I4B), VALUE, INTENT(IN) :: flag
    INTEGER(I4B), VALUE, INTENT(IN) :: count
    INTEGER(I4B), INTENT(IN) :: INDEX(count)
    REAL(DFP), INTENT(IN) :: VALUE(count)
    TYPE(LIS_VECTOR), INTENT(INOUT) :: v
    INTEGER(I4B) :: lis_vector_set_values
  END FUNCTION lis_vector_set_values
END INTERFACE

PUBLIC :: lis_vector_set_values

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_set_values2(LIS_INT flag, LIS_INT start, LIS_INT count, LIS_SCALAR value[], LIS_VECTOR v);

INTERFACE
  FUNCTION lis_vector_set_values2(flag, start, count, VALUE, v) &
    & BIND(C, name="lis_vector_set_values2")
    IMPORT :: LIS_VECTOR, I4B, DFP
    INTEGER(I4B), VALUE, INTENT(IN) :: flag
    INTEGER(I4B), VALUE, INTENT(IN) :: start
    INTEGER(I4B), VALUE, INTENT(IN) :: count
    REAL(DFP), INTENT(IN) :: VALUE(count)
    TYPE(LIS_VECTOR), INTENT(INOUT) :: v
    INTEGER(I4B) :: lis_vector_set_values2
  END FUNCTION lis_vector_set_values2
END INTERFACE

PUBLIC :: lis_vector_set_values2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_print(LIS_VECTOR x);

INTERFACE
  FUNCTION lis_vector_print(x) &
    & BIND(C, name="lis_vector_print")
    IMPORT :: LIS_VECTOR, I4B
    TYPE(LIS_VECTOR), VALUE, INTENT(IN) :: x
    INTEGER(I4B) :: lis_vector_print
  END FUNCTION lis_vector_print
END INTERFACE
!
PUBLIC :: lis_vector_print

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern LIS_INT lis_vector_scatter(LIS_SCALAR value[], LIS_VECTOR v);

INTERFACE
  FUNCTION lis_vector_scatter(VALUE, v) &
    & BIND(C, name="lis_vector_scatter")
    IMPORT :: LIS_VECTOR, I4B, DFP
    REAL(DFP), INTENT(IN) :: VALUE(*)
    TYPE(LIS_VECTOR), INTENT(INOUT) :: v
    INTEGER(I4B) :: lis_vector_scatter
  END FUNCTION lis_vector_scatter
END INTERFACE

PUBLIC :: lis_vector_scatter

! extern LIS_INT lis_vector_gather(LIS_VECTOR v, LIS_SCALAR value[]);
! extern LIS_INT lis_vector_is_null(LIS_VECTOR v);
! extern LIS_INT lis_vector_swap(LIS_VECTOR vsrc, LIS_VECTOR vdst);
! extern LIS_INT lis_vector_copy(LIS_VECTOR vsrc, LIS_VECTOR vdst);
! extern LIS_INT lis_vector_axpy(LIS_SCALAR alpha, LIS_VECTOR vx, LIS_VECTOR vy);
! extern LIS_INT lis_vector_xpay(LIS_VECTOR vx, LIS_SCALAR alpha, LIS_VECTOR vy);
! extern LIS_INT lis_vector_axpyz(LIS_SCALAR alpha, LIS_VECTOR vx, LIS_VECTOR vy, LIS_VECTOR vz);
! extern LIS_INT lis_vector_scale(LIS_SCALAR alpha, LIS_VECTOR vx);
! extern LIS_INT lis_vector_pmul(LIS_VECTOR vx,LIS_VECTOR vy,LIS_VECTOR vz);
! extern LIS_INT lis_vector_pdiv(LIS_VECTOR vx,LIS_VECTOR vy,LIS_VECTOR vz);
! extern LIS_INT lis_vector_set_all(LIS_SCALAR alpha, LIS_VECTOR vx);
! extern LIS_INT lis_vector_abs(LIS_VECTOR vx);
! extern LIS_INT lis_vector_reciprocal(LIS_VECTOR vx);
! extern LIS_INT lis_vector_conjugate(LIS_VECTOR vx);
! extern LIS_INT lis_vector_shift(LIS_SCALAR sigma, LIS_VECTOR vx);
! extern LIS_INT lis_vector_dot(LIS_VECTOR vx, LIS_VECTOR vy, LIS_SCALAR *value);
! extern LIS_INT lis_vector_nhdot(LIS_VECTOR vx, LIS_VECTOR vy, LIS_SCALAR *value);
! extern LIS_INT lis_vector_nrm1(LIS_VECTOR vx, LIS_REAL *value);
! extern LIS_INT lis_vector_nrm2(LIS_VECTOR vx, LIS_REAL *value);
! extern LIS_INT lis_vector_nrmi(LIS_VECTOR vx, LIS_REAL *value);
! extern LIS_INT lis_vector_sum(LIS_VECTOR vx, LIS_SCALAR *value);

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE display_lisvector(obj, msg, unitno)
  TYPE(LIS_VECTOR), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  REAL(DFP), POINTER :: VALUE(:)

  CALL Display(obj%label, "label = ", unitno)
  CALL Display(obj%status, "status = ", unitno)
  CALL Display(obj%PRECISION, "PRECISION = ", unitno)
  CALL Display(obj%gn, "gn = ", unitno)
  CALL Display(obj%n, "n = ", unitno)
  CALL Display(obj%np, "np = ", unitno)
  CALL Display(obj%pad, "pad= ", unitno)
  CALL Display(obj%origin, "origin= ", unitno)
  CALL Display(obj%is_copy, "is_copy= ", unitno)
  CALL Display(obj%is_destroy, "is_destroy= ", unitno)
  CALL Display(obj%is_scaled, "is_scaled= ", unitno)
  CALL Display(obj%my_rank, "my_rank= ", unitno)
  CALL Display(obj%nprocs, "nprocs= ", unitno)
  CALL Display(obj%comm, "comm= ", unitno)
  CALL Display(obj%is, "is= ", unitno)
  CALL Display(obj%ie, "ie= ", unitno)
  CALL Display(obj%intvalue, "intvalue= ", unitno)

  IF (C_ASSOCIATED(obj%ranges)) THEN
    CALL Display("ranges is associated", unitno)
  ELSE
    CALL Display("ranges is NOT associated", unitno)
  END IF

  IF (C_ASSOCIATED(obj%VALUE)) THEN
    CALL Display("VALUE is associated", unitno)
    CALL C_F_POINTER(obj%VALUE, VALUE, [obj%n])
    IF (obj%n .LE. 10) THEN
      CALL Display(VALUE, "value = ", unitno)
    ELSE
      CALL Display(VALUE(1:10), "value(1:10) = ", unitno)
    END IF
    NULLIFY (VALUE)
  ELSE
    CALL Display("VALUE is NOT associated", unitno)
  END IF

  IF (C_ASSOCIATED(obj%value_lo)) THEN
    CALL Display("value_lo is associated", unitno)
  ELSE
    CALL Display("value_lo is NOT associated", unitno)
  END IF

  IF (C_ASSOCIATED(obj%work)) THEN
    CALL Display("work is associated", unitno)
  ELSE
    CALL Display("work is NOT associated", unitno)
  END IF

END SUBROUTINE display_lisvector

END MODULE LISVector
