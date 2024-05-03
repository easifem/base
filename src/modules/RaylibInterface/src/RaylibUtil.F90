! raylib_util.f90
!
! Utility procedures for C inter-operability with raylib.
!
! Author:  Philipp Engel
! Licence: ISC

MODULE raylib_util
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE(TYPE, EXTERNAL)
PRIVATE

INTERFACE
  FUNCTION c_strlen(str) BIND(c, name='strlen')
    IMPORT :: C_PTR, C_SIZE_T
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(in), VALUE :: str
    INTEGER(kind=C_SIZE_T) :: c_strlen
  END FUNCTION c_strlen
END INTERFACE

PUBLIC :: c_f_str_ptr
CONTAINS
SUBROUTINE c_f_str_ptr(c_str, f_str)
        !! Copies a C string, passed as a C pointer, to a Fortran string.
  TYPE(C_PTR), INTENT(in) :: c_str
  CHARACTER(:), ALLOCATABLE, INTENT(out) :: f_str

  CHARACTER(kind=C_CHAR), POINTER :: ptrs(:)
  INTEGER(kind=C_SIZE_T) :: i, sz

  copy_block: BLOCK
    IF (.NOT. C_ASSOCIATED(c_str)) EXIT copy_block
    sz = c_strlen(c_str)
    IF (sz < 0) EXIT copy_block
    CALL C_F_POINTER(c_str, ptrs, [sz])
    ALLOCATE (CHARACTER(len=sz) :: f_str)

    DO i = 1, sz
      f_str(i:i) = ptrs(i)
    END DO

    RETURN
  END BLOCK copy_block

  IF (.NOT. ALLOCATED(f_str)) f_str = ''
END SUBROUTINE c_f_str_ptr
END MODULE raylib_util
