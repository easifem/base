
!----------------------------------------------------------------------------
!                                                            Expand@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary: Expand the array in row or column dim
!
!# Introduction
! Expand an array in col or row dim, and add an element.
! if dim = 1, then expand the array in row dimension
! if dim = 2, then expand the array in col dimension

INTERFACE
  MODULE PURE SUBROUTINE expand_r2_int32(mat, n, chunk_size, dim, &
    & val, finished)
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: mat(:,:)
    !! expandable matrix
    INTEGER(I4B), INTENT(INOUT) :: n
    !! counter for last element added to `mat`.
    !! must be initialized to `size(mat,dim)`
    !! (or 0 if not allocated) before first call
    INTEGER(I4B), INTENT(IN) :: chunk_size
    !! allocate `mat` in blocks of this size (>0)
    !! expansion will occur in row if dim=1
    !! expansion will occur in col if dim=2
    INTEGER( I4B ), INTENT( IN ) :: dim
    !! dimension of expansion, and addition
    INTEGER(INT32), OPTIONAL, INTENT(IN) :: val(:)
    !! the row or col vector to be added
    !! if dim = 1, then val is a row
    !! if dim = 2, then val is col
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: finished
    !! set to true to return `mat`
    !! as its correct size (`n`)
  END SUBROUTINE expand_r2_int32
END INTERFACE

INTERFACE EXPAND
  MODULE PROCEDURE expand_r2_int32
END INTERFACE EXPAND
