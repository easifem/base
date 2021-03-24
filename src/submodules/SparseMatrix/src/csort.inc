!-----------------------------------------------------------------------
! This routine sorts the elements of  a matrix (stored in Compressed
! Sparse Row Format) in increasing order of their column indices within
! each row. It uses insertion sort
!-----------------------------------------------------------------------
! on entry:
!---------
! n     = the row dimension of the matrix
! a     = the matrix A in compressed sparse row format.
! ja    = the array of column indices of the elements in array a.
! ia    = the array of pointers to the rows.
! values= logical indicating whether or not the real values a(*) must
!         also be permuted. IF (.not. values) then the array a is not
!         touched by csort and can be a dummy array.
!
! on return:
!----------
! the matrix stored in the structure a, ja, ia is permuted in such a
! way that the column indices are in increasing order within each row.
! iwork(1:nnz) contains the permutation used  to rearrange the elements.
!-----------------------------------------------------------------------
! Y. Saad - recoded Dec. 20th -  2017 -
!-----------------------------------------------------------------------
! local variables

!----------------------------------------------------------------------------
!                                                                      CSORT
!----------------------------------------------------------------------------

! This is modified version of CSORT of SAAD

PURE SUBROUTINE CSORT_SAAD (a,ja,ia,values)
  REAL( DFP ), INTENT( INOUT ) ::  a( : )
  INTEGER( I4B ), INTENT( INOUT ) :: ja( : )
  INTEGER( I4B ), INTENT( INOUT ) :: ia( : )
  LOGICAL( LGT ), INTENT( IN ) :: values

  ! Internal variables
  INTEGER( I4B ) :: row, i, k, j, n
  REAL( DFP ) :: rj

  n = SIZE( ia ) - 1
  rj = 0.0
  DO row=1, n
    DO k=ia(row)+1, ia(row+1)-1
      j = ja(k)
      IF (values) rj = a(k)
      i = k-1;
      DO WHILE ( (i .GE. ia(row)) .AND. (j < ja(i)) )
        ja(i+1) = ja(i)
        IF (values) a(i+1) = a(i)
        i = i-1
        IF (i .EQ. 0) EXIT
      ENDDO
      ja(i+1) = j
      IF (values) a(i+1) = rj
    ENDDO
  ENDDO
END SUBROUTINE CSORT_SAAD