!-----------------------------------------------------------------------
!     purpose:
!     --------
!     this function returns the element a(i,j) of a matrix a,
!     for any pair (i,j). the matrix is assumed to be stored
!     in compressed sparse row (csr) format. getelm performs a
!     binary search in the case where it is known that the elements
!     are sorted so that the column indices are in increasing order.
!     also returns (in iadd) the address of the element a(i,j) in
!     arrays a and ja when the search is successsful (zero IF not).
!-----
!     first contributed by noel nachtigal (mit).
!     recoded jan. 20, 1991, by y. saad [in particular
!     added handling of the non-sorted case + the iadd output]
!-----------------------------------------------------------------------
!     parameters:
!     -----------
! on entry:
!----------
!     i      = the row index of the element sought (input).
!     j      = the column index of the element sought (input).
!     a      = the matrix a in compressed sparse row format (input).
!     ja     = the array of column indices (input).
!     ia     = the array of pointers to the rows' data (input).
!     sorted = logical indicating whether the matrix is knonw to
!              have its column indices sorted in increasing order
!              (sorted=.true.) or not (sorted=.false.).
!              (input).
! on return:
!-----------
!     getelm = value of a(i,j).
!     iadd   = address of element a(i,j) in arrays a, ja IF found,
!              zero IF not found. (output)
!
!     note: the inputs i and j are not checked for validity.
!-----------------------------------------------------------------------
!     noel m. nachtigal october 28, 1990 -- youcef saad jan 20, 1991.
!-----------------------------------------------------------------------

PURE FUNCTION getelm_saad( i, j, a, ja, ia, sorted ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  INTEGER( I4B ), INTENT( IN ) :: ia( : )
  INTEGER( I4B ), INTENT( IN ) :: j
  INTEGER( I4B ), INTENT( IN ) :: ja( : )
  REAL( DFP ), INTENT( IN ) ::  a( : )
  LOGICAL( LGT ), INTENT( IN ) :: sorted
  REAL( DFP ) :: Ans

  ! Internal variables
  integer :: iadd, ibeg, iend, imid, k

  iadd = 0
  Ans = 0.0
  ibeg = ia(i)
  iend = ia(i+1)-1

  IF( .NOT. sorted ) THEN
    DO k = ibeg, iend
      IF( ja(k) .eq. j ) THEN
        Ans = a( k )
        EXIT
      ENDIF
    END DO

  ELSE

    DO
      imid = ( ibeg + iend ) / 2
      IF( ja(imid) .eq. j ) THEN
        Ans = a(imid)
        EXIT
      ENDIF

      IF( ibeg .ge. iend ) EXIT

      IF( ja(imid) .gt. j ) THEN
        iend = imid -1
      ELSE
        ibeg = imid +1
      ENDIF
    END DO
  ENDIF

END FUNCTION getelm_saad