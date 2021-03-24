!-----------------------------------------------------------------------
! extracts the submatrix A(i1:i2,j1:j2) and puts the result in
! matrix ao,iao,jao
!---- In place: ao,jao,iao may be the same as a,ja,ia.
!--------------
! on input
!---------
! n	= row dimension of the matrix
! i1,i2 = two integers with i2 .GE. i1 indicating the range of rows to be
!          extracted.
! j1,j2 = two integers with j2 .GE. j1 indicating the range of columns
!         to be extracted.
!         * There is no checking whether the input values for i1, i2, j1,
!           j2 are between 1 and n.
! a,
! ja,
! ia    = matrix in compressed sparse row format.
!
! job	= job indicator: IF job .ne. 1 then the real values in a are NOT
!         extracted, only the column indices (i.e. data structure) are.
!         otherwise values as well as column indices are extracted...
!
! on output
!--------------
! nr	= number of rows of submatrix
! nc	= number of columns of submatrix
!	  * IF either of nr or nc is nonpositive the code will quit.
!
! ao,
! jao,iao = extracted matrix in general sparse format with jao containing
!	the column indices,and iao being the pointer to the beginning
!	of the row,in arrays a,ja.
!----------------------------------------------------------------------c
!           Y. Saad, Sep. 21 1989                                      c
!----------------------------------------------------------------------c

SUBROUTINE submat_saad( job,i1,i2,j1,j2,a,ja,ia,nr,nc,ao,jao,iao )
  INTEGER( I4B ), INTENT( IN ) :: job
  INTEGER( I4B ), INTENT( IN ) :: i1
  INTEGER( I4B ), INTENT( IN ) :: i2
  INTEGER( I4B ), INTENT( IN ) :: j1
  INTEGER( I4B ), INTENT( IN ) :: j2
  INTEGER( I4B ), INTENT( OUT ) :: nr
  INTEGER( I4B ), INTENT( OUT ) :: nc
  INTEGER( I4B ), INTENT( IN ) :: ia( : )
  INTEGER( I4B ), INTENT( IN ) :: ja( : )
  INTEGER( I4B ), INTENT( OUT ) :: jao( : )
  INTEGER( I4B ), INTENT( OUT ) :: iao( : )
  REAL( DFP ), INTENT( IN ) :: a( : )
  REAL( DFP ), INTENT( INOUT) :: ao( : )

  nr = i2-i1+1
  nc = j2-j1+1

  IF ( nr .LE. 0 .or. nc .LE. 0) RETURN
  klen = 0
  DO i = 1,nr
    ii = i1+i-1
    k1 = ia(ii)
    k2 = ia(ii+1)-1
    iao(i) = klen+1
    DO k=k1,k2
      j = ja(k)
      IF (j .GE. j1 .and. j .LE. j2) then
        klen = klen+1
        IF (job .EQ. 1) ao(klen) = a(k)
        jao(klen) = j - j1+1
      ENDIF
    END DO
  END DO
  iao(nr+1) = klen+1
END SUBROUTINE submat_saad