
! c     This routine performs two tasks to clean up a CSR matrix
! c     -- remove duplicate/zero entries,
! c     -- perform a partial ordering, new order lower triangular part,
! c        main diagonal, upper triangular part.
! c
! c     On entry:
! c
! c     job   = options
! c         0 -- nothing is DOne
! c         1 -- eliminate duplicate entries, zero entries.
! c         2 -- eliminate duplicate entries and perform partial ordering.
! c         3 -- eliminate duplicate entries, sort the entries in the
! c              increasing order of clumn indices.
! c
! c     value2  -- 0 the matrix is pattern only (a is not touched)
! c                1 matrix has values too.
! c     nrow    -- row dimension of the matrix
! c     a,ja,ia -- input matrix in CSR format
! c
! c     On RETURN:
! c     a,ja,ia -- cleaned matrix
! c     indu    -- pointers to the beginning of the upper triangular
! c                portion IF job > 1
! c
! c     Work space:
! c     iwk     -- integer work space of size nrow+1
! c

! array INDU is used as marker for existing indices, it is also the
! location of the entry.
! IWK is used to stored the old IA array.
! matrix is copied to squeeze out the space taken by the duplicated
! entries.

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CLNCSR_SAAD( a, ja, ia, job, isValues, IndexUT )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: a( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: ja( : )
  INTEGER( I4B ), INTENT( INOUT) :: ia( : )
  INTEGER( I4B ), INTENT( IN ) :: job
  LOGICAL( LGT ), INTENT( IN ) :: isValues
  INTEGER( I4B ), OPTIONAL, INTENT( INOUT ) :: IndexUT( : )

  ! Internal variables
  INTEGER( I4B ) :: i, j, k, ko, ipos, kfirst, klast, nrow, value2
  REAL( DFP ) :: tmp
  INTEGER( I4B ):: indu(( size( ia ) - 1 )), iwk (( size( ia ) )), temp_ja( size( ja ) )
  REAL( DFP ) :: temp_a( size( a ) )

  IF( isValues ) THEN
    value2 = 1
  ELSE
    value2 = 0
  END IF

  nrow = size( ia ) - 1

  SELECT CASE( job )
  CASE( 1 )
    CALL JOB1
    CALL FINISH_JOB
  CASE( 2 )
    CALL JOB1
    CALL JOB2
    CALL FINISH_JOB
  CASE( 3 )
    CALL JOB1
    CALL JOB2
    CALL JOB3
    CALL FINISH_JOB
  END SELECT

  CONTAINS

  SUBROUTINE JOB1
    DO i = 1, nrow
      indu(i) = 0
      iwk(i) = ia(i)
    END DO
    iwk(nrow+1) = ia(nrow+1)
    k = 1

    DO i = 1, nrow
      ia(i) = k
      ipos = iwk(i)
      klast = iwk(i+1)

      DO WHILE (ipos.lt.klast)
        j = ja(ipos)
        IF (indu(j).eq.0) THEN
          IF (value2.ne.0) THEN
            IF (a(ipos) .ne. 0.0D0) THEN
              indu(j) = k
              ja(k) = ja(ipos)
              a(k) = a(ipos)
              k = k + 1
            ENDIF
          ELSE
            indu(j) = k
            ja(k) = ja(ipos)
            k = k + 1
          ENDIF
        ELSE IF (value2.ne.0) THEN
          a(indu(j)) = a(indu(j)) + a(ipos)
        ENDIF
        ipos = ipos + 1
      END DO

      DO ipos = ia(i), k - 1
        indu(ja(ipos)) = 0
      END DO

    END DO

    ia(nrow+1) = k
  END SUBROUTINE JOB1

  ! partial ordering ..
  ! split the matrix into strict upper/lower triangular
  ! parts, INDU points to the the beginning of the upper part.

  SUBROUTINE JOB2
    DO i = 1, nrow
      klast = ia(i+1) - 1
      kfirst = ia(i)
      DO WHILE( klast.gt.kfirst )
        IF (ja(klast).lt.i .and. ja(kfirst).ge.i) THEN
          ! swap klast with kfirst ..
          j = ja(klast)
          ja(klast) = ja(kfirst)
          ja(kfirst) = j
          IF (value2.ne.0) THEN
            tmp = a(klast)
            a(klast) = a(kfirst)
            a(kfirst) = tmp
          ENDIF
        ENDIF
        IF (ja(klast).ge.i) klast = klast - 1
        IF (ja(kfirst).lt.i) kfirst = kfirst + 1
      END DO
      IF (ja(klast).lt.i) THEN
        indu(i) = klast + 1
      ELSE
        indu(i) = klast
      ENDIF
    END DO
  END SUBROUTINE JOB2

  ! IF (job.le.2) RETURN

  SUBROUTINE JOB3
    ! order the entries according to column indices
    ! bubble-sort is used
    DO i = 1, nrow
      DO ipos = ia(i), indu(i)-1
        DO j = indu(i)-1, ipos+1, -1
          k = j - 1
          IF (ja(k).gt.ja(j)) THEN
            ko = ja(k)
            ja(k) = ja(j)
            ja(j) = ko
            IF (value2.ne.0) THEN
              tmp = a(k)
              a(k) = a(j)
              a(j) = tmp
            ENDIF
          ENDIF
        END DO
      END DO

      DO ipos = indu(i), ia(i+1)-1
        DO j = ia(i+1)-1, ipos+1, -1
          k = j - 1
          IF (ja(k).gt.ja(j)) THEN
            ko = ja(k)
            ja(k) = ja(j)
            ja(j) = ko
            IF (value2.ne.0) THEN
              tmp = a(k)
              a(k) = a(j)
              a(j) = tmp
            ENDIF
          ENDIF
        END DO
      END DO
    END DO
  END SUBROUTINE JOB3

  SUBROUTINE FINISH_JOB
    IF( PRESENT( IndexUT ) ) IndexUT = INDU
    k = SIZE( A )
    j = IA( k ) - 1
    IF( k .NE. j ) THEN
      temp_ja = ja
      temp_a = a
      CALL Reallocate( A, j, JA, j )
      DO i = 1, j
        A( i ) = temp_a( i )
        JA( i ) = temp_ja( i )
      END DO
    END IF
  END SUBROUTINE FINISH_JOB

END SUBROUTINE