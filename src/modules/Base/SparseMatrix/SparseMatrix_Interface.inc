!----------------------------------------------------------------------------
!                                                                 CsrToDense
!----------------------------------------------------------------------------
INTERFACE CsrToDense

  PURE SUBROUTINE CSRDNS(nrow,ncol,a,ja,ia,dns,ndns,ierr)
    !<--- This is from SparseKit >> format.f
    USE GlobalData, ONLY : Int32, Real64
    INTEGER( Int32 ), INTENT( IN ) :: nrow, ncol
    REAL( Real64 ), INTENT( IN ) :: A( : )
    INTEGER( Int32 ), INTENT( IN ) :: JA( : ), IA( : )
    REAL( Real64 ), INTENT( INOUT) :: DNS( :, : )
    INTEGER( Int32 ), INTENT( IN ) :: ndns
    INTEGER( Int32 ), INTENT( INOUT) :: ierr
  END SUBROUTINE CSRDNS
END INTERFACE CsrToDense

PUBLIC :: CsrToDense

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------