PROGRAM main
USE GlobalData, ONLY: DFP
USE AssertUtility, ONLY: Assert
IMPLICIT NONE
REAL(DFP) :: mat(4, 4, 2)
CALL Assert(mat=mat, s=[4, 4, 2], msg="shape of mat is not same as [4,4,2]", &
            line=__LINE__, file=__FILE__, routine="main()")

CALL Assert(mat=mat, s=[5, 5, 2], msg="shape of mat is not same as [5,5,2]", &
            line=__LINE__, file=__FILE__, routine="main()")
END PROGRAM main
