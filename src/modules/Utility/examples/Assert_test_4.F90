PROGRAM main
USE GlobalData, ONLY: DFP
USE AssertUtility, ONLY: Assert
IMPLICIT NONE
CALL Assert(n1=1, n2=2, msg="n1 and n2 are not same", &
            line=__LINE__, file=__FILE__, routine="main()")

CALL Assert(n1=1, n2=1, n3=1, &
            msg="n1, n2, n3 are not same", &
            line=__LINE__, file=__FILE__, routine="main()")

CALL Assert(n1=1, n2=1, n3=1, n4=1, &
            msg="n1, n2, n3, n4 are not same", &
            line=__LINE__, file=__FILE__, routine="main()")
END PROGRAM main
