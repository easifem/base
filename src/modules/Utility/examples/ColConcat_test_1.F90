PROGRAM main
USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
USE AppendUtility, ONLY: ColConcat
USE MdEncode_Method, ONLY: MdEnCode
! USE Display_Method, ONLY: Display
USE String_Class, ONLY: Display
USE Test_Method, ONLY: OK
IMPLICIT NONE
! Concat two integer vectors
CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4])), &
             "colconcat="//CHAR_LF//CHAR_LF)
CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4]) &
            .EQ. RESHAPE([1, 2, 3, 4], [2, 2])), "colconcat")
! Concat two integer vectors of different length
CALL Display(MdEnCode(ColConcat(a=[1, 2], b=[3, 4, 5])), &
             "colconcat="//CHAR_LF//CHAR_LF)
CALL OK(ALL(ColConcat(a=[1, 2], b=[3, 4, 5]) &
            .EQ. RESHAPE([1, 2, 0, 3, 4, 5], [3, 2])), "colconcat")
! Concat two integer vectors of different length.
CALL Display(MdEnCode(ColConcat(a=[3, 4, 5], b=[1, 2])), &
             "colconcat="//CHAR_LF//CHAR_LF)
END PROGRAM main
