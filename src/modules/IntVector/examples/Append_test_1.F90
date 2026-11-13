!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: Testing Append for a scalar, an array and an IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Append
USE IntVector_Method, ONLY: Size
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: append a scalar to a new object
BLOCK
  TYPE(IntVector_) :: obj
  CALL Append(obj, 5_I4B)
  CALL OK(Size(obj) .EQ. 1, "Append: scalar, tsize")
  CALL OK(obj%val(1) .EQ. 5, "Append: scalar, value")
  CALL OK(obj%capacity .EQ. SIZE(obj%val), "Append: capacity")
END BLOCK

! Test 2: repeated scalar append grows past the chunk size
BLOCK
  TYPE(IntVector_) :: obj
  INTEGER(I4B) :: ii
  LOGICAL :: isok
  DO ii = 1, 25
    CALL Append(obj, ii)
  END DO
  isok = ALL(obj%val(1:25) .EQ. [(ii, ii=1, 25)])
  CALL OK(Size(obj) .EQ. 25, "Append: repeated scalar, tsize")
  CALL OK(isok, "Append: repeated scalar, values")
  CALL OK(obj%capacity .GE. 25, "Append: repeated scalar, capacity")
END BLOCK

! Test 3: append an array to an object with values
BLOCK
  TYPE(IntVector_) :: obj
  obj = [1_I4B, 2_I4B]
  CALL Append(obj, [3_I4B, 4_I4B, 5_I4B])
  CALL OK(Size(obj) .EQ. 5, "Append: array, tsize")
  CALL OK(ALL(obj%val(1:5) .EQ. [1, 2, 3, 4, 5]), "Append: array, val")
  CALL OK(obj%capacity .EQ. SIZE(obj%val), "Append: array capacity")
END BLOCK

! Test 4: array longer than the chunk size, on a new object
BLOCK
  TYPE(IntVector_) :: obj
  INTEGER(I4B) :: ii
  CALL Append(obj, [(ii, ii=1, 37)])
  CALL OK(Size(obj) .EQ. 37, "Append: long array, tsize")
  CALL OK(ALL(obj%val(1:37) .EQ. [(ii, ii=1, 37)]), &
          "Append: long array, values")
END BLOCK

! Test 5: append an IntVector_, only its active entries are copied
BLOCK
  TYPE(IntVector_) :: obj, other
  obj = [1_I4B]
  CALL Append(other, 7_I4B)
  CALL Append(other, 8_I4B)
  CALL OK(other%capacity .GT. Size(other), "Append: other has spare room")
  CALL Append(obj, other)
  CALL OK(Size(obj) .EQ. 3, "Append: IntVector_, tsize")
  CALL OK(ALL(obj%val(1:3) .EQ. [1, 7, 8]), "Append: IntVector_, values")
  CALL OK(Size(other) .EQ. 2, "Append: IntVector_, other unchanged")
END BLOCK

END PROGRAM main
