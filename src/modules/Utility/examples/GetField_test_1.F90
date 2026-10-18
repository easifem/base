!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetField returns the i-th blank or comma separated field

PROGRAM main
USE ISO_FORTRAN_ENV, ONLY: IOSTAT_END
USE GlobalData, ONLY: I4B
USE StringUtility, ONLY: GetField
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(LEN=100) :: astr
CHARACTER(:), ALLOCATABLE :: field
CHARACTER(LEN=5) :: want(5)
INTEGER(I4B) :: ierr, ii

astr = "Hello world, how are you"
want = [CHARACTER(LEN=5) :: "Hello", "world", "how", "are", "you"]

DO ii = 1, 5
  CALL GetField(i=ii, chars=astr, field=field, ierr=ierr)
  CALL OK(field .EQ. TRIM(want(ii)), "field "//want(ii))
  CALL OK(ierr .EQ. 0_I4B, "ierr is 0 for field "//want(ii))
END DO

! i out of range: ierr is IOSTAT_END and field is not allocated
CALL GetField(i=6, chars=astr, field=field, ierr=ierr)
CALL OK(ierr .EQ. IOSTAT_END, "i > number of fields")
CALL OK(.NOT. ALLOCATED(field), "field is not allocated")

CALL GetField(i=0, chars=astr, field=field, ierr=ierr)
CALL OK(ierr .EQ. IOSTAT_END, "i = 0")

END PROGRAM main
