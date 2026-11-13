! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

N = right - left + 1
IF (N < minimumLengthForInsertion) THEN
  CALL argInsertionSort(this, idx, left, right)
  RETURN
END IF
IF (maxDepth == 0) THEN
  ! ArgHeapSort resets its arg to 1..N and compares array(arg(:)),
  ! so sort the values of this slice and map back to idx
  tmp = idx(left:right)
  CALL argHeapsort(this(tmp), perm)
  idx(left:right) = tmp(perm)
  RETURN
END IF
imid = left + N / 2
CALL argMedian(this, idx, left, imid, right)
! use the median as pivot, as in IntroSort
CALL Swap(idx(left), idx(imid))
CALL argPartition(this, idx, left, right, iPivot)
CALL _Recursive_ArgIntroSort_(this, idx, left, iPivot - 1, maxDepth - 1)
CALL _Recursive_ArgIntroSort_(this, idx, iPivot + 1, right, maxDepth - 1)
