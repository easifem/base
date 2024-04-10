!
!(c) Matthew Kennel, Institute for Nonlinear Science (2004)
!
! Licensed under the Academic Free License version 1.1 found in file LICENSE
! with additional provisions found in that same file.
!

! There are two modules in this file
!
! kdtree2_priority_queue_module
! kdtree2_module

MODULE Kd2PQueue_Module
USE GlobalData, ONLY: kdkind => DFP, I4B, LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: kdtree2_result
PUBLIC :: pq
PUBLIC :: pq_create
PUBLIC :: pq_delete, pq_insert
PUBLIC :: pq_extract_max, pq_max, pq_replace_max, pq_maxpri

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! maintain a priority queue (PQ) of data, pairs of 'priority/payload',
! implemented with a binary heap.  This is the type, and the 'dis' field
! is the priority.

TYPE kdtree2_result
  ! a pair of distances, indexes
  REAL(kdkind) :: dis !=0.0
  INTEGER :: idx !=-1   Initializers cause some bugs in compilers.
END TYPE kdtree2_result

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! A heap-based priority queue lets one efficiently implement the following
! operations, each in log(N) time, as opposed to linear time.
!
! 1)  add a datum (push a datum onto the queue, increasing its length)
! 2)  return the priority value of the maximum priority element
! 3)  pop-off (and delete) the element with the maximum priority, decreasing
!     the size of the queue.
! 4)  replace the datum with the maximum priority with a supplied datum
!     (of either higher or lower priority), maintaining the size of the
!     queue.
!
!
! In the k-d tree case, the 'priority' is the square distance of a point in
! the data set to a reference point.   The goal is to keep the smallest M
! distances to a reference point.  The tree algorithm searches terminal
! nodes to decide whether to add points under consideration.
!
! A priority queue is useful here because it lets one quickly return the
! largest distance currently existing in the list.  If a new candidate
! distance is smaller than this, then the new candidate ought to replace
! the old candidate.  In priority queue terms, this means removing the
! highest priority element, and inserting the new one.
!
! Algorithms based on Cormen, Leiserson, Rivest, _Introduction
! to Algorithms_, 1990, with further optimization by the author.
!
! Originally informed by a C implementation by Sriranga Veeraraghavan.
!
! This module is not written in the most clear way, but is implemented such
! for speed, as it its operations will be called many times during searches
! of large numbers of neighbors.
!
TYPE pq
  !
  ! The priority queue consists of elements
  ! priority(1:heap_size), with associated payload(:).
  !
  ! There are heap_size active elements.
  ! Assumes the allocation is always sufficient.  Will NOT increase it
  ! to match.
  INTEGER :: heap_size = 0
  TYPE(kdtree2_result), POINTER :: elems(:)
END TYPE pq

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

FUNCTION pq_create(results_in) RESULT(res)
  !
  ! Create a priority queue from ALREADY allocated
  ! array pointers for storage.  NOTE! It will NOT
  ! add any alements to the heap, i.e. any existing
  ! data in the input arrays will NOT be used and may
  ! be overwritten.
  !
  ! usage:
  !    real(kdkind), pointer :: x(:)
  !    integer, pointer :: k(:)
  !    allocate(x(1000),k(1000))
  !    pq => pq_create(x,k)
  !
  TYPE(kdtree2_result), TARGET :: results_in(:)
  TYPE(pq) :: res
  !
  !
  INTEGER :: nalloc

  nalloc = SIZE(results_in, 1)
  IF (nalloc .LT. 1) THEN
    WRITE (*, *) 'PQ_CREATE: error, input arrays must be allocated.'
  END IF
  res%elems => results_in
  res%heap_size = 0
  RETURN
END FUNCTION pq_create

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!
! operations for getting parents and left + right children
! of elements in a binary heap.
!

!
! These are written inline for speed.
!
!  integer function parent(i)
!    integer, intent(in) :: i
!    parent = (i/2)
!    return
!  end function parent

!  integer function left(i)
!    integer, intent(in) ::i
!    left = (2*i)
!    return
!  end function left

!  integer function right(i)
!    integer, intent(in) :: i
!    right = (2*i)+1
!    return
!  end function right

!  logical function compare_priority(p1,p2)
!    real(kdkind), intent(in) :: p1, p2
!
!    compare_priority = (p1 .gt. p2)
!    return
!  end function compare_priority

SUBROUTINE heapify(a, i_in)
  !
  ! take a heap rooted at 'i' and force it to be in the
  ! heap canonical form.   This is performance critical
  ! and has been tweaked a little to reflect this.
  !
  TYPE(pq), POINTER :: a
  INTEGER, INTENT(in) :: i_in
  !
  INTEGER :: i, l, r, largest

  REAL(kdkind) :: pri_i, pri_l, pri_r, pri_largest

  TYPE(kdtree2_result) :: temp

  i = i_in

  bigloop: DO
    l = 2 * i ! left(i)
    r = l + 1 ! right(i)
    !
    ! set 'largest' to the index of either i, l, r
    ! depending on whose priority is largest.
    !
    ! note that l or r can be larger than the heap size
    ! in which case they do not count.

    ! does left child have higher priority?
    IF (l .GT. a%heap_size) THEN
      ! we know that i is the largest as both l and r are invalid.
      EXIT
    ELSE
      pri_i = a%elems(i)%dis
      pri_l = a%elems(l)%dis
      IF (pri_l .GT. pri_i) THEN
        largest = l
        pri_largest = pri_l
      ELSE
        largest = i
        pri_largest = pri_i
      END IF

      !
      ! between i and l we have a winner
      ! now choose between that and r.
      !
      IF (r .LE. a%heap_size) THEN
        pri_r = a%elems(r)%dis
        IF (pri_r .GT. pri_largest) THEN
          largest = r
        END IF
      END IF
    END IF

    IF (largest .NE. i) THEN
      ! swap data in nodes largest and i, then heapify

      temp = a%elems(i)
      a%elems(i) = a%elems(largest)
      a%elems(largest) = temp
      !
      ! Canonical heapify() algorithm has tail-ecursive call:
      !
      !        call heapify(a,largest)
      ! we will simulate with cycle
      !
      i = largest
      CYCLE bigloop ! continue the loop
    ELSE
      RETURN ! break from the loop
    END IF
  END DO bigloop
  RETURN
END SUBROUTINE heapify

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE pq_max(a, e)
  !
  ! return the priority and its payload of the maximum priority element
  ! on the queue, which should be the first one, if it is
  ! in heapified form.
  !
  TYPE(pq), POINTER :: a
  TYPE(kdtree2_result), INTENT(out) :: e

  IF (a%heap_size .GT. 0) THEN
    e = a%elems(1)
  ELSE
    WRITE (*, *) 'PQ_MAX: ERROR, heap_size < 1'
    STOP
  END IF
  RETURN
END SUBROUTINE pq_max

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

REAL(kdkind) FUNCTION pq_maxpri(a)
  TYPE(pq), POINTER :: a

  IF (a%heap_size .GT. 0) THEN
    pq_maxpri = a%elems(1)%dis
  ELSE
    WRITE (*, *) 'PQ_MAX_PRI: ERROR, heapsize < 1'
    STOP
  END IF
  RETURN
END FUNCTION pq_maxpri

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE pq_extract_max(a, e)
  !
  ! return the priority and payload of maximum priority
  ! element, and remove it from the queue.
  ! (equivalent to 'pop()' on a stack)
  !
  TYPE(pq), POINTER :: a
  TYPE(kdtree2_result), INTENT(out) :: e

  IF (a%heap_size .GE. 1) THEN
    !
    ! return max as first element
    !
    e = a%elems(1)

    !
    ! move last element to first
    !
    a%elems(1) = a%elems(a%heap_size)
    a%heap_size = a%heap_size - 1
    CALL heapify(a, 1)
    RETURN
  ELSE
    WRITE (*, *) 'PQ_EXTRACT_MAX: error, attempted to pop non-positive PQ'
    STOP
  END IF

END SUBROUTINE pq_extract_max

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

REAL(kdkind) FUNCTION pq_insert(a, dis, idx)
  !
  ! Insert a new element and return the new maximum priority,
  ! which may or may not be the same as the old maximum priority.
  !
  TYPE(pq), POINTER :: a
  REAL(kdkind), INTENT(in) :: dis
  INTEGER, INTENT(in) :: idx
  !    type(kdtree2_result), intent(in) :: e
  !
  INTEGER :: i, isparent
  REAL(kdkind) :: parentdis

  a%heap_size = a%heap_size + 1
  i = a%heap_size

  DO WHILE (i .GT. 1)
    isparent = INT(i / 2)
    parentdis = a%elems(isparent)%dis
    IF (dis .GT. parentdis) THEN
      ! move what was in i's parent into i.
      a%elems(i)%dis = parentdis
      a%elems(i)%idx = a%elems(isparent)%idx
      i = isparent
    ELSE
      EXIT
    END IF
  END DO

  ! insert the element at the determined position
  a%elems(i)%dis = dis
  a%elems(i)%idx = idx

  pq_insert = a%elems(1)%dis
  RETURN
  !    end if

END FUNCTION pq_insert

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

REAL(kdkind) FUNCTION pq_replace_max(a, dis, idx)
  !
  ! Replace the extant maximum priority element
  ! in the PQ with (dis,idx).  Return
  ! the new maximum priority, which may be larger
  ! or smaller than the old one.
  !
  TYPE(pq), POINTER :: a
  REAL(kdkind), INTENT(in) :: dis
  INTEGER, INTENT(in) :: idx
!    type(kdtree2_result), intent(in) :: e
  ! not tested as well!

  INTEGER :: parent, child, N
  REAL(kdkind) :: prichild, prichildp1

  TYPE(kdtree2_result) :: etmp

  IF (.TRUE.) THEN
    N = a%heap_size
    IF (N .GE. 1) THEN
      parent = 1
      child = 2

      loop: DO WHILE (child .LE. N)
        prichild = a%elems(child)%dis

        !
        ! posibly child+1 has higher priority, and if
        ! so, get it, and increment child.
        !

        IF (child .LT. N) THEN
          prichildp1 = a%elems(child + 1)%dis
          IF (prichild .LT. prichildp1) THEN
            child = child + 1
            prichild = prichildp1
          END IF
        END IF

        IF (dis .GE. prichild) THEN
          EXIT loop
          ! we have a proper place for our new element,
          ! bigger than either children's priority.
        ELSE
          ! move child into parent.
          a%elems(parent) = a%elems(child)
          parent = child
          child = 2 * parent
        END IF
      END DO loop
      a%elems(parent)%dis = dis
      a%elems(parent)%idx = idx
      pq_replace_max = a%elems(1)%dis
    ELSE
      a%elems(1)%dis = dis
      a%elems(1)%idx = idx
      pq_replace_max = dis
    END IF
  ELSE
    !
    ! slower version using elementary pop and push operations.
    !
    CALL pq_extract_max(a, etmp)
    etmp%dis = dis
    etmp%idx = idx
    pq_replace_max = pq_insert(a, dis, idx)
  END IF
  RETURN
END FUNCTION pq_replace_max

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE pq_delete(a, i)
  !
  ! delete item with index 'i'
  !
  TYPE(pq), POINTER :: a
  INTEGER :: i

  IF ((i .LT. 1) .OR. (i .GT. a%heap_size)) THEN
    WRITE (*, *) 'PQ_DELETE: error, attempt to remove out of bounds element.'
    STOP
  END IF

  ! swap the item to be deleted with the last element
  ! and shorten heap by one.
  a%elems(i) = a%elems(a%heap_size)
  a%heap_size = a%heap_size - 1

  CALL heapify(a, i)

END SUBROUTINE pq_delete

END MODULE Kd2PQueue_Module
