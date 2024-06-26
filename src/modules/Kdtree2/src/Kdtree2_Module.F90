!(c) Matthew Kennel, INstitute for Nonlinear Science (2004)
!
! Licensed under the Academic Free License version 1.1 found in file LICENSE
! with additional provisions found in that same file.
!

! There are two modules in this file
!
! Kdtree2_priority_queue_module
! Kdtree2_module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! K-D tree routines in Fortran 90 by Matt Kennel.
! Original program was written in Sather by Steve Omohundro and
! Matt Kennel.  Only the Euclidean metric is supported.
!
!
! This module is identical to 'kd_tree', except that the order
! of subscripts is reversed in the data file.
! IN otherwords for an embedding of N D-dimensional vectors, the
! data file is here, in natural Fortran order  data(1:D, 1:N)
! because Fortran lays out columns first,
!
! whereas conventionally (C-style) it is data(1:N,1:D)
! as in the original kd_tree module.

MODULE Kdtree2_Module
USE GlobalData, ONLY: kdkind => DFP, I4B, LGT, stdout, stderr, CHAR_LF
USE ErrorHandling, ONLY: Errormsg
USE Display_Method, ONLY: Display
USE Kd2PQueue_Module
USE INputUtility
IMPLICIT NONE
PRIVATE

PUBLIC :: Kdtree2_, Kdtree2Result_, Kdtree2Node_
PUBLIC :: Kdtree2_create, Kdtree2_Destroy
PUBLIC :: Kdtree2_n_nearest, Kdtree2_n_nearest_around_point
PUBLIC :: Kdtree2_r_nearest, Kdtree2_r_nearest_around_point
PUBLIC :: Kdtree2_r_count, Kdtree2_r_count_around_point
PUBLIC :: Kdtree2_sort_results
PUBLIC :: Kdtree2_n_nearest_brute_force, Kdtree2_r_nearest_brute_force

INTEGER, PARAMETER :: bucket_size = 12
! The maximum number of points to keep in a terminal node.

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE interval
  REAL(kdkind) :: lower, upper
END TYPE interval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! an internal tree node
TYPE :: Kdtree2Node_
  PRIVATE
  INTEGER :: cut_dim
  ! the dimension to cut
  REAL(kdkind) :: cut_val
  ! where to cut the dimension
  REAL(kdkind) :: cut_val_left, cut_val_right
  ! improved cutoffs knowing the spread in child boxes.
  INTEGER :: l, u
  TYPE(Kdtree2Node_), POINTER :: left, right
  TYPE(interval), ALLOCATABLE :: box(:)
  ! child pointers
  ! Points included in this node are indexes[k] with k \in [l,u]
END TYPE Kdtree2Node_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: Kdtree2_
  ! Global information about the tree, one per tree
  INTEGER :: dimen = 0, n = 0
  ! dimensionality and total # of points
  LOGICAL :: sort = .FALSE.
  ! do we always sort output results?
  LOGICAL :: rearrange = .FALSE.
  REAL(kdkind), POINTER :: the_data(:, :) => NULL()
  ! pointer to the actual data array
  !
  !  IMPORTANT NOTE:  IT IS DIMENSIONED   the_data(1:d,1:N)
  !  which may be opposite of what may be conventional.
  !  This is, because in Fortran, the memory layout is such that
  !  the first dimension is in sequential order.  Hence, with
  !  (1:d,1:N), all components of the vector will be in consecutive
  !  memory locations.  The search time is dominated by the
  !  evaluation of distances in the terminal nodes.  Putting all
  !  vector components in consecutive memory location improves
  !  memory cache locality, and hence search speed, and may enable
  !  vectorization on some processors and compilers.

  INTEGER, ALLOCATABLE :: ind(:)
  ! permuted index into the data, so that indexes[l..u] of some
  ! bucket represent the indexes of the actual points in that
  ! bucket.
  REAL(kdkind), ALLOCATABLE :: rearranged_data(:, :)
  ! if (rearrange .eqv. .true.) then rearranged_data has been
  ! created so that rearranged_data(:,i) = the_data(:,ind(i)),
  ! permitting search to use more cache-friendly rearranged_data, at
  ! some initial computation and storage cost.
  TYPE(Kdtree2Node_), POINTER :: root => NULL()
  ! root pointer of the tree
END TYPE Kdtree2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! One of these is created for each search.
!
! Many fields are copied from the tree structure, in order to
! speed up the search.
!
TYPE :: tree_search_record
  PRIVATE
  INTEGER :: dimen
  INTEGER :: nn, nfound
  REAL(kdkind) :: ballsize
  INTEGER :: centeridx = 999, correltime = 9999
  ! exclude points within 'correltime' of 'centeridx', iff centeridx >= 0
  INTEGER :: nalloc ! how much allocated for results(:)?
  LOGICAL :: rearrange ! are the data rearranged or original?
  ! did the # of points found overflow the storage provided?
  LOGICAL :: overflow
  REAL(kdkind), POINTER :: qv(:) ! query vector
  TYPE(Kdtree2Result_), POINTER :: results(:) ! results
  TYPE(pq) :: pq
  REAL(kdkind), POINTER :: DATA(:, :) ! temp pointer to data
  INTEGER, POINTER :: ind(:) ! temp pointer to indexes
END TYPE tree_search_record

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE(tree_search_record), SAVE, TARGET :: sr ! A GLOBAL VARIABLE for search

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

! create the actual tree structure, given an input array of data.
!
! Note, input data is input_data(1:d,1:N), NOT the other way around.
! THIS IS THE REVERSE OF THE PREVIOUS VERSION OF THIS MODULE.
! The reason for it is cache friendliness, improving performance.
!
! Optional arguments:  If 'dim' is specified, then the tree
!                      will only search the first 'dim' components
!                      of input_data, otherwise, dim is inferred
!                      from SIZE(input_data,1).
!
!                      if sort .eqv. .true. then output results
!                      will be sorted by increasing distance.
!                      default=.false., as it is faster to not sort.
!
!                      if rearrange .eqv. .true. then an internal
!                      copy of the data, rearranged by terminal node,
!                      will be made for cache friendliness.
!                      default=.true., as it speeds searches, but
!                      building takes longer, and extra memory is used.

FUNCTION Kdtree2_create(input_data, dim, sort, rearrange) RESULT(mr)
  TYPE(Kdtree2_), POINTER :: mr
  INTEGER, INTENT(IN), OPTIONAL :: dim
  LOGICAL, INTENT(IN), OPTIONAL :: sort
  LOGICAL, INTENT(IN), OPTIONAL :: rearrange
  REAL(kdkind), TARGET :: input_data(:, :)

  ! internal variables
  INTEGER :: i

  ALLOCATE (mr)
  mr%the_data => input_data
  ! pointer assignment

  mr%dimen = INput(default=SIZE(input_data, 1), option=dim)
  mr%n = SIZE(input_data, 2)

#ifdef DEBUG_VER

  IF (mr%dimen > mr%n) THEN
    !  unlikely to be correct
    WRITE (*, *) 'KD_TREE_TRANS: likely user error.'
    WRITE (*, *) 'KD_TREE_TRANS: You passed in matrix with D=', mr%dimen
    WRITE (*, *) 'KD_TREE_TRANS: and N=', mr%n
    WRITE (*, *) 'KD_TREE_TRANS: note, that new format is data(1:D,1:N)'
    write (*,*) 'KD_TREE_TRANS: with usually N >> D.   If N =approx= D, then a k-d tree'
    WRITE (*, *) 'KD_TREE_TRANS: is not an appropriate data structure.'
    STOP
  END IF

#endif

  CALL build_tree(mr)

  mr%sort = INput(default=.FALSE., option=sort)
  mr%rearrange = INput(default=.TRUE., option=rearrange)

  IF (.NOT. mr%rearrange) THEN
    IF (ALLOCATED(mr%rearranged_data)) DEALLOCATE (mr%rearranged_data)
    RETURN
  END IF

  ALLOCATE (mr%rearranged_data(mr%dimen, mr%n))
  DO i = 1, mr%n
    mr%rearranged_data(:, i) = mr%the_data(:, mr%ind(i))
  END DO

END FUNCTION Kdtree2_create

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE build_tree(tp)
  TYPE(Kdtree2_), INTENT(INOUT) :: tp
  INTEGER :: j
  TYPE(Kdtree2Node_), POINTER :: dummy => NULL()
  ALLOCATE (tp%ind(tp%n))
  DO CONCURRENT(j=1:tp%n)
    tp%ind(j) = j
  END DO
  tp%root => build_tree_for_range(tp, 1, tp%n, dummy)
END SUBROUTINE build_tree

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

RECURSIVE FUNCTION build_tree_for_range(tp, l, u, parent) RESULT(res)
  TYPE(Kdtree2Node_), POINTER :: res
  TYPE(Kdtree2_), INTENT(INOUT) :: tp
  TYPE(Kdtree2Node_), POINTER :: parent
  INTEGER, INTENT(IN) :: l, u

  ! internal variables
  INTEGER :: i, c, m, dimen
  LOGICAL :: recompute, isok
  REAL(kdkind) :: average

  ! first compute min and max
  dimen = tp%dimen
  ALLOCATE (res)
  ALLOCATE (res%box(dimen))

  ! First, compute an APPROXIMATE bounding box of all points
  ! associated with this node.
  IF (u < l) THEN
    ! no points in this box
    NULLIFY (res)
    RETURN
  END IF

  isok = (u - l) <= bucket_size
  IF (isok) THEN
    ! always compute true bounding box for terminal nodes.
    DO i = 1, dimen
      CALL spread_in_coordinate(tp, i, l, u, res%box(i))
    END DO
    res%cut_dim = 0
    res%cut_val = 0.0
    res%l = l
    res%u = u
    res%left => NULL()
    res%right => NULL()
    RETURN
  END IF

  ! modify approximate bounding box.  This will be an
  ! overestimate of the true bounding box, as we are only recomputing
  ! the bounding box for the dimension that the parent split on.
  !
  ! Going to a true bounding box computation would significantly
  ! increase the time necessary to build the tree, and usually
  ! has only a very small difference.  This box is not used
  ! for searching but only for deciding which coordinate to split on.
  DO i = 1, dimen

    recompute = .TRUE.
    IF (ASSOCIATED(parent)) THEN
      IF (i .NE. parent%cut_dim) THEN
        recompute = .FALSE.
      END IF
    END IF

    IF (recompute) THEN
      CALL spread_in_coordinate(tp, i, l, u, res%box(i))
    ELSE
      res%box(i) = parent%box(i)
    END IF

  END DO

  c = MAXLOC(res%box(1:dimen)%upper - res%box(1:dimen)%lower, 1)
  ! c is the identity of which coordinate has the greatest spread.

  ! select point halfway between min and max, as per A. Moore,
  ! who says this helps in some degenerate cases, or
  ! actual arithmetic average.
  ! actually compute average
  average = SUM(tp%the_data(c, tp%ind(l:u))) / REAL(u - l + 1, kdkind)

  res%cut_val = average
  m = select_on_coordinate_value(tp%the_data, tp%ind, c, average, l, u)

  ! moves indexes around
  res%cut_dim = c
  res%l = l
  res%u = u
  ! res%cut_val = tp%the_data(c,tp%ind(m))

  res%left => build_tree_for_range(tp, l, m, res)
  res%right => build_tree_for_range(tp, m + 1, u, res)

  IF (ASSOCIATED(res%right) .EQV. .FALSE.) THEN
    res%box = res%left%box
    res%cut_val_left = res%left%box(c)%upper
    res%cut_val = res%cut_val_left
  ELSEIF (ASSOCIATED(res%left) .EQV. .FALSE.) THEN
    res%box = res%right%box
    res%cut_val_right = res%right%box(c)%lower
    res%cut_val = res%cut_val_right
  ELSE
    res%cut_val_right = res%right%box(c)%lower
    res%cut_val_left = res%left%box(c)%upper
    res%cut_val = (res%cut_val_left + res%cut_val_right) / 2

    ! now remake the true bounding box for self.
    ! Since we are taking unions (in effect) of a tree structure,
    ! this is much faster than doing an exhaustive
    ! search over all points
    res%box%upper = MAX(res%left%box%upper, res%right%box%upper)
    res%box%lower = MIN(res%left%box%lower, res%right%box%lower)
  END IF
END FUNCTION build_tree_for_range

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Move elts of ind around between l and u, so that all points
! <= than alpha (in c cooordinate) are first, and then
! all points > alpha are second.
!
! Algorithm (matt kennel).
!
! Consider the list as having three parts: on the left,
! the points known to be <= alpha.  On the right, the points
! known to be > alpha, and in the middle, the currently unknown
! points.   The algorithm is to scan the unknown points, starting
! from the left, and swapping them so that they are added to
! the left stack or the right stack, as appropriate.
!
! The algorithm finishes when the unknown stack is empty.
INTEGER FUNCTION select_on_coordinate_value(v, ind, c, alpha, li, ui) &
  RESULT(res)
  INTEGER, INTENT(IN) :: c, li, ui
  REAL(kdkind), INTENT(IN) :: alpha
  REAL(kdkind) :: v(1:, 1:)
  INTEGER :: ind(1:)
  INTEGER :: tmp
  INTEGER :: lb, rb

  ! The points known to be <= alpha are in
  ! [l,lb-1]
  !
  ! The points known to be > alpha are in
  ! [rb+1,u].
  !
  ! Therefore we add new points into lb or
  ! rb as appropriate.  When lb=rb
  ! we are done.  We return the location of the last point <= alpha.
  lb = li; rb = ui

  DO WHILE (lb < rb)
    IF (v(c, ind(lb)) <= alpha) THEN
      ! it is good where it is.
      lb = lb + 1
    ELSE
      ! swap it with rb.
      tmp = ind(lb); ind(lb) = ind(rb); ind(rb) = tmp
      rb = rb - 1
    END IF
  END DO

  ! now lb .eq. ub
  IF (v(c, ind(lb)) <= alpha) THEN
    res = lb
  ELSE
    res = lb - 1
  END IF

END FUNCTION select_on_coordinate_value

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE spread_in_coordinate(tp, c, l, u, interv)
  TYPE(Kdtree2_), INTENT(INOUT) :: tp
  TYPE(interval), INTENT(out) :: interv
  INTEGER, INTENT(IN) :: c, l, u
  ! internal variables
  REAL(kdkind) :: last, lmax, lmin, t, smin, smax
  INTEGER :: i, ulocal
  ! REAL(kdkind), POINTER :: v(:, :)
  ! INTEGER, POINTER :: ind(:)

  ASSOCIATE (v => tp%the_data(1:, 1:), ind => tp%ind(1:))
    smin = v(c, ind(l))
    smax = smin
    ulocal = u

    DO i = l + 2, ulocal, 2
      lmin = v(c, ind(i - 1))
      lmax = v(c, ind(i))
      IF (lmin > lmax) THEN
        t = lmin
        lmin = lmax
        lmax = t
      END IF
      IF (smin > lmin) smin = lmin
      IF (smax < lmax) smax = lmax
    END DO

    IF (i == ulocal + 1) THEN
      last = v(c, ind(ulocal))
      IF (smin > last) smin = last
      IF (smax < last) smax = last
    END IF

    interv%lower = smin
    interv%upper = smax
  END ASSOCIATE

END SUBROUTINE spread_in_coordinate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Deallocates all memory for the tree, except input data matrix
SUBROUTINE Kdtree2_Destroy(tp)
  TYPE(Kdtree2_), INTENT(INOUT) :: tp

  CALL destroy_node(tp%root)

  tp%dimen = 0
  tp%sort = .FALSE.
  tp%rearrange = .FALSE.
  tp%the_data => NULL()
  IF (ALLOCATED(tp%ind)) DEALLOCATE (tp%ind)
  IF (ALLOCATED(tp%rearranged_data)) DEALLOCATE (tp%rearranged_data)

CONTAINS
  RECURSIVE SUBROUTINE destroy_node(np)
    TYPE(Kdtree2Node_), POINTER :: np

    IF (ASSOCIATED(np%left)) THEN
      CALL destroy_node(np%left)
      NULLIFY (np%left)
    END IF

    IF (ASSOCIATED(np%right)) THEN
      CALL destroy_node(np%right)
      NULLIFY (np%right)
    END IF

    IF (ALLOCATED(np%box)) DEALLOCATE (np%box)
    DEALLOCATE (np)

  END SUBROUTINE destroy_node

END SUBROUTINE Kdtree2_Destroy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Find the 'nn' vectors in the tree nearest to 'qv' in euclidean norm
! returning their indexes and distances in 'indexes' and 'distances'
! arrays already allocated passed to this subroutine.
SUBROUTINE Kdtree2_n_nearest(tp, qv, nn, results)
  TYPE(Kdtree2_), TARGET, INTENT(INOUT) :: tp
  REAL(kdkind), TARGET, INTENT(IN) :: qv(:)
  INTEGER, INTENT(IN) :: nn
  TYPE(Kdtree2Result_), TARGET :: results(:)

  sr%ballsize = HUGE(1.0)
  sr%qv => qv
  sr%nn = nn
  sr%nfound = 0
  sr%centeridx = -1
  sr%correltime = 0
  sr%overflow = .FALSE.

  sr%results => results

  sr%nalloc = nn ! will be checked

  sr%ind => tp%ind
  sr%rearrange = tp%rearrange
  IF (tp%rearrange) THEN
    sr%DATA => tp%rearranged_data
  ELSE
    sr%DATA => tp%the_data
  END IF
  sr%dimen = tp%dimen

  CALL validate_query_storage(nn)
  sr%pq = pq_create(results)

  CALL search(tp%root)

  IF (tp%sort) THEN
    CALL Kdtree2_sort_results(nn, results)
  END IF
!    deallocate(sr%pqp)
  RETURN
END SUBROUTINE Kdtree2_n_nearest

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Matthew Kennel
! date:  2024-04-10
! summary:  Find nn vectors in the tree.
!
! Find the 'nn' vectors in the tree nearest to point 'idxin',
! with correlation window 'correltime', returing results in
! results(:), which must be pre-allocated upon entry.

SUBROUTINE Kdtree2_n_nearest_around_point(tp, idxin, correltime, nn, results)
  TYPE(Kdtree2_), TARGET, INTENT(INOUT) :: tp
  INTEGER, INTENT(IN) :: idxin
  INTEGER, INTENT(IN) :: correltime
  !! correlation window
  INTEGER, INTENT(IN) :: nn
  TYPE(Kdtree2Result_), TARGET :: results(:)

  ALLOCATE (sr%qv(tp%dimen))
  sr%qv = tp%the_data(:, idxin)
  ! copy the vector
  sr%ballsize = HUGE(1.0)
  ! the largest real(kdkind) number
  sr%centeridx = idxin
  sr%correltime = correltime

  sr%nn = nn
  sr%nfound = 0

  sr%dimen = tp%dimen
  sr%nalloc = nn

  sr%results => results

  sr%ind => tp%ind
  sr%rearrange = tp%rearrange

  IF (sr%rearrange) THEN
    sr%DATA => tp%rearranged_data
  ELSE
    sr%DATA => tp%the_data
  END IF

  CALL validate_query_storage(nn)
  sr%pq = pq_create(results)

  CALL search(tp%root)

  IF (tp%sort) THEN
    CALL Kdtree2_sort_results(nn, results)
  END IF
  DEALLOCATE (sr%qv)
  RETURN
END SUBROUTINE Kdtree2_n_nearest_around_point

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! find the nearest neighbors to point 'idxin', within SQUARED
! Euclidean distance 'r2'.   Upon ENTRY, nalloc must be the
! size of memory allocated for results(1:nalloc).  Upon
! EXIT, nfound is the number actually found within the ball.
!
!  Note that if nfound .gt. nalloc then more neighbors were found
!  than there were storage to store.  The resulting list is NOT
!  the smallest ball inside norm r^2
!
! Results are NOT sorted unless tree was created with sort option.

SUBROUTINE Kdtree2_r_nearest(tp, qv, r2, nfound, nalloc, results)
  TYPE(Kdtree2_), TARGET, INTENT(INOUT) :: tp
  REAL(kdkind), TARGET, INTENT(IN) :: qv(:)
  REAL(kdkind), INTENT(IN) :: r2
  INTEGER, INTENT(out) :: nfound
  INTEGER, INTENT(IN) :: nalloc
  TYPE(Kdtree2Result_), TARGET :: results(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: msg = &
           '[Warning] :: return from Kdtree2_r_nearest found more neighbors' &
                             //CHAR_LF// &
               'than storage was provided for.  Answer is NOT smallest ball' &
                             //CHAR_LF// &
            'KD_TREE_TRANS: with that number of neighbors!  I.e. it is wrong.'
#endif

  !
  sr%qv => qv
  sr%ballsize = r2
  sr%nn = 0 ! flag for fixed ball search
  sr%nfound = 0
  sr%centeridx = -1
  sr%correltime = 0

  sr%results => results

  CALL validate_query_storage(nalloc)
  sr%nalloc = nalloc
  sr%overflow = .FALSE.
  sr%ind => tp%ind
  sr%rearrange = tp%rearrange

  IF (tp%rearrange) THEN
    sr%DATA => tp%rearranged_data
  ELSE
    sr%DATA => tp%the_data
  END IF
  sr%dimen = tp%dimen

  CALL search(tp%root)
  nfound = sr%nfound
  IF (tp%sort) THEN
    CALL Kdtree2_sort_results(nfound, results)
  END IF

#ifdef DEBUG_VER

  IF (sr%overflow) THEN
    CALL Errormsg( &
      msg=msg, &
      file=__FILE__, &
      line=__LINE__, &
      routine="Kdtree2_n_nearest()")
  END IF

#endif

END SUBROUTINE Kdtree2_r_nearest

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Like Kdtree2_r_nearest, but around a point 'idxin' already existing
! in the data set.
!
! Results are NOT sorted unless tree was created with sort option.
SUBROUTINE Kdtree2_r_nearest_around_point(tp, idxin, correltime, r2, &
                                          nfound, nalloc, results)
  TYPE(Kdtree2_), TARGET, INTENT(INOUT) :: tp
  INTEGER, INTENT(IN) :: idxin, correltime, nalloc
  REAL(kdkind), INTENT(IN) :: r2
  INTEGER, INTENT(out) :: nfound
  TYPE(Kdtree2Result_), TARGET :: results(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: msg = &
               'warning! return from Kdtree2_r_nearest found more neighbors' &
                             //CHAR_LF// &
               'than storage was provided for.  Answer is NOT smallest ball' &
                             //CHAR_LF// &
                           'with that number of neighbors!  I.e. it is wrong.'
#endif

  ALLOCATE (sr%qv(tp%dimen))
  sr%qv = tp%the_data(:, idxin) ! copy the vector
  sr%ballsize = r2
  sr%nn = 0 ! flag for fixed r search
  sr%nfound = 0
  sr%centeridx = idxin
  sr%correltime = correltime

  sr%results => results

  sr%nalloc = nalloc
  sr%overflow = .FALSE.

  CALL validate_query_storage(nalloc)

  !    sr%dsl = HUGE(sr%dsl)    ! set to huge positive values
  !    sr%il = -1               ! set to invalid indexes

  sr%ind => tp%ind
  sr%rearrange = tp%rearrange

  IF (tp%rearrange) THEN
    sr%DATA => tp%rearranged_data
  ELSE
    sr%DATA => tp%the_data
  END IF
  sr%rearrange = tp%rearrange
  sr%dimen = tp%dimen

  !
  !sr%dsl = Huge(sr%dsl)    ! set to huge positive values
  !sr%il = -1               ! set to invalid indexes
  !

  CALL search(tp%root)
  nfound = sr%nfound
  IF (tp%sort) THEN
    CALL Kdtree2_sort_results(nfound, results)
  END IF

#ifdef DEBUG_VER

  IF (sr%overflow) THEN
    CALL Errormsg(msg=msg, file=__FILE__, line=__LINE__, &
                  routine="Kdtree2_r_nearest_around_point()", unitno=stderr)
  END IF

#endif

  DEALLOCATE (sr%qv)
END SUBROUTINE Kdtree2_r_nearest_around_point

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Count the number of neighbors within square distance 'r2'.
FUNCTION Kdtree2_r_count(tp, qv, r2) RESULT(nfound)
  TYPE(Kdtree2_), TARGET, INTENT(INOUT) :: tp
  REAL(kdkind), TARGET, INTENT(IN) :: qv(:)
  REAL(kdkind), INTENT(IN) :: r2
  INTEGER :: nfound

  INTRINSIC HUGE

  sr%qv => qv
  sr%ballsize = r2

  sr%nn = 0 ! flag for fixed r search
  sr%nfound = 0
  sr%centeridx = -1
  sr%correltime = 0

  NULLIFY (sr%results) ! for some reason, FTN 95 chokes on '=> null()'

  sr%nalloc = 0 ! we do not allocate any storage but that's OK
  ! for counting.
  sr%ind => tp%ind
  sr%rearrange = tp%rearrange
  IF (tp%rearrange) THEN
    sr%DATA => tp%rearranged_data
  ELSE
    sr%DATA => tp%the_data
  END IF
  sr%dimen = tp%dimen

  sr%overflow = .FALSE.

  CALL search(tp%root)

  nfound = sr%nfound

END FUNCTION Kdtree2_r_count

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Count the number of neighbors within square distance 'r2' around
! point 'idxin' with decorrelation time 'correltime'.

FUNCTION Kdtree2_r_count_around_point(tp, idxin, correltime, r2) &
  RESULT(nfound)
  TYPE(Kdtree2_), TARGET, INTENT(INOUT) :: tp
  INTEGER, INTENT(IN) :: correltime, idxin
  REAL(kdkind), INTENT(IN) :: r2
  INTEGER :: nfound

  ALLOCATE (sr%qv(tp%dimen))
  sr%qv = tp%the_data(:, idxin)
  sr%ballsize = r2

  sr%nn = 0 ! flag for fixed r search
  sr%nfound = 0
  sr%centeridx = idxin
  sr%correltime = correltime
  NULLIFY (sr%results)

  sr%nalloc = 0 ! we do not allocate any storage but that's OK
  ! for counting.

  sr%ind => tp%ind
  sr%rearrange = tp%rearrange

  IF (sr%rearrange) THEN
    sr%DATA => tp%rearranged_data
  ELSE
    sr%DATA => tp%the_data
  END IF
  sr%dimen = tp%dimen

  sr%overflow = .FALSE.

  CALL search(tp%root)

  nfound = sr%nfound

END FUNCTION Kdtree2_r_count_around_point

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-10
! summary:  check storage when run in debug mode

SUBROUTINE validate_query_storage(n)
  INTEGER, INTENT(IN) :: n

#ifdef DEBUG_VER

  CHARACTER(*), PARAMETER :: msg = "Not enough storage for results"
  LOGICAL(LGT) :: problem

  problem = SIZE(sr%results, 1) .LT. n
  IF (problem) THEN
    CALL ErrorMsg( &
      msg=msg, &
      line=__LINE__, &
      unitno=stderr, &
      file=__FILE__, &
      routine="validate_query_storage()")
    STOP
  END IF

#endif

END SUBROUTINE validate_query_storage

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! distance between iv[1:n] and qv[1:n]
! .. Function Return Value ..
! re-implemented to improve vectorization.
FUNCTION square_distance(d, iv, qv) RESULT(res)
  REAL(kdkind) :: res
  INTEGER :: d
  REAL(kdkind) :: iv(:), qv(:)
  res = SUM((iv(1:d) - qv(1:d))**2)
END FUNCTION square_distance

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! This is the innermost core routine of the kd-tree search.  Along
! with "process_terminal_node", it is the performance bottleneck.
!
! This version uses a logically complete secondary search of
! "box in bounds", whether the sear
RECURSIVE SUBROUTINE search(node)
  TYPE(Kdtree2Node_), POINTER :: node

  !  internal variables
  TYPE(Kdtree2Node_), POINTER :: ncloser, nfarther
  INTEGER :: cut_dim, i
  REAL(kdkind) :: qval, dis
  REAL(kdkind) :: ballsize
  REAL(kdkind), POINTER :: qv(:)
  TYPE(interval), POINTER :: box(:)
  LOGICAL(LGT) :: isok

  isok = (ASSOCIATED(node%left) .AND. ASSOCIATED(node%right)) .EQV. .FALSE.

  IF (isok) THEN

    ! we are on a terminal node
    IF (sr%nn .EQ. 0) THEN
      CALL process_terminal_node_fixedball(node)
    ELSE
      CALL process_terminal_node(node)
    END IF
    RETURN

  END IF

  ! we are not on a terminal node
  qv => sr%qv(1:)
  cut_dim = node%cut_dim
  qval = qv(cut_dim)

  IF (qval < node%cut_val) THEN
    ncloser => node%left
    nfarther => node%right
    dis = (node%cut_val_right - qval)**2
    !  extra = node%cut_val - qval
  ELSE
    ncloser => node%right
    nfarther => node%left
    dis = (node%cut_val_left - qval)**2
    !  extra = qval- node%cut_val_left
  END IF

  IF (ASSOCIATED(ncloser)) CALL search(ncloser)

  ! we may need to search the second node.
  isok = ASSOCIATED(nfarther)
  IF (.NOT. isok) RETURN

  ballsize = sr%ballsize
  ! dis=extra**2

  isok = dis <= ballsize
  IF (.NOT. isok) RETURN

  ! we do this separately as going on the first cut dimen is often
  ! a good idea.
  ! note that if extra**2 < sr%ballsize, then the next
  ! check will also be false.
  box => node%box(1:)
  DO i = 1, sr%dimen
    IF (i .NE. cut_dim) THEN
      dis = dis + dis2_from_bnd(qv(i), box(i)%lower, box(i)%upper)
      IF (dis > ballsize) RETURN
    END IF
  END DO

  ! if we are still here then we need to search mroe.
  CALL search(nfarther)

END SUBROUTINE search

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

REAL(kdkind) FUNCTION dis2_from_bnd(x, amin, amax) RESULT(res)
  REAL(kdkind), INTENT(IN) :: x, amin, amax
  LOGICAL(LGT) :: isok

  res = 0.0

  isok = x > amax
  IF (isok) THEN
    res = (x - amax)**2
    RETURN
  END IF

  isok = x < amin
  IF (isok) res = (amin - x)**2
END FUNCTION dis2_from_bnd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Look for actual near neighbors in 'node', and update
! the search results on the sr data structure.
SUBROUTINE process_terminal_node(node)
  TYPE(Kdtree2Node_), POINTER :: node
  !
  REAL(kdkind), POINTER :: qv(:)
  INTEGER, POINTER :: ind(:)
  REAL(kdkind), POINTER :: DATA(:, :)
  !
  INTEGER :: dimen, i, indexofi, k, centeridx, correltime
  REAL(kdkind) :: ballsize, sd, newpri
  LOGICAL :: rearrange
  TYPE(pq), POINTER :: pqp
  !
  ! copy values from sr to local variables
  !
  !
  ! Notice, making local pointers with an EXPLICIT lower bound
  ! seems to generate faster code.
  ! why?  I don't know.
  qv => sr%qv(1:)
  pqp => sr%pq
  dimen = sr%dimen
  ballsize = sr%ballsize
  rearrange = sr%rearrange
  ind => sr%ind(1:)
  DATA => sr%DATA(1:, 1:)
  centeridx = sr%centeridx
  correltime = sr%correltime

  !    doing_correl = (centeridx >= 0)  ! Do we have a decorrelation window?
  !    include_point = .true.    ! by default include all points
  ! search through terminal bucket.

  mainloop: DO i = node%l, node%u
    IF (rearrange) THEN
      sd = 0.0
      DO k = 1, dimen
        sd = sd + (DATA(k, i) - qv(k))**2
        IF (sd > ballsize) CYCLE mainloop
      END DO
      indexofi = ind(i) ! only read it if we have not broken out
    ELSE
      indexofi = ind(i)
      sd = 0.0
      DO k = 1, dimen
        sd = sd + (DATA(k, indexofi) - qv(k))**2
        IF (sd > ballsize) CYCLE mainloop
      END DO
    END IF

    IF (centeridx > 0) THEN ! doing correlation interval?
      IF (ABS(indexofi - centeridx) < correltime) CYCLE mainloop
    END IF

    !
    ! two choices for any point.  The list so far is either undersized,
    ! or it is not.
    !
    ! If it is undersized, then add the point and its distance
    ! unconditionally.  If the point added fills up the working
    ! list then set the sr%ballsize, maximum distance bound (largest distance on
    ! list) to be that distance, instead of the initialized +infinity.
    !
    ! If the running list is full size, then compute the
    ! distance but break out immediately if it is larger
    ! than sr%ballsize, "best squared distance" (of the largest element),
    ! as it cannot be a good neighbor.
    !
    ! Once computed, compare to best_square distance.
    ! if it is smaller, then delete the previous largest
    ! element and add the new one.

    IF (sr%nfound .LT. sr%nn) THEN
      !
      ! add this point unconditionally to fill list.
      !
      sr%nfound = sr%nfound + 1
      newpri = pq_insert(pqp, sd, indexofi)
      IF (sr%nfound .EQ. sr%nn) ballsize = newpri
      ! we have just filled the working list.
      ! put the best square distance to the maximum value
      ! on the list, which is extractable from the PQ.

    ELSE
      !
      ! now, if we get here,
      ! we know that the current node has a squared
      ! distance smaller than the largest one on the list, and
      ! belongs on the list.
      ! Hence we replace that with the current one.
      !
      ballsize = pq_replace_max(pqp, sd, indexofi)
    END IF
  END DO mainloop
  !
  ! Reset sr variables which may have changed during loop
  !
  sr%ballsize = ballsize

END SUBROUTINE process_terminal_node

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Look for actual near neighbors in 'node', and update
! the search results on the sr data structure, i.e.
! save all within a fixed ball.
SUBROUTINE process_terminal_node_fixedball(node)
  TYPE(Kdtree2Node_), POINTER :: node
  !
  REAL(kdkind), POINTER :: qv(:)
  INTEGER, POINTER :: ind(:)
  REAL(kdkind), POINTER :: DATA(:, :)
  !
  INTEGER :: nfound
  INTEGER :: dimen, i, indexofi, k
  INTEGER :: centeridx, correltime, nn
  REAL(kdkind) :: ballsize, sd
  LOGICAL :: rearrange

  ! copy values from sr to local variables
  qv => sr%qv(1:)
  dimen = sr%dimen
  ballsize = sr%ballsize
  rearrange = sr%rearrange
  ind => sr%ind(1:)
  DATA => sr%DATA(1:, 1:)
  centeridx = sr%centeridx
  correltime = sr%correltime
  nn = sr%nn ! number to search for
  nfound = sr%nfound

  ! search through terminal bucket.
  mainloop: DO i = node%l, node%u

    !
    ! two choices for any point.  The list so far is either undersized,
    ! or it is not.
    !
    ! If it is undersized, then add the point and its distance
    ! unconditionally.  If the point added fills up the working
    ! list then set the sr%ballsize, maximum distance bound
    ! (largest distance on list) to be that distance,
    ! instead of the initialized +infinity.
    !
    ! If the running list is full size, then compute the
    ! distance but break out immediately if it is larger
    ! than sr%ballsize, "best squared distance" (of the largest element),
    ! as it cannot be a good neighbor.
    !
    ! Once computed, compare to best_square distance.
    ! if it is smaller, then delete the previous largest
    ! element and add the new one.

    ! which index to the point do we use?

    IF (rearrange) THEN
      sd = 0.0
      DO k = 1, dimen
        sd = sd + (DATA(k, i) - qv(k))**2
        IF (sd > ballsize) CYCLE mainloop
      END DO
      indexofi = ind(i) ! only read it if we have not broken out
    ELSE
      indexofi = ind(i)
      sd = 0.0
      DO k = 1, dimen
        sd = sd + (DATA(k, indexofi) - qv(k))**2
        IF (sd > ballsize) CYCLE mainloop
      END DO
    END IF

    IF (centeridx > 0) THEN ! doing correlation interval?
      IF (ABS(indexofi - centeridx) < correltime) CYCLE mainloop
    END IF

    nfound = nfound + 1
    IF (nfound .GT. sr%nalloc) THEN
      ! oh nuts, we have to add another one to the tree but
      ! there isn't enough room.
      sr%overflow = .TRUE.
    ELSE
      sr%results(nfound)%dis = sd
      sr%results(nfound)%idx = indexofi
    END IF
  END DO mainloop

  ! Reset sr variables which may have changed during loop
  sr%nfound = nfound
END SUBROUTINE process_terminal_node_fixedball

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Matthew Kennel
! date:  2024-04-10
! summary:  Used for benchmarking only
!
! find the 'n' nearest neighbors to 'qv' by exhaustive search.
! only use this subroutine for testing, as it is SLOW!  The
! whole point of a k-d tree is to avoid doing what this subroutine
! does.

SUBROUTINE Kdtree2_n_nearest_brute_force(tp, qv, nn, results)
  TYPE(Kdtree2_), TARGET, INTENT(INOUT) :: tp
  REAL(kdkind), INTENT(IN) :: qv(:)
  INTEGER, INTENT(IN) :: nn
  TYPE(Kdtree2Result_) :: results(:)

  INTEGER :: i, j, k
  REAL(kdkind), ALLOCATABLE :: all_distances(:)
  ! ..
  ALLOCATE (all_distances(tp%n))
  DO i = 1, tp%n
    all_distances(i) = square_distance(tp%dimen, qv, tp%the_data(:, i))
  END DO
  ! now find 'n' smallest distances
  DO i = 1, nn
    results(i)%dis = HUGE(1.0)
    results(i)%idx = -1
  END DO
  DO i = 1, tp%n
    IF (all_distances(i) < results(nn)%dis) THEN
      ! insert it somewhere on the list
      DO j = 1, nn
        IF (all_distances(i) < results(j)%dis) EXIT
      END DO
      ! now we know 'j'
      DO k = nn - 1, j, -1
        results(k + 1) = results(k)
      END DO
      results(j)%dis = all_distances(i)
      results(j)%idx = i
    END IF
  END DO
  DEALLOCATE (all_distances)
END SUBROUTINE Kdtree2_n_nearest_brute_force

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Kdtree2_r_nearest_brute_force(tp, qv, r2, nfound, results)
  ! find the nearest neighbors to 'qv' with distance**2 <= r2 by exhaustive
  ! search.
  ! only use this subroutine for testing, as it is SLOW!  The
  ! whole point of a k-d tree is to avoid doing what this subroutine
  ! does.
  TYPE(Kdtree2_), TARGET, INTENT(INOUT) :: tp
  REAL(kdkind), INTENT(IN) :: qv(:)
  REAL(kdkind), INTENT(IN) :: r2
  INTEGER, INTENT(out) :: nfound
  TYPE(Kdtree2Result_) :: results(:)

  INTEGER :: i, nalloc
  REAL(kdkind), ALLOCATABLE :: all_distances(:)
  ! ..
  ALLOCATE (all_distances(tp%n))
  DO i = 1, tp%n
    all_distances(i) = square_distance(tp%dimen, qv, tp%the_data(:, i))
  END DO

  nfound = 0
  nalloc = SIZE(results, 1)

  DO i = 1, tp%n
    IF (all_distances(i) < r2) THEN
      ! insert it somewhere on the list
      IF (nfound .LT. nalloc) THEN
        nfound = nfound + 1
        results(nfound)%dis = all_distances(i)
        results(nfound)%idx = i
      END IF
    END IF
  END DO
  DEALLOCATE (all_distances)

  CALL Kdtree2_sort_results(nfound, results)

END SUBROUTINE Kdtree2_r_nearest_brute_force

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!  Use after search to sort results(1:nfound) in order of increasing
!  distance.
SUBROUTINE Kdtree2_sort_results(nfound, results)
  INTEGER, INTENT(IN) :: nfound
  TYPE(Kdtree2Result_), TARGET :: results(:)
  IF (nfound .GT. 1) CALL heapsort_struct(results, nfound)
END SUBROUTINE Kdtree2_sort_results

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Sort a(1:n) in ascending order
SUBROUTINE heapsort_struct(a, n)
  INTEGER, INTENT(IN) :: n
  TYPE(Kdtree2Result_), INTENT(INOUT) :: a(:)

  TYPE(Kdtree2Result_) :: VALUE ! temporary value
  INTEGER :: i, j
  INTEGER :: ileft, iright

  ileft = n / 2 + 1
  iright = n

  IF (n .EQ. 1) RETURN

  DO
    IF (ileft > 1) THEN
      ileft = ileft - 1
      VALUE = a(ileft)
    ELSE
      VALUE = a(iright)
      a(iright) = a(1)
      iright = iright - 1
      IF (iright == 1) THEN
        a(1) = VALUE
        RETURN
      END IF
    END IF

    i = ileft
    j = 2 * ileft
    DO WHILE (j <= iright)
      IF (j < iright) THEN
        IF (a(j)%dis < a(j + 1)%dis) j = j + 1
      END IF
      IF (VALUE%dis < a(j)%dis) THEN
        a(i) = a(j); 
        i = j
        j = j + j
      ELSE
        j = iright + 1
      END IF
    END DO
    a(i) = VALUE

  END DO
END SUBROUTINE heapsort_struct

END MODULE Kdtree2_Module
