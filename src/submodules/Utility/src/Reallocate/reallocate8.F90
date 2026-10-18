

! Each vector follows the rule of the rank 2 and rank 3 overloads:
! - not allocated: allocate exactly n entries
! - isExpand false or absent: reallocate if the size is not n
! - isExpand true: keep the vector if it has at least n entries,
!   else allocate n * expandFactor entries
! Only the first n entries are set to zero.

LOGICAL(LGT) :: abool, ex
INTEGER(I4B) :: ii, tsize, fac

ex = .FALSE.
IF (PRESENT(isExpand)) ex = isExpand

fac = 1
IF (PRESENT(expandFactor)) fac = expandFactor

IF (ALLOCATED(vec1)) THEN
  IF (ex) THEN
    abool = SIZE(vec1) .LT. n1
    tsize = n1 * fac
  ELSE
    abool = SIZE(vec1) .NE. n1
    tsize = n1
  END IF
  IF (abool) THEN
    DEALLOCATE (vec1)
    ALLOCATE (vec1(tsize))
    ! a new allocation can be larger than the request: zero all of it
    vec1 = ZERO1
  END IF
ELSE
  ALLOCATE (vec1(n1))
END IF

DO CONCURRENT(ii=1:n1)
  vec1(ii) = ZERO1
END DO

IF (ALLOCATED(vec2)) THEN
  IF (ex) THEN
    abool = SIZE(vec2) .LT. n2
    tsize = n2 * fac
  ELSE
    abool = SIZE(vec2) .NE. n2
    tsize = n2
  END IF
  IF (abool) THEN
    DEALLOCATE (vec2)
    ALLOCATE (vec2(tsize))
    ! a new allocation can be larger than the request: zero all of it
    vec2 = ZERO2
  END IF
ELSE
  ALLOCATE (vec2(n2))
END IF

DO CONCURRENT(ii=1:n2)
  vec2(ii) = ZERO2
END DO

IF (PRESENT(vec3)) THEN
  IF (ALLOCATED(vec3)) THEN
    IF (ex) THEN
      abool = SIZE(vec3) .LT. n3
      tsize = n3 * fac
    ELSE
      abool = SIZE(vec3) .NE. n3
      tsize = n3
    END IF
    IF (abool) THEN
      DEALLOCATE (vec3)
      ALLOCATE (vec3(tsize))
      ! a new allocation can be larger than the request: zero all of it
      vec3 = ZERO3
    END IF
  ELSE
    ALLOCATE (vec3(n3))
  END IF

  DO CONCURRENT(ii=1:n3)
    vec3(ii) = ZERO3
  END DO
END IF

IF (PRESENT(vec4)) THEN
  IF (ALLOCATED(vec4)) THEN
    IF (ex) THEN
      abool = SIZE(vec4) .LT. n4
      tsize = n4 * fac
    ELSE
      abool = SIZE(vec4) .NE. n4
      tsize = n4
    END IF
    IF (abool) THEN
      DEALLOCATE (vec4)
      ALLOCATE (vec4(tsize))
      ! a new allocation can be larger than the request: zero all of it
      vec4 = ZERO4
    END IF
  ELSE
    ALLOCATE (vec4(n4))
  END IF

  DO CONCURRENT(ii=1:n4)
    vec4(ii) = ZERO4
  END DO
END IF

IF (PRESENT(vec5)) THEN
  IF (ALLOCATED(vec5)) THEN
    IF (ex) THEN
      abool = SIZE(vec5) .LT. n5
      tsize = n5 * fac
    ELSE
      abool = SIZE(vec5) .NE. n5
      tsize = n5
    END IF
    IF (abool) THEN
      DEALLOCATE (vec5)
      ALLOCATE (vec5(tsize))
      ! a new allocation can be larger than the request: zero all of it
      vec5 = ZERO5
    END IF
  ELSE
    ALLOCATE (vec5(n5))
  END IF

  DO CONCURRENT(ii=1:n5)
    vec5(ii) = ZERO5
  END DO
END IF

IF (PRESENT(vec6)) THEN
  IF (ALLOCATED(vec6)) THEN
    IF (ex) THEN
      abool = SIZE(vec6) .LT. n6
      tsize = n6 * fac
    ELSE
      abool = SIZE(vec6) .NE. n6
      tsize = n6
    END IF
    IF (abool) THEN
      DEALLOCATE (vec6)
      ALLOCATE (vec6(tsize))
      ! a new allocation can be larger than the request: zero all of it
      vec6 = ZERO6
    END IF
  ELSE
    ALLOCATE (vec6(n6))
  END IF

  DO CONCURRENT(ii=1:n6)
    vec6(ii) = ZERO6
  END DO
END IF
