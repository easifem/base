LOGICAL(LGT) :: isok, abool, ispresent
INTEGER(I4B) :: ii

isok = ALLOCATED(vec1)

IF (isok) THEN

  abool = SIZE(Vec1) .NE. n1

  IF (abool) THEN
    DEALLOCATE (Vec1)
    ALLOCATE (Vec1(n1))
  END IF

ELSE
  ALLOCATE (Vec1(n1))
END IF

DO CONCURRENT(ii=1:n1)
  vec1(ii) = ZERO1
END DO

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

isok = ALLOCATED(vec2)

IF (isok) THEN

  abool = SIZE(Vec2) .NE. n2

  IF (abool) THEN
    DEALLOCATE (Vec2)
    ALLOCATE (Vec2(n2))
  END IF

ELSE
  ALLOCATE (Vec2(n2))
END IF

DO CONCURRENT(ii=1:n2)
  vec2(ii) = ZERO2
END DO

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ispresent = PRESENT(vec3)

IF (ispresent) THEN

  isok = ALLOCATED(vec3)

  IF (isok) THEN

    abool = SIZE(Vec3) .NE. n3

    IF (abool) THEN
      DEALLOCATE (Vec3)
      ALLOCATE (Vec3(n3))
    END IF

  ELSE
    ALLOCATE (Vec3(n3))
  END IF

  DO CONCURRENT(ii=1:n3)
    vec3(ii) = ZERO3
  END DO

END IF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ispresent = PRESENT(vec4)

IF (ispresent) THEN

  isok = ALLOCATED(vec4)

  IF (isok) THEN

    abool = SIZE(Vec4) .NE. n4

    IF (abool) THEN
      DEALLOCATE (Vec4)
      ALLOCATE (Vec4(n4))
    END IF

  ELSE
    ALLOCATE (Vec4(n4))
  END IF

  DO CONCURRENT(ii=1:n4)
    vec4(ii) = ZERO4
  END DO

END IF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ispresent = PRESENT(vec5)

IF (ispresent) THEN

  isok = ALLOCATED(vec5)

  IF (isok) THEN

    abool = SIZE(Vec5) .NE. n5

    IF (abool) THEN
      DEALLOCATE (Vec5)
      ALLOCATE (Vec5(n5))
    END IF

  ELSE
    ALLOCATE (Vec5(n5))
  END IF

  DO CONCURRENT(ii=1:n5)
    vec5(ii) = ZERO5
  END DO

END IF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ispresent = PRESENT(vec6)

IF (ispresent) THEN

  isok = ALLOCATED(vec6)

  IF (isok) THEN

    abool = SIZE(Vec6) .NE. n6

    IF (abool) THEN
      DEALLOCATE (Vec6)
      ALLOCATE (Vec6(n6))
    END IF

  ELSE
    ALLOCATE (Vec6(n6))
  END IF

  DO CONCURRENT(ii=1:n6)
    vec6(ii) = ZERO6
  END DO

END IF
