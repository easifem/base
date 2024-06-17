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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: r8mat solve
!
!# Introduction
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, integer ( kind = 4 ) RHS_NUM, the number of right hand sides.
!    RHS_NUM must be at least 0.
!
!    Input/output, real ( kind = 8 ) A(N,N+rhs_num), contains in rows and
!    columns 1 to N the coefficient matrix, and in columns N+1 through
!    N+rhs_num, the right hand sides.  On output, the coefficient matrix
!    area has been destroyed, while the right hand sides have
!    been overwritten with the corresponding solutions.
!
!    Output, integer ( kind = 4 ) INFO, singularity flag.
!    0, the matrix was not singular, the solutions were computed;
!    J, factorization failed on step J, and the solutions could not
!    be computed.

PURE SUBROUTINE r8mat_solve(n, rhs_num, a, info)
  INTEGER(I4B), INTENT(IN) :: n
  INTEGER(I4B), INTENT(IN) :: rhs_num
  REAL(DFP), INTENT(INOUT) :: a(n, n + rhs_num)
  INTEGER(I4B), INTENT(OUT) :: info
  !!
  REAL(DFP) :: apivot
  REAL(DFP) :: factor
  INTEGER(I4B) :: i
  INTEGER(I4B) :: ipivot
  INTEGER(I4B) :: j
  !!
  info = 0
  !!
  do j = 1, n
    !
    !  Choose a pivot row.
    !
    ipivot = j
    apivot = a(j, j)
    !
    do i = j + 1, n
      if (abs(apivot) < abs(a(i, j))) then
        apivot = a(i, j)
        ipivot = i
      end if
    end do
    !
    if (apivot == 0.0D+00) then
      info = j
      return
    end if
    !
    !  Interchange.
    !
    do i = 1, n + rhs_num
      call swap(a(ipivot, i), a(j, i))
    end do
    !
    !  A(J,J) becomes 1.
    !
    a(j, j) = 1.0D+00
    a(j, j + 1:n + rhs_num) = a(j, j + 1:n + rhs_num) / apivot
    !
    !  A(I,J) becomes 0.
    !
    do i = 1, n
      if (i /= j) then
        factor = a(i, j)
        a(i, j) = 0.0D+00
        a(i,j+1:n+rhs_num) = a(i,j+1:n+rhs_num) - factor * a(j,j+1:n+rhs_num)
      end if
    end do
  end do
END SUBROUTINE r8mat_solve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function r8vec_normsq_affine(n, v0, v1) result(ans)
  integer(i4b), intent(in) :: n
  real(dfp), intent(in) :: v0(n)
  real(dfp), intent(in) :: v1(n)
  real(dfp) :: ans
  ans = sum((v0(1:n) - v1(1:n))**2)
end function r8vec_normsq_affine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function i4_wrap(ival, ilo, ihi) result(ans)
  integer(i4b), intent(in) :: ival
  integer(i4b), intent(in) :: ilo
  integer(i4b), intent(in) :: ihi
  integer(i4b) :: ans
  !!
  integer(i4b) :: jhi
  integer(i4b) :: jlo
  integer(i4b) :: wide
  !!
  jlo = min(ilo, ihi)
  jhi = max(ilo, ihi)
  !!
  wide = jhi - jlo + 1
  !!
  if (wide == 1) then
    ans = jlo
  else
    ans = jlo + i4_modp(ival - jlo, wide)
  end if
  !!
end function

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function i4_modp(i, j) result(ans)
  integer(i4b), INTENT(IN) :: i
  integer(i4b), INTENT(IN) :: j
  integer(i4b) :: ans
  if (j == 0) then
    return
  end if
  ans = mod(i, j)
  if (ans < 0) then
    ans = ans + abs(j)
  end if
end function

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function i4vec_lcm(n, v)
  integer(i4b), intent(in) :: n
  integer(i4b), intent(in) :: v(n)
  integer(i4b) :: i4vec_lcm
  integer(i4b) :: i
  integer(i4b) :: lcm
  !
  lcm = 1
  do i = 1, n
    if (v(i) == 0) then
      lcm = 0
      i4vec_lcm = lcm
      return
    end if
    lcm = i4_lcm(lcm, v(i))
  end do
  i4vec_lcm = lcm
end function

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function i4_lcm(i, j)
  integer(i4b), intent(in) :: i, j
  INTEGER(I4B) :: i4_lcm
  i4_lcm = abs(i * (j / i4_gcd(i, j)))
end function

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function i4_gcd(i, j)
  INTEGER(I4B), INTENT(IN) :: i, j
  INTEGER(I4B) :: i4_gcd
  !!
  integer(kind=4) p
  integer(kind=4) q
  integer(kind=4) r
  !
  i4_gcd = 1
  !
  !  Return immediately if either I or J is zero.
  !
  if (i == 0) then
    i4_gcd = max(1, abs(j))
    return
  else if (j == 0) then
    i4_gcd = max(1, abs(i))
    return
  end if
  !
  !  Set P to the larger of I and J, Q to the smaller.
  !  This way, we can alter P and Q as we go.
  !
  p = max(abs(i), abs(j))
  q = min(abs(i), abs(j))
  !
  !  Carry out the Euclidean algorithm.
  !
  do
    r = mod(p, q)
    if (r == 0) then
      exit
    end if
    p = q
    q = r
  end do
  i4_gcd = q
end function

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

pure function r8_huge()
  real(dfp) :: r8_huge
  r8_huge = 1.0D+30
end function
