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

SUBMODULE(RecursiveNodesUtility) Methods
USE BaseMethod
CONTAINS

!----------------------------------------------------------------------------
!                                               RecursiveNode1D
!----------------------------------------------------------------------------

MODULE PROCEDURE RecursiveNode1D
INTEGER(I4B) :: n, jj
INTEGER(I4B), PARAMETER :: d = 1_I4B
INTEGER(I4B) :: aindx(d + 1)
REAL(DFP) :: avar
REAL(DFP), PARAMETER :: xij(2) = [0.0_DFP, 1.0_DFP]
INTEGER(I4B), ALLOCATABLE :: indices(:, :)
REAL(DFP), ALLOCATABLE :: x(:)
!!
n = order
x = InterpolationPoint_Line(order=order, ipType=ipType, xij=xij)
!!
IF (order .GT. 1) THEN
  avar = x(2)
  x(2:order) = x(3:)
  x(order + 1) = avar
END IF
!!
indices = GetMultiIndices(n=n, d=d)
CALL Reallocate(ans, SIZE(indices, 1), SIZE(indices, 2))
!!
DO jj = 1, SIZE(ans, 2)
  aindx = indices(:, jj) + 1
  avar = x(aindx(1)) + x(aindx(2))
  ans(1, jj) = x(aindx(1)) / avar
  ans(2, jj) = x(aindx(2)) / avar
END DO
!!
IF (PRESENT(domain)) THEN
  ans = Coord_Map(x=ans, from="BaryCentric", to=domain)
END IF
!!
IF (ALLOCATED(indices)) DEALLOCATE (indices)
IF (ALLOCATED(x)) DEALLOCATE (x)
!!
END PROCEDURE RecursiveNode1D

!----------------------------------------------------------------------------
!                                               RecursiveNode2D
!----------------------------------------------------------------------------

MODULE PROCEDURE RecursiveNode2D
INTEGER(I4B) :: n, jj, ii
INTEGER(I4B), PARAMETER :: d = 2_I4B
INTEGER(I4B) :: aindx(d + 1), indx(d)
REAL(DFP) :: xi, xt, b(d + 1), bs(d), Xn(order + 1)
REAL(DFP) :: BX(2, order + 1, order + 1)
INTEGER(I4B), ALLOCATABLE :: indices(:, :)
!!
n = order
CALL BarycentericNodeFamily1D(order=order, ipType=ipType, ans=BX, Xn=Xn)
!
indices = GetMultiIndices(n=n, d=d)
CALL Reallocate(ans, SIZE(indices, 1), SIZE(indices, 2))
!!
DO jj = 1, SIZE(ans, 2)
  !!
  aindx = indices(:, jj)
  xt = 0.0_DFP
  !!
  DO ii = 1, d + 1
    !!
    indx = Pop(aindx, ii)
    bs = BX(:, indx(1) + 1, indx(2) + 1)
    b = Push(vec=bs, value=0.0_DFP, pos=ii)
    xi = Xn(SUM(indx) + 1)
    xt = xt + xi
    ans(1:d + 1, jj) = ans(1:d + 1, jj) + xi * b
    !!
  END DO
  !!
  ans(:, jj) = ans(:, jj) / xt
  !!
END DO
!!
IF (PRESENT(domain)) THEN
  ans = Coord_Map(x=ans, from="BaryCentric", to=domain)
END IF
!!
!!
IF (ALLOCATED(indices)) DEALLOCATE (indices)
!!
END PROCEDURE RecursiveNode2D

!----------------------------------------------------------------------------
!                                               RecursiveNode3D
!----------------------------------------------------------------------------

MODULE PROCEDURE RecursiveNode3D
INTEGER(I4B) :: n, jj, ii
INTEGER(I4B), PARAMETER :: d = 3_I4B
INTEGER(I4B) :: aindx(d + 1), indx(d)
REAL(DFP) :: xi, xt, b(d + 1), bs(d), Xn(order + 1)
REAL(DFP) :: BX(3, order + 1, order + 1, order + 1)
INTEGER(I4B), ALLOCATABLE :: indices(:, :)
!!
n = order
CALL BarycentericNodeFamily2D(order=order, ipType=ipType, ans=BX, Xn=Xn)
!
indices = GetMultiIndices(n=n, d=d)
CALL Reallocate(ans, SIZE(indices, 1), SIZE(indices, 2))
ans = 0.0_DFP
!!
DO jj = 1, SIZE(ans, 2)
  !!
  aindx = indices(:, jj)
  xt = 0.0_DFP
  !!
  DO ii = 1, d + 1
    !!
    indx = Pop(aindx, ii)
    bs = BX(:, indx(1) + 1, indx(2) + 1, indx(3) + 1)
    b = Push(vec=bs, value=0.0_DFP, pos=ii)
    xi = Xn(SUM(indx) + 1)
    xt = xt + xi
    ans(:, jj) = ans(:, jj) + xi * b
    !!
  END DO
  !!
  ans(:, jj) = ans(:, jj) / xt
  !!
END DO
!!
IF (PRESENT(domain)) THEN
  ans = Coord_Map(x=ans, from="BaryCentric", to=domain)
END IF
!!
IF (ALLOCATED(indices)) DEALLOCATE (indices)
!!
END PROCEDURE RecursiveNode3D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE BarycentericNodeFamily1D(order, ipType, ans, Xn)
  INTEGER(I4B), INTENT(IN) :: order
  INTEGER(I4B), INTENT(IN) :: ipType
  REAL(DFP), INTENT(OUT) :: ans(2, order + 1, order + 1)
  REAL(DFP), INTENT(OUT) :: Xn(order + 1)
  !!
  INTEGER(I4B) :: ii, jj, n
  INTEGER(I4B), PARAMETER :: d = 1_I4B
  REAL(DFP), ALLOCATABLE :: BXn(:, :)
  INTEGER(I4B), ALLOCATABLE :: indices(:, :)
  !!
  DO ii = 0, order
    n = ii
    indices = GetMultiIndices(n=n, d=d)
    BXn = RecursiveNode1D(order=n, ipType=ipType)
    !!
    DO jj = 1, n + 1
      ans(1:d + 1, indices(1, jj) + 1, indices(2, jj) + 1) = BXn(1:d + 1, jj)
    END DO
    !!
  END DO
  !!
  Xn = BXn(1, :)
  !!
  IF (ALLOCATED(BXn)) DEALLOCATE (BXn)
  IF (ALLOCATED(indices)) DEALLOCATE (indices)
  !!
END SUBROUTINE BarycentericNodeFamily1D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE BarycentericNodeFamily2D(order, ipType, ans, Xn)
  INTEGER(I4B), INTENT(IN) :: order
  INTEGER(I4B), INTENT(IN) :: ipType
  REAL(DFP), INTENT(OUT) :: ans(3, order + 1, order + 1, order + 1)
  REAL(DFP), INTENT(OUT) :: Xn(order + 1)
  !!
  INTEGER(I4B) :: ii, jj, n
  INTEGER(I4B), PARAMETER :: d = 2_I4B
  REAL(DFP), ALLOCATABLE :: BXn(:, :)
  INTEGER(I4B), ALLOCATABLE :: indices(:, :)
  REAL(DFP) :: avar
  REAL(DFP), PARAMETER :: xij(2) = [0.0_DFP, 1.0_DFP]
  !!
  DO ii = 0, order
    n = ii
    indices = GetMultiIndices(n=n, d=d)
    BXn = RecursiveNode2D(order=n, ipType=ipType)
    !!
    DO jj = 1, SIZE(BXn, 2)
      ans(1:3, &
        & indices(1, jj) + 1, &
        & indices(2, jj) + 1, &
        & indices(3, jj) + 1) = BXn(1:3, jj)
    END DO
    !!
  END DO
  !!
  Xn = InterpolationPoint_Line(order=order, ipType=ipType, xij=xij)
  !!
  IF (order .GT. 1) THEN
    avar = Xn(2)
    Xn(2:order) = Xn(3:)
    Xn(order + 1) = avar
  END IF
  !!
  IF (ALLOCATED(BXn)) DEALLOCATE (BXn)
  IF (ALLOCATED(indices)) DEALLOCATE (indices)
  !!
END SUBROUTINE BarycentericNodeFamily2D

!----------------------------------------------------------------------------
!                                                          Unit2Equilateral
!----------------------------------------------------------------------------

MODULE PROCEDURE Unit2Equilateral
INTEGER(I4B) :: ii
!!
IF (d .GT. 1_I4B) THEN
  ! Move the top vertex over the centroid
  DO ii = 1, d - 1
    x(ii, :) = x(ii, :) + x(d, :) / d
  END DO
  ! Make the projection onto the lesser dimensions equilateral
  CALL Unit2Equilateral(d - 1, x(1:d - 1, :))
  ! scale the vertical dimension
  x(d, :) = x(d, :) * SQRT((d + 1.0_DFP) / (2.0_DFP * d))
END IF
END PROCEDURE Unit2Equilateral

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Equilateral2Unit
INTEGER(I4B) :: ii
!!
IF (d .GT. 1_I4B) THEN
  x(d, :) = x(d, :) / SQRT((d + 1.0_DFP) / (2.0_DFP * d))
  CALL Equilateral2Unit(d=d - 1, x=x(1:d - 1, :))
  DO ii = 1, d - 1
    x(ii, :) = x(ii, :) - x(d, :) / d
  END DO
END IF
END PROCEDURE Equilateral2Unit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ToUnit
TYPE(String) :: astr
INTEGER(I4B) :: d
astr = UpperCase(TRIM(domain))
SELECT CASE (astr%chars())
CASE ("UNIT")
  ans = x
CASE ("BIUNIT")
  ans = 0.5_DFP * (x + 1.0_DFP)
CASE ("BARYCENTRIC")
  d = SIZE(x, 1)
  ans = x(1:d - 1, :)
CASE ("EQUILATERAL")
  d = SIZE(x, 1)
  ans = x
  ans = ans / 2.0_DFP
  CALL Equilateral2Unit(d=d, x=ans)
  ans = ans + 1.0_DFP / (d + 1.0_DFP)
END SELECT
END PROCEDURE ToUnit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnit
TYPE(String) :: astr
INTEGER(I4B) :: d
astr = UpperCase(TRIM(domain))
SELECT CASE (astr%chars())
CASE ("UNIT")
  ans = x
CASE ("BIUNIT")
  ans = 2.0_DFP * x - 1
CASE ("BARYCENTRIC")
  ans = x.ROWCONCAT. (1.0_DFP - SUM(x, dim=1))
CASE ("EQUILATERAL")
  d = SIZE(x, 1)
  ans = x
  ans = ans - 1.0_DFP / (d + 1.0_DFP)
  CALL Unit2Equilateral(d=d, x=ans)
  ans = ans * 2.0_DFP
END SELECT
END PROCEDURE FromUnit

!----------------------------------------------------------------------------
!                                                                 Coord_Map
!----------------------------------------------------------------------------

MODULE PROCEDURE Coord_Map
ans = FromUnit(x=(ToUnit(x=x, domain=from)), domain=to)
END PROCEDURE Coord_Map

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
