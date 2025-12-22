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

MODULE Tetrahedron_QuadraturePoint_Solin
USE GlobalData, ONLY: DFP, I4B, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: QuadraturePointTetrahedronSolin
PUBLIC :: QuadratureOrderTetrahedronSolin
PUBLIC :: QuadratureNumberTetrahedronSolin

INTEGER(I4B), PUBLIC, PARAMETER :: MAX_ORDER_TETRAHEDRON_SOLIN = 21

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION QuadratureOrderTetrahedronSolin(nips) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: nips
  INTEGER(I4B) :: ans
  ans = -1
  SELECT CASE (nips)
  CASE (1)
    ans = 1
  CASE (4)
    ans = 2
  CASE (5)
    ans = 3
  CASE (11)
    ans = 4
  CASE (14)
    ans = 5
  CASE (24)
    ans = 6
  CASE (31)
    ans = 7
  CASE (43)
    ans = 8
  CASE (53)
    ans = 9
  CASE (126)
    ans = 11
  CASE (210)
    ans = 13
  CASE (330)
    ans = 15
  CASE (495)
    ans = 17
  CASE (715)
    ans = 19
  CASE (1001)
    ans = 21
  END SELECT
END FUNCTION QuadratureOrderTetrahedronSolin

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION QuadratureNumberTetrahedronSolin(order) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: order
  INTEGER(I4B) :: ans
  ans = -1
  SELECT CASE (order)
  CASE (0, 1)
    ans = 1
  CASE (2)
    ans = 4
  CASE (3)
    ans = 5
  CASE (4)
    ans = 11
  CASE (5)
    ans = 14
  CASE (6)
    ans = 24
  CASE (7)
    ans = 31
  CASE (8)
    ans = 43
  CASE (9)
    ans = 53
  CASE (10)
    ans = 126
  CASE (11)
    ans = 126
  CASE (12)
    ans = 210
  CASE (13)
    ans = 210
  CASE (14)
    ans = 330
  CASE (15)
    ans = 330
  CASE (16)
    ans = 495
  CASE (17)
    ans = 495
  CASE (18)
    ans = 715
  CASE (19)
    ans = 715
  CASE (20)
    ans = 1001
  CASE (21)
    ans = 1001
  END SELECT
END FUNCTION QuadratureNumberTetrahedronSolin

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE QuadraturePointTetrahedronSolin(order, ans, nrow, ncol)
  INTEGER(I4B), INTENT(IN) :: order
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  SELECT CASE (order)
  CASE (0, 1)
    CALL QP_Tetrahedron_Order1(ans=ans, nrow=nrow, ncol=ncol)
  CASE (2)
    CALL QP_Tetrahedron_Order2(ans=ans, nrow=nrow, ncol=ncol)
  CASE (3)
    CALL QP_Tetrahedron_Order3(ans=ans, nrow=nrow, ncol=ncol)
  CASE (4)
    CALL QP_Tetrahedron_Order4(ans=ans, nrow=nrow, ncol=ncol)
  CASE (5)
    CALL QP_Tetrahedron_Order5(ans=ans, nrow=nrow, ncol=ncol)
  CASE (6)
    CALL QP_Tetrahedron_Order6(ans=ans, nrow=nrow, ncol=ncol)
  CASE (7)
    CALL QP_Tetrahedron_Order7(ans=ans, nrow=nrow, ncol=ncol)
  CASE (8)
    CALL QP_Tetrahedron_Order8(ans=ans, nrow=nrow, ncol=ncol)
  CASE (9)
    CALL QP_Tetrahedron_Order9(ans=ans, nrow=nrow, ncol=ncol)
  CASE (10)
    CALL QP_Tetrahedron_Order10(ans=ans, nrow=nrow, ncol=ncol)
  CASE (11)
    CALL QP_Tetrahedron_Order11(ans=ans, nrow=nrow, ncol=ncol)
  CASE (12)
    CALL QP_Tetrahedron_Order12(ans=ans, nrow=nrow, ncol=ncol)
  CASE (13)
    CALL QP_Tetrahedron_Order13(ans=ans, nrow=nrow, ncol=ncol)
  CASE (14)
    CALL QP_Tetrahedron_Order14(ans=ans, nrow=nrow, ncol=ncol)
  CASE (15)
    CALL QP_Tetrahedron_Order15(ans=ans, nrow=nrow, ncol=ncol)
  CASE (16)
    CALL QP_Tetrahedron_Order16(ans=ans, nrow=nrow, ncol=ncol)
  CASE (17)
    CALL QP_Tetrahedron_Order17(ans=ans, nrow=nrow, ncol=ncol)
  CASE (18)
    CALL QP_Tetrahedron_Order18(ans=ans, nrow=nrow, ncol=ncol)
  CASE (19)
    CALL QP_Tetrahedron_Order19(ans=ans, nrow=nrow, ncol=ncol)
  CASE (20)
    CALL QP_Tetrahedron_Order20(ans=ans, nrow=nrow, ncol=ncol)
  CASE (21)
    CALL QP_Tetrahedron_Order21(ans=ans, nrow=nrow, ncol=ncol)
  END SELECT

CONTAINS

#include "./include/Tetrahedron/order1.F90"
#include "./include/Tetrahedron/order2.F90"
#include "./include/Tetrahedron/order3.F90"
#include "./include/Tetrahedron/order4.F90"
#include "./include/Tetrahedron/order5.F90"
#include "./include/Tetrahedron/order6.F90"
#include "./include/Tetrahedron/order7.F90"
#include "./include/Tetrahedron/order8.F90"
#include "./include/Tetrahedron/order9.F90"
#include "./include/Tetrahedron/order10.F90"
#include "./include/Tetrahedron/order11.F90"
#include "./include/Tetrahedron/order12.F90"
#include "./include/Tetrahedron/order13.F90"
#include "./include/Tetrahedron/order14.F90"
#include "./include/Tetrahedron/order15.F90"
#include "./include/Tetrahedron/order16.F90"
#include "./include/Tetrahedron/order17.F90"
#include "./include/Tetrahedron/order18.F90"
#include "./include/Tetrahedron/order19.F90"
#include "./include/Tetrahedron/order20.F90"
#include "./include/Tetrahedron/order21.F90"

END SUBROUTINE QuadraturePointTetrahedronSolin

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Tetrahedron_QuadraturePoint_Solin
