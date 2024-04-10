! Copyright 2015 Dennis Decker Jensen
! See <http://testanything.org> and <https://metacpan.org/pod/Test::More>
! Tectonics: gfortran -g -Wall -Wextra -std=f2008ts -c test.f08
!
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

MODULE Test_Method
USE Test_Base, ONLY: test_unit, diago_unit, &
                  & ok, diago, note, PASS, fail, todo
USE Test_Planning, ONLY: plan, done_testing, skip_all, bail_out
USE Test_More, ONLY: is, isabs, isrel, isnear, skip
END MODULE Test_Method
