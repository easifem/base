
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

!> author: Vikas Sharma, Ph. D.
! date: 2021-02-24
! summary: All methods of the [[IntVector_]] data type
!
! This module gathers the methods of [[IntVector_]]. `USE` it to get
! all of them at once. It holds no code of its own. Each module below
! declares the interfaces of one group of methods.
!
! - [[IntVector_ConstructorMethod]]: allocate, initiate, copy, size and
!   capacity
! - [[IntVector_AppendMethod]]: `Append` and `H_CONCAT`
! - [[IntVector_EnquireMethod]]: `IsAllocated`, `IsInitiated`,
!   `IsPresent` and `.IN.`
! - [[IntVector_GetMethod]]: `Get`, `Get_` and `GetIndex`
! - [[IntVector_SetMethod]]: `Set`, `SetTotalDimension` and
!   `RemoveDuplicates`
! - [[IntVector_IOMethod]]: `Display`

MODULE IntVector_Method
USE IntVector_AppendMethod
USE IntVector_ConstructorMethod
USE IntVector_EnquireMethod
USE IntVector_GetMethod
USE IntVector_IOMethod
USE IntVector_SetMethod
END MODULE IntVector_Method
