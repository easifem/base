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

MODULE MultiIndices_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Initiate the multi indices

INTERFACE
  MODULE PURE SUBROUTINE obj_Initiate1(obj, n, d)
    TYPE(MultiIndices_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    INTEGER(I4B), INTENT(IN) :: d
  END SUBROUTINE obj_Initiate1
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE obj_Initiate1
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                       MultiIndices@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Function to construct the multi-index

INTERFACE
  MODULE PURE FUNCTION obj_MultiIndices(n, d) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    INTEGER(I4B), INTENT(IN) :: d
    TYPE(MultiIndices_) :: ans
  END FUNCTION obj_MultiIndices
END INTERFACE

INTERFACE MultiIndices
  MODULE PROCEDURE obj_MultiIndices
END INTERFACE MultiIndices

PUBLIC :: MultiIndices

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Deallocate the object

INTERFACE
  MODULE PURE SUBROUTINE obj_Deallocate(obj)
    TYPE(MultiIndices_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE Deallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE Deallocate

PUBLIC :: Deallocate

!----------------------------------------------------------------------------
!                                                           Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Display the content

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    TYPE(MultiIndices_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE obj_Display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                           Size@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Get the number of touples

INTERFACE
  MODULE PURE FUNCTION obj_Size1(obj) RESULT(ans)
    TYPE(MultiIndices_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size1
END INTERFACE

INTERFACE Size
  MODULE PROCEDURE obj_Size1
END INTERFACE Size

PUBLIC :: Size

!----------------------------------------------------------------------------
!                                                           Size@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Get the number of touples

INTERFACE
  MODULE PURE FUNCTION obj_Size2(obj, upto) RESULT(ans)
    TYPE(MultiIndices_), INTENT(IN) :: obj
    LOGICAL(LGT), INTENT(IN) :: upto
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size2
END INTERFACE

INTERFACE Size
  MODULE PROCEDURE obj_Size2
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                         GetIndices@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Get Indices

INTERFACE
  MODULE PURE FUNCTION obj_GetMultiIndices1(obj) RESULT(ans)
    TYPE(MultiIndices_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_GetMultiIndices1
END INTERFACE

INTERFACE GetMultiIndices
  MODULE PROCEDURE obj_GetMultiIndices1
END INTERFACE GetMultiIndices

PUBLIC :: GetMultiIndices

!----------------------------------------------------------------------------
!                                                         GetIndices@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Sept 2022
! summary:         Get Indices

INTERFACE
  MODULE PURE FUNCTION obj_GetMultiIndices2(obj, upto) RESULT(ans)
    TYPE(MultiIndices_), INTENT(IN) :: obj
    LOGICAL(LGT), INTENT(IN) :: upto
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_GetMultiIndices2
END INTERFACE

INTERFACE GetMultiIndices
  MODULE PROCEDURE obj_GetMultiIndices2
END INTERFACE GetMultiIndices

END MODULE MultiIndices_Method
