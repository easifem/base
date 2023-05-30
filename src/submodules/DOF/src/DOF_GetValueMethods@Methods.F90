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

SUBMODULE(DOF_GetValueMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             getArrayvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getvalue1
INTEGER(I4B) :: m, n, i, k, tdof
!
!
!
m = SIZE(idof)
n = SIZE(nodenum)
!
CALL Reallocate(v, m * n)
!
SELECT CASE (obj%StorageFMT)
  !
  !
  !
CASE (DOF_FMT)
  !
  IF (StorageFMT .EQ. NODES_FMT) THEN
    !
    DO i = 1, m
      DO k = 1, n
        v((k - 1) * m + i) = &
          & val(nodenum(k) + obj%valmap(idof(i)) - 1)
      END DO
    END DO
    !
  ELSE
    !
    DO i = 1, m
      v((i - 1) * n + 1:i * n) = &
        & val(nodenum + obj%valmap(idof(i)) - 1)
    END DO
    !
  END IF
  !
  !
  !
CASE (NODES_FMT)
  !
  tdof = .tdof.obj
  !
  IF (StorageFMT .EQ. dof_FMT) THEN
    !
    DO i = 1, n
      DO k = 1, m
        v((k - 1) * n + i) = val((nodenum(i) - 1) * tdof + idof(k))
      END DO
    END DO
    !
  ELSE
    !
    DO i = 1, n
      DO k = 1, m
        v((i - 1) * m + k) &
          & = val((nodenum(i) - 1) * tdof + idof(k))
      END DO
    END DO
    !
  END IF
  !
END SELECT
!
END PROCEDURE dof_getvalue1

!----------------------------------------------------------------------------
!                                                             getArrayvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getvalue2
INTEGER(I4B) :: m, n, i, k, tdof
!
!
!
k = obj%valmap(idof(1) + 1) - obj%valmap(idof(1))
m = SIZE(idof)
!
DO i = 1, m
  k = MAX(k, obj%valmap(idof(i) + 1) - obj%valmap(idof(i)))
END DO
!
IF (PRESENT(force3D) .AND. m .LT. 3) THEN
  CALL reallocate(v, 3, k)
ELSE
  CALL reallocate(v, m, k)
END IF
!
SELECT CASE (obj%StorageFMT)
  !
  !
  !
CASE (DOF_FMT)
  !
  tdof = .tdof.obj
  DO i = 1, m
    n = obj%valmap(idof(i) + 1) - obj%valmap(idof(i))
    ! length of idof( i )
    DO k = 1, n
      v(i, k) = val(k + obj%valmap(idof(i)) - 1)
    END DO
  END DO
  !
  !
  !
CASE (NODES_FMT)
  !
  tdof = .tdof.obj
  n = obj%valmap(2) - obj%valmap(1) ! size of dof; homogenous
  DO i = 1, n
    DO k = 1, m
      v(k, i) = val((i - 1) * tdof + idof(k))
    END DO
  END DO
  !
END SELECT
!
END PROCEDURE dof_getvalue2

!----------------------------------------------------------------------------
!                                                             getArrayvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_getvalue3
INTEGER(I4B) :: m, n, i, k, tdof
!
! main
!
k = 0
DO i = 1, SIZE(idof)
  k = k + obj%valmap(idof(i) + 1) - obj%valmap(idof(i))
END DO
!
CALL reallocate(v, k)
!
SELECT CASE (obj%StorageFMT)
  !
  !
  !
CASE (DOF_FMT)
  !
  IF (StorageFMT .EQ. nodes_FMT) THEN
    !
    tdof = .tdof.obj
    m = SIZE(idof)
    DO i = 1, m
      n = obj%valmap(idof(i) + 1) - obj%valmap(idof(i))
      DO k = 1, n
        v((k - 1) * m + i) = val(k + obj%valmap(idof(i)) - 1)
      END DO
    END DO
    !
  ELSE
    !
    m = 0; n = 0
    DO i = 1, SIZE(idof)
      m = n + 1
      n = n + obj%valmap(idof(i) + 1) - obj%valmap(idof(i))
      v(m:n) = &
        & val(obj%valmap(idof(i)):obj%valmap(idof(i + 1) - 1))
    END DO
    !
  END IF
  !
  !
  !
CASE (Nodes_FMT)
  !
  tdof = .tdof.obj
  m = SIZE(idof)
  !
  IF (StorageFMT .EQ. dof_FMT) THEN
    n = obj%valmap(2) - obj%valmap(1)
    DO i = 1, n
      DO k = 1, m
        v((k - 1) * n + i) = val((i - 1) * tdof + idof(k))
      END DO
    END DO
    !
  ELSE
    !
    DO i = 1, obj%valmap(2) - obj%valmap(1)
      DO k = 1, m
        v((i - 1) * m + k) &
          & = val((i - 1) * tdof + idof(k))
      END DO
    END DO
  END IF
  !
END SELECT
!
!
END PROCEDURE dof_getvalue3

!----------------------------------------------------------------------------
!                                                                Arrayvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_get1
!
CALL getvalue( &
  & v=ans, &
  & val=val, &
  & obj=obj, &
  & idof=idof, &
  & nodenum=nodenum, &
  & StorageFMT=StorageFMT)
!
END PROCEDURE dof_get1

!----------------------------------------------------------------------------
!                                                                Arrayvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_get2
!
CALL getvalue( &
  & v=ans, &
  & val=val, &
  & obj=obj, &
  & idof=idof, &
  & StorageFMT=StorageFMT)
!
END PROCEDURE dof_get2

END SUBMODULE Methods
