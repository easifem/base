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
USE GlobalData, ONLY: DOF_FMT, NODES_FMT

USE ReallocateUtility, ONLY: Reallocate

USE DOF_GetMethods, ONLY: OPERATOR(.tdof.), &
                          GetNodeLoc, &
                          GetIndex_, &
                          GetIDOF

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             getArrayvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue1
INTEGER(I4B) :: tsize
CALL Reallocate(v, SIZE(idof) * SIZE(nodenum))
CALL GetValue_(v, tsize, val, obj, idof, storageFMT, &
               nodenum)
END PROCEDURE obj_GetValue1

!----------------------------------------------------------------------------
!                                                             getArrayvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue2
INTEGER(I4B) :: m, n, i, k
LOGICAL(LGT) :: abool

k = obj%valmap(idof(1) + 1) - obj%valmap(idof(1))
m = SIZE(idof)

DO i = 1, m
  k = MAX(k, obj%valmap(idof(i) + 1) - obj%valmap(idof(i)))
END DO

abool = PRESENT(force3D) .AND. (m .LT. 3)
IF (abool) m = 3

CALL Reallocate(v, m, k)
CALL GetValue_(v, val, m, k, obj, idof, force3D)

END PROCEDURE obj_GetValue2

!----------------------------------------------------------------------------
!                                                             getArrayvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue3
INTEGER(I4B) :: i, k

k = 0
DO i = 1, SIZE(idof)
  k = k + obj%valmap(idof(i) + 1) - obj%valmap(idof(i))
END DO

CALL Reallocate(v, k)
CALL GetValue_(v, k, val, obj, idof, storageFMT)

END PROCEDURE obj_GetValue3

!----------------------------------------------------------------------------
!                                                                Arrayvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get1
CALL GetValue(v=ans, val=val, obj=obj, idof=idof, nodenum=nodenum, &
              StorageFMT=StorageFMT)
END PROCEDURE obj_get1

!----------------------------------------------------------------------------
!                                                                Arrayvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get2
CALL GetValue(v=ans, val=val, obj=obj, idof=idof, StorageFMT=StorageFMT)
END PROCEDURE obj_get2

!----------------------------------------------------------------------------
!                                                                  GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_1
INTEGER(I4B) :: m, n, i, k, tdof
m = SIZE(idof)
n = SIZE(nodenum)

tsize = m * n

SELECT CASE (obj%StorageFMT)

CASE (DOF_FMT)

  ! Returned storage format is NOT same as the storage format of the object
  ! that is NODES_FMT
  IF (StorageFMT .EQ. NODES_FMT) THEN

    DO CONCURRENT(i=1:m, k=1:n)
      v((k - 1) * m + i) = val(nodenum(k) + obj%valmap(idof(i)) - 1)
    END DO

    RETURN

  END IF

  ! Returned storage format is same as the storage format of the object
  ! that is DOF_FMT
  DO CONCURRENT(i=1:m)
    v((i - 1) * n + 1:i * n) = val(nodenum + obj%valmap(idof(i)) - 1)
  END DO

CASE (NODES_FMT)

  tdof = .tdof.obj

  ! Returned storage format is NOT same as the storage format of the object
  ! that is DOF_FMT
  IF (StorageFMT .EQ. DOF_FMT) THEN

    DO CONCURRENT(i=1:n, k=1:m)
      v((k - 1) * n + i) = val((nodenum(i) - 1) * tdof + idof(k))
    END DO

    RETURN

  END IF

  ! Returned storage format is same as the storage format of the object
  ! that is NODES_FMT
  DO CONCURRENT(i=1:n, k=1:m)
    v((i - 1) * m + k) = val((nodenum(i) - 1) * tdof + idof(k))
  END DO

END SELECT

END PROCEDURE obj_GetValue_1

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_2
INTEGER(I4B) :: m, n, i, k, tdof
LOGICAL(LGT) :: abool

k = obj%valmap(idof(1) + 1) - obj%valmap(idof(1))
m = SIZE(idof)

DO i = 1, m
  k = MAX(k, obj%valmap(idof(i) + 1) - obj%valmap(idof(i)))
END DO
ncol = k

nrow = m
abool = PRESENT(force3D) .AND. (m .LT. 3)
IF (abool) nrow = 3

tdof = .tdof.obj

SELECT CASE (obj%StorageFMT)

CASE (DOF_FMT)

  DO i = 1, m
    n = obj%valmap(idof(i) + 1) - obj%valmap(idof(i))
    ! length of idof( i )
    DO k = 1, n
      v(i, k) = val(k + obj%valmap(idof(i)) - 1)
    END DO
  END DO

CASE (NODES_FMT)

  n = obj%valmap(2) - obj%valmap(1) ! size of dof; homogenous
  DO i = 1, n
    DO k = 1, m
      v(k, i) = val((i - 1) * tdof + idof(k))
    END DO
  END DO

END SELECT

END PROCEDURE obj_GetValue_2

!----------------------------------------------------------------------------
!                                                                   GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_3
INTEGER(I4B) :: m, n, i, k, tdof, tsize_idof

tsize_idof = SIZE(idof)

k = 0
DO i = 1, tsize_idof
  k = k + obj%valmap(idof(i) + 1) - obj%valmap(idof(i))
END DO

tsize = k

SELECT CASE (obj%StorageFMT)

CASE (DOF_FMT)

  IF (StorageFMT .EQ. NODES_FMT) THEN

    tdof = .tdof.obj
    m = tsize_idof
    DO i = 1, m
      n = obj%valmap(idof(i) + 1) - obj%valmap(idof(i))
      DO k = 1, n
        v((k - 1) * m + i) = val(k + obj%valmap(idof(i)) - 1)
      END DO
    END DO

    RETURN

  END IF

  m = 0; n = 0
  DO i = 1, tsize_idof
    m = n + 1
    n = n + obj%valmap(idof(i) + 1) - obj%valmap(idof(i))
    v(m:n) = &
      val(obj%valmap(idof(i)):obj%valmap(idof(i + 1) - 1))
  END DO

CASE (Nodes_FMT)

  tdof = .tdof.obj
  m = tsize_idof

  IF (StorageFMT .EQ. DOF_FMT) THEN
    n = obj%valmap(2) - obj%valmap(1)
    DO i = 1, n
      DO k = 1, m
        v((k - 1) * n + i) = val((i - 1) * tdof + idof(k))
      END DO
    END DO
    RETURN
  END IF

  DO i = 1, obj%valmap(2) - obj%valmap(1)
    DO k = 1, m
      v((i - 1) * m + k) &
        = val((i - 1) * tdof + idof(k))
    END DO
  END DO

END SELECT

END PROCEDURE obj_GetValue_3

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_4
INTEGER(I4B) :: ii, jj

tsize = .tdof.obj

DO ii = 1, tsize
  jj = GetNodeLoc(obj=obj, nodenum=nodenum, idof=ii)
  v(ii) = val(jj)
END DO

END PROCEDURE obj_GetValue_4

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_5
INTEGER(I4B) :: ii, jj, kk

tsize = obj.tdof.ivar

DO ii = 1, tsize
  kk = GetIDOF(obj=obj, ivar=ivar, idof=ii)
  jj = GetNodeLoc(obj=obj, nodenum=nodenum, idof=kk)
  v(ii) = val(jj)
END DO

END PROCEDURE obj_GetValue_5

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_6
INTEGER(I4B) :: ii, jj, kk
INTEGER(I4B), ALLOCATABLE :: indx(:)

tsize = .tdof.obj
tsize = tsize * SIZE(nodenum)
ALLOCATE (indx(tsize))
CALL GetIndex_(obj=obj, nodenum=nodenum, ans=indx, tsize=tsize)

DO CONCURRENT(ii=1:tsize)
  v(ii) = val(indx(ii))
END DO

DEALLOCATE (indx)

END PROCEDURE obj_GetValue_6

!----------------------------------------------------------------------------
!                                                                 GetValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetValue_7
INTEGER(I4B) :: ii, jj

tsize = SIZE(nodenum)

DO ii = 1, tsize
  jj = GetNodeLoc(obj=obj, nodenum=nodenum(ii), idof=idof)
  v(ii) = val(jj)
END DO

END PROCEDURE obj_GetValue_7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
