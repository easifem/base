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
!                                                               getSUPGParam
!----------------------------------------------------------------------------

INTEGER(I4B) :: ierr
TYPE(String) :: msg
!!
ierr = 0_I4B
msg = "Unknown error"
!!
SELECT CASE (opt)
CASE (1)
  !!
  CALL DBEStabParam1a( &
    & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
    & phi=phi, L0=L0, dt=dt)
  !!
CASE (2)
  !!
  CALL DBEStabParam2a( &
    & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
    & phi=phi, L0=L0, dt=dt)
  !!
CASE (3)
  !!
  IF (.NOT. PRESENT(L0)) THEN
    ierr = 3
    msg = "opt=3, L0 should be present"
  ELSE
    CALL DBEStabParam3a( &
      & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
      & phi=phi, L0=L0, dt=dt)
  END IF
  !!
CASE (4)
  !!
  CALL DBEStabParam4a( &
    & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
    & phi=phi, L0=L0, dt=dt)
  !!
CASE (5)
  !!
  IF (.NOT. PRESENT(L0) .OR. .NOT. PRESENT(c4)) THEN
    ierr = 5
    msg = "opt=5, L0 and c4 should be present"
  ELSE
    CALL DBEStabParam5a( &
      & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
      & phi=phi, L0=L0, c4=c4, dt=dt)
  END IF
  !!
CASE (6)
  !!
  IF (.NOT. PRESENT(L0) .OR. .NOT. PRESENT(c4)) THEN
    ierr = 6
    msg = "opt=6, L0 and c4 should be present"
  ELSE
    CALL DBEStabParam6a( &
      & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
      & phi=phi, L0=L0, c4=c4, dt=dt)
  END IF
  !!
CASE (7)
  !!
  IF (.NOT. PRESENT(L0) .OR. .NOT. PRESENT(c4)) THEN
    ierr = 7
    msg = "opt=7, L0 and c4 should be present"
  ELSE
    CALL DBEStabParam7a( &
      & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
      & phi=phi, L0=L0, c4=c4, dt=dt)
  END IF
  !!
CASE (8)
  !!
  IF (.NOT. PRESENT(L0) .OR. .NOT. PRESENT(c4)) THEN
    ierr = 8
    msg = "opt=8, L0 and c4 should be present"
  ELSE
    CALL DBEStabParam8a( &
      & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
      & phi=phi, L0=L0, c4=c4, dt=dt)
  END IF
  !!
CASE (11)
  !!
  CALL DBEStabParam11a( &
    & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
    & phi=phi, L0=L0, dt=dt)
  !!
CASE (12)
  !!
  CALL DBEStabParam12a( &
    & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
    & phi=phi, L0=L0, dt=dt)
  !!
CASE (13)
  !!
  IF (.NOT. PRESENT(L0)) THEN
    ierr = 13
    msg = "opt=13, L0 should be present"
  ELSE
    CALL DBEStabParam13a( &
      & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
      & phi=phi, L0=L0, dt=dt)
  END IF
  !!
CASE (14)
  !!
  CALL DBEStabParam14a( &
    & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
    & phi=phi, L0=L0, dt=dt)
  !!
CASE (15)
  !!
  IF (.NOT. PRESENT(L0) .OR. .NOT. PRESENT(c4)) THEN
    ierr = 15
    msg = "opt=15, L0 and c4 should be present"
  ELSE
    CALL DBEStabParam15a( &
      & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
      & phi=phi, L0=L0, c4=c4, dt=dt)
  END IF
  !!
CASE (16)
  !!
  IF (.NOT. PRESENT(L0) .OR. .NOT. PRESENT(c4)) THEN
    ierr = 16
    msg = "opt=16, L0 and c4 should be present"
  ELSE
    CALL DBEStabParam16a( &
      & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
      & phi=phi, L0=L0, c4=c4, dt=dt)
  END IF
  !!
CASE (17)
  !!
  IF (.NOT. PRESENT(L0) .OR. .NOT. PRESENT(c4)) THEN
    ierr = 17
    msg = "opt=17, L0 and c4 should be present"
  ELSE
    CALL DBEStabParam17a( &
      & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
      & phi=phi, L0=L0, c4=c4, dt=dt)
  END IF
  !!
CASE (18)
  !!
  !!
  IF (.NOT. PRESENT(L0) .OR. .NOT. PRESENT(c4)) THEN
    ierr = 18
    msg = "opt=18, L0 and c4 should be present"
  ELSE
    CALL DBEStabParam18a( &
      & obj=obj, tau=tau, h=h, nulsic=nulsic, c=c, val=val, nu=nu, k=k, &
      & phi=phi, L0=L0, c4=c4, dt=dt)
  END IF
  !!
CASE DEFAULT
  ierr = 100
  msg = "No rule found for opt="//tostring(opt)
END SELECT
!!
!!
!!
IF (ierr .NE. 0) THEN
  CALL Errormsg( &
  & msg=msg%chars()//" Error-code="//tostring(ierr), &
  & file=__FILE__, routine="elemsd_GetDBEStabParam1()", line=__LINE__, &
  & unitno=stderr)
END IF
!!
