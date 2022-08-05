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

n = SIZE( array )
IF( n .EQ. 1) RETURN
l=n/2+1
k=n
DO WHILE( k .NE. 1 )
  IF( l .GT. 1 ) THEN
    l=l-1
    t=array(L)
  ELSE
    t=array(k)
    array(k)=array(1)
    k=k-1
    IF( k .EQ. 1 ) THEN
      array(1)=t
      EXIT
    ENDIF
  ENDIF
  i=l
  j=l+l
  DO WHILE( j .LE. k )
    IF( j .LT. k ) THEN
      IF( array( j ) .LT. array( j+1 ) ) j=j+1
    ENDIF
    IF ( t .LT. array(j) ) THEN
      array(i)=array(j)
      i=j
      j=j+j
    ELSE
      j=k+1
    ENDIF
  END DO
  array(i)=t
ENDDO