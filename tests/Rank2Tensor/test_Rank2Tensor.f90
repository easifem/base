! This program is a part of EASIFEM library,
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D,
!
! This program is free software: you can redistribute it and/or modify,
! it under the terms of the GNU General Public License as published by,
! the Free Software Foundation, either version 3 of the License, or,
! (at your option) any later version.,
!
! This program is distributed in the hope that it will be useful,,
! but WITHOUT ANY WARRANTY; without even the implied warranty of,
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the,
! GNU General Public License for more details.,
!
! You should have received a copy of the GNU General Public License,
! along with this program.  If not, see <https: //www.gnu.org/licenses/>.
!

module test_Rank2Tensor
use easifemBase
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( Rank2Tensor_ ) :: obj
  real( dfp ) :: mat(3,3)
  call random_number( mat )
  call display( mat, "mat=")
  call initiate( obj, mat )
  call display( obj, "test1: ")
  call initiate( obj, sym(mat), .true.)
  call display( obj, "test2: ")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
  type( Rank2Tensor_ ) :: obj
  real( dfp ) :: v( 6 )
  call random_number(v)
  call display( v, "v: ", orient="row" )
  call Initiate( obj, VoigtRank2Tensor( v, VoigtType=StressTypeVoigt ) )
  call display( obj, "test2: obj: ")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
  type( Rank2Tensor_ ) :: obj
  real( dfp ) :: mat(3,3)
  call random_number( mat )
  call display( mat, "test3: mat=")
  obj = Rank2Tensor( mat )
  call display( obj, "test3: obj=")
  obj = Rank2Tensor( sym(mat), .true.)
  call display( obj, "test3: obj=")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
  type( Rank2Tensor_ ) :: obj
  real( dfp ) :: v(6)
  call random_number( v )
  call display( v, "test4 mat=")
  obj = Rank2Tensor( VoigtRank2Tensor(v, VoigtType=StressTypeVoigt) )
  call display( obj, "test4 obj=")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
  class( Rank2Tensor_ ), pointer :: obj
  real( dfp ) :: mat(3,3)
  call random_number( mat )
  call display( mat, "test5: mat=")
  obj => Rank2Tensor_Pointer( mat )
  call display( obj, "test5: obj=")
  obj => Rank2Tensor_Pointer( sym(mat), .true.)
  call display( obj, "test5: obj=")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test6
  class( Rank2Tensor_ ), pointer :: obj
  real( dfp ) :: v(6)
  call random_number( v )
  call display( v, "test6: mat=")
  obj => Rank2Tensor_Pointer( VoigtRank2Tensor(v, VoigtType=StressTypeVoigt))
  call display( obj, "test6: obj=")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test7
  type( Rank2Tensor_ ) :: obj
  real( dfp ) :: mat( 3, 3 )
  call random_number(mat)
  obj = mat
  call display( obj, "test7: obj=")
  mat = 0.0; mat = obj
  call display( mat, "test7: mat=")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test8
  type( Rank2Tensor_ ) :: obj
  type( VoigtRank2Tensor_ ) :: v
  real( dfp ) :: mat( 3, 3 )
  call random_number(mat)
  obj = mat
  call display( obj, "test8: obj=")
  v = obj
  call display( v, "test8: v=")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test9
  type( Rank2Tensor_ ) :: obj
  call IdentityTensor(obj)
  call display( obj, "test9: IdentityTensor=")
  call Ones(obj)
  call display( obj, "test9: Ones=")
  call Zeros(obj)
  call display( obj, "test9: Zeros=")
  call IsotropicTensor(obj, 2.0_DFP)
  call display( obj, "test9: Isotropic=")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test10
  type( Rank2Tensor_ ) :: obj
  real( dfp ) :: mat( 3, 3 )
  call random_number( mat )
  obj = sym(mat)
  call display( obj, "test10: obj=" )
  call display( trace(obj), "test10: trace(obj)=" )
  call display( j2(obj), "test10: j2(obj)=" )
  call display( j3(obj), "test10: j3(obj)=" )
  call display( det(obj), "test10: det(obj)=" )
  call display( Deviatoric(obj), "test10: dev(obj)=" )
  call display( LodeAngle(obj, LodeType=SineLode), "test10: LodeAngle=" )
  call display( Sym(obj), "test10: sym=" )
  call display( SkewSym(obj), "test10: skewsym=" )
  call display( Isotropic(obj), "test10: skewsym=" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test11
  type( Rank2Tensor_ ) :: obj
  type( VoigtRank2Tensor_ ) :: V
  real( dfp ) :: mat( 3, 3 )
  call random_number( mat )
  obj = sym( mat )
  V = VoigtRank2Tensor( sym( mat ), VoigtType = StressTypeVoigt )
  call display( obj, "test11: obj=")
  call display( V, "test11: V=")
  call display( contraction(obj, obj), "test11: r2:r2 =")
  call display( contraction(obj, V), "test11: r2:r2v =")
  call display( contraction(V, V), "test11: r2v:r2v =")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test12
  type( Rank2Tensor_ ) :: obj
  real( dfp ) :: mat( 3, 3 ), QR( 3, 3 ), WR( 3 ), QI( 3, 3 ), WI( 3 )
  mat = 0.0
  mat(1,1) = 5.0
  mat(2:3, 2) = [-6, -12]
  mat(2:3, 3) = [-12, 1]
  call initiate( obj, mat, isSym=.true.)
  call Eigen( obj, QR, WR )
  call BlankLines(unitNo=stdout, NOL=2)
  call display( obj, 'test12: obj=')
  call display( Invariants(obj), "test12: Invariants=" )
  call display( QR, "test12: QR=")
  call display( WR, "test12: WR=")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test13
  type( Rank2Tensor_ ) :: obj, R, U, V
  real( dfp ) :: mat( 3, 3 ) = reshape( [1.0, -0.333, 0.959, 0.495, 1.0, 0.0, 0.5, -0.247, 1.5], [3,3] )
  call initiate( obj, mat, isSym=.false. )
  call PolarDecomp( Obj, R, U, V )
  call display( obj, "test13: obj=")
  call display( R, "test13: R=" )
  call display( U, "test13: U=" )
  call display( V, "test13: V=" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test14
  type( Rank2Tensor_ ) :: a, b
  real( dfp ) :: mat( 3, 3 )
  call random_number( mat )
  a = mat
  call random_number( mat )
  b = mat
  call display( a+b, "test14: a+b=")
  call display( a+1.0_DFP, "test14: a+1=")
  call display( 1.0_DFP + a, "test14: 1+a=")
  call display( a-b, "test14: a-b=")
  call display( a-1.0_DFP, "test14: a-1=")
  call display( 1.0_DFP - a, "test14: 1-a=")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test15
  type( Rank2Tensor_ ) :: a, b
  real( dfp ) :: mat( 3, 3 )
  call random_number( mat )
  a = mat
  call random_number( mat )
  b = mat
  call display( a*b, "test15: a*b=")
  call display( a*1.0_DFP, "test15: a*1=")
  call display( 1.0_DFP * a, "test15: 1*a=")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test16
  type( Rank2Tensor_ ) :: a
  real( dfp ) :: mat( 3, 3 ), vec( 3 )
  call random_number( mat )
  call random_number( vec )
  a = mat
  call display( matmul( a, a ), "test16: matmul(a,a)=" )
  call display( matmul( a, vec ), "test16: matmul(a,vec)=")
  call display( matmul( vec, a ), "test16: matmul(vec, a)=")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test17
  type( Rank2Tensor_ ) :: obj, invobj
  real( dfp ) :: mat( 3, 3 )
  call random_number( mat )
  obj = mat
  call inv( obj, invobj )
  call display( obj, "test17: obj = ")
  call display( invobj, "test17: obj = ")
  call display( matmul(obj,invobj), "test17: should be I=")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test18
  type( DeformationGradient_ ) :: F
  class( Rank2Tensor_ ), pointer :: objptr => null()
  real( dfp ) :: mat( 3, 3 )

  call random_number( mat )
  call display( mat, "test18: mat = ")
  F = mat !< this works too
  call display( F, "test18: F = " )
  F = DeformationGradient(Rank2Tensor(mat=mat, issym=.false.))
  call display( F, "test18: F = " )
  objptr => DeformationGradient_Pointer(Rank2Tensor(mat=mat, issym=.false.))
  call display( objptr, "test18: objptr = " )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test19
  type( Rank2Tensor_ ) :: R, U, V
  type( DeformationGradient_ ) :: F
  type( LeftCauchyGreen_ ) :: b
  type( RightCauchyGreen_ ) :: C
  real( dfp ) :: mat( 3, 3 ) = reshape( [1.0, -0.333, 0.959, 0.495, 1.0, 0.0, 0.5, -0.247, 1.5], [3,3] )

  call initiate( F, mat, isSym=.false. )
  call PolarDecomp( F, R=R, U=U, V=V )
  call display( F, "test13: obj=")
  call display( R, "test13: R=" )
  call display( U, "test13: U=" )
  call display( V, "test13: V=" )
  b = LeftCauchyGreen( F=F )
  call display( b, "b(F) = " )
  b = LeftCauchyGreen( V = V )
  call display( b, "b(V) = " )
  C = RightCauchyGreen( F=F )
  call display( C, "C(F) = " )
  C = RightCauchyGreen( U = U )
  call display( C, "C(U) = " )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_Rank2Tensor
implicit none
! call test1
! call test2
! call test3
! call test4
! call test5
! call test6
! call test7
! call test8
! call test9
! call test10
! call test11
! call test12
! call test13
! call test14
! call test15
! call test16
! call test17
! call test18
call test19

end program main
