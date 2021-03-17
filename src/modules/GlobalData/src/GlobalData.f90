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
MODULE GlobalData
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : stdin => input_unit, &
    & stdout => output_unit, stderr => error_unit
  IMPLICIT NONE
  PUBLIC
  SAVE

  LOGICAL            :: is_initialized = .FALSE., Error_Flag = .FALSE.
  INTEGER, PARAMETER  :: endianL        = 1
  INTEGER, PARAMETER  :: endianB        = 0
  INTEGER            :: endian         = endianL
  !$OMP THREADPRIVATE(is_initialized,endian, Error_Flag)

#ifdef Real128
  INTEGER, PARAMETER :: Real128 = SELECTED_REAL_KIND(33,4931)
#else
  INTEGER, PARAMETER :: Real128 = SELECTED_REAL_KIND(15,307)
#endif

  INTEGER, PARAMETER :: Real64  = SELECTED_REAL_KIND(15,307)
  INTEGER, PARAMETER :: Real32  = SELECTED_REAL_KIND(6,37)
  INTEGER, PARAMETER :: Float  = Real64  ! Default
  INTEGER, PARAMETER :: DFP  = Real64  ! Default
  INTEGER, PARAMETER :: Int64  = SELECTED_INT_KIND(18)
  INTEGER, PARAMETER :: Int32  = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: Int16  = SELECTED_INT_KIND(4)
  INTEGER, PARAMETER :: Int8  = SELECTED_INT_KIND(2)
  INTEGER, PARAMETER :: DIP  = Int32 ! Default
  INTEGER, PARAMETER :: I4B  = Int32 ! Default
  INTEGER, PARAMETER :: SP= Real32
  INTEGER, PARAMETER :: DP= Real64
  INTEGER, PARAMETER :: DFPC = KIND((1.0D0,1.0D0))   !Default Kind cmplx
  INTEGER, PARAMETER :: SPC = KIND((1.0,1.0))    !Single Precision cmplx
  INTEGER, PARAMETER :: DPC = KIND((1.0D0,1.0D0))!Double Precision cmplx
  INTEGER, PARAMETER :: LGT = KIND(.true.)       !Logical

  ! Format parameters
#ifdef Real128
  CHARACTER(*), PARAMETER :: FReal128  = '(E42.33E4)'
#else
  CHARACTER(*), PARAMETER :: FReal128  = '(E23.15E3)'
#endif
  CHARACTER(*), PARAMETER :: FReal64   = '(E23.15E3)'
  CHARACTER(*), PARAMETER :: FReal32   = '(E13.6E2)'
  CHARACTER(*), PARAMETER :: FReal   = FReal64 ! Default
  CHARACTER(*), PARAMETER :: FFloat   = FReal64 ! Default
  CHARACTER(*), PARAMETER :: FDFP   = FReal64 ! Default
  CHARACTER(*), PARAMETER :: FInt64   = '(I20)'
  CHARACTER(*), PARAMETER :: FInt64ZP = '(I20.19)'
  CHARACTER(*), PARAMETER :: FInt32   = '(I11)'
  CHARACTER(*), PARAMETER :: FInt32ZP = '(I11.10)'
  CHARACTER(*), PARAMETER :: FInt16   = '(I6)'
  CHARACTER(*), PARAMETER :: FInt16ZP = '(I6.5)'
  CHARACTER(*), PARAMETER :: FInt8   = '(I4)'
  CHARACTER(*), PARAMETER :: FInt8ZP = '(I4.3)'
  CHARACTER(*), PARAMETER :: FInt   = FInt32 !Default
  CHARACTER(*), PARAMETER :: FI4B   = FInt32 !Default
  CHARACTER(*), PARAMETER :: FI4BZP = FInt32ZP
  CHARACTER(*), PARAMETER :: FIntZP = FInt32ZP

  ! Length (number of digits) of formatted numbers
#ifdef Real128
  INTEGER, PARAMETER :: DReal128 = 42
#else
  INTEGER, PARAMETER :: DReal128 = 23
#endif
  INTEGER, PARAMETER :: DReal64  = 23
  INTEGER, PARAMETER :: DReal32  = 13
  INTEGER, PARAMETER :: DReal  = DReal64 !Default
  INTEGER, PARAMETER :: DFloat  = DReal64 !Default
  INTEGER, PARAMETER :: DDFP  = DReal64 !Default
  INTEGER, PARAMETER :: DInt64  = 20
  INTEGER, PARAMETER :: DInt32  = 11
  INTEGER, PARAMETER :: DInt16  = 6
  INTEGER, PARAMETER :: DInt8  = 4
  INTEGER, PARAMETER :: DInt  = DInt32 !Default
  INTEGER, PARAMETER :: DI4B  = DInt32 !Default

  ! List of kinds
  INTEGER, PARAMETER :: REAL_KINDS_LIST(1:4) &
    & = [Real128, Real64, Real32, Float]
  CHARACTER(*), PARAMETER :: REAL_FORMATS_LIST(1:4) &
    & = [FReal128, FReal64, FReal32//' ', FReal]
  INTEGER, PARAMETER :: INTEGER_KINDS_LIST(1:5)  &
    & = [Int64, Int32, Int16, Int8, DIP]
  CHARACTER(*), PARAMETER :: INTEGER_FORMATS_LIST(1:5) = &
    & [FInt64, FInt32, FInt16//' ', FInt8//' ', FInt]

  ! Minimum and maximum (representable) values
  REAL(Real128), PARAMETER :: TypeReal128 = 1.0
  REAL(Real128), PARAMETER :: MinReal128 = -huge(1._Real128)
  REAL(Real128), PARAMETER :: MaxReal128 =  huge(1._Real128)

  REAL(Real64), PARAMETER :: TypeReal64 = 1.0
  REAL(Real64), PARAMETER :: MinReal64 = -huge(1._Real64 )
  REAL(Real64), PARAMETER :: MaxReal64 = huge(1._Real64 )

  REAL(Real32), PARAMETER :: TypeReal32 = 1.0
  REAL(Real32), PARAMETER :: MinReal32 = -huge(1._Real32 )
  REAL(Real32), PARAMETER :: MaxReal32 = huge(1._Real32 )

  REAL(Float), PARAMETER :: MinFloat  = MinReal64 ! default
  REAL(Float), PARAMETER :: MinReal  = MinReal64 ! default
  REAL(Float), PARAMETER :: MaxFloat  = MaxReal64 ! default
  REAL(Float), PARAMETER :: MaxReal  = MaxReal64 ! default
  REAL(Float), PARAMETER :: MinDFP  = MinReal64 ! default
  REAL(Float), PARAMETER :: MaxDFP  = MaxReal64 ! default
  REAL(Float), PARAMETER :: TypeReal = 1.0, TypeDFP = 1.0, TypeFloat = 1.0

  INTEGER(Int64), PARAMETER :: MinInt64  = -huge(1_Int64), TypeInt64 = 1
  INTEGER(Int32), PARAMETER :: MinInt32  = -huge(1_Int32), TypeInt32 = 1
  INTEGER(Int16), PARAMETER :: MinInt16  = -huge(1_Int16), TypeInt16 = 1
  INTEGER(Int8), PARAMETER :: MinInt8  = -huge(1_Int8), TypeInt8 = 1
  INTEGER(DIP), PARAMETER :: MinInt  = MinInt32, TypeInt = 1 ! default
  INTEGER(I4B), PARAMETER :: MinI4B  = MinInt32, TypeIntI4B = 1 ! default
  INTEGER(Int64), PARAMETER :: MaxInt64  =  huge(1_Int64)
  INTEGER(Int32), PARAMETER :: MaxInt32  =  huge(1_Int32)
  INTEGER(Int16), PARAMETER :: MaxInt16  =  huge(1_Int16)
  INTEGER(Int8), PARAMETER :: MaxInt8  =  huge(1_Int8)
  INTEGER(DIP), PARAMETER :: MaxI4B  = MaxInt32 !default
  INTEGER(DIP), PARAMETER :: MaxInt  = MaxInt32 !default

  ! Real smallest (representable) values
  REAL(Real128), PARAMETER :: smallReal128 = tiny(1._Real128)
  REAL(Real64),  PARAMETER :: smallReal64  = tiny(1._Real64 )
  REAL(Real32),  PARAMETER :: smallReal32  = tiny(1._Real32 )
  REAL(Float),  PARAMETER :: smallFloat  = smallReal64 !default
  REAL(Float),  PARAMETER :: smallReal  = smallReal64 !default
  REAL(Float),  PARAMETER :: smallDFP  = smallReal64 !default

! Smallest REAL representable difference by the running calculator
  REAL(Real128), PARAMETER :: ZeroReal128 = nearest(1._Real128, 1._Real128) - &
    & nearest(1._Real128,-1._Real128)
  REAL(Real64),  PARAMETER :: ZeroReal64  = nearest(1._Real64, 1._Real64) - &
    & nearest(1._Real64,-1._Real64)
  REAL(Real32),  PARAMETER :: ZeroReal32  = nearest(1._Real32, 1._Real32) - &
    & nearest(1._Real32,-1._Real32)
  REAL(Float),  PARAMETER :: Zero = ZeroReal64

  ! Bits/bytes memory requirements (REAL variables (R?P) must be computed at runtime)
  INTEGER(Int16)            :: BIReal128
  INTEGER(Int8)            :: BIReal64
  INTEGER(Int8)            :: BIReal32
  INTEGER(Int8)            :: BIFloat !default in bits
  INTEGER(Int8)            :: BIReal !default in bits
  INTEGER(Int8)            :: BIDFP !default in bytes
  INTEGER(Int16)            :: BYReal128
  INTEGER(Int8)            :: BYReal64
  INTEGER(Int8)            :: BYReal32
  INTEGER(Int8)            :: BYFloat !default
  INTEGER(Int8)            :: BYReal !default
  INTEGER(Int8)            :: BYDFP !default
  INTEGER(Int64), PARAMETER :: BIInt64 = bit_size(MaxInt64)
  INTEGER(Int32), PARAMETER :: BIInt32 = bit_size(MaxInt32)
  INTEGER(Int16), PARAMETER :: BIInt16 = bit_size(MaxInt16)
  INTEGER(Int8), PARAMETER :: BIInt8 = bit_size(MaxInt8)
  INTEGER(DIP), PARAMETER :: BIInt = bit_size(MaxInt) !default in bits
  INTEGER(DIP), PARAMETER :: BII4B = bit_size(MaxInt) !default in bits
  INTEGER(Int64), PARAMETER :: BYInt64 = bit_size(MaxInt64)/8_Int64
  INTEGER(Int32), PARAMETER :: BYInt32 = bit_size(MaxInt32)/8_Int32
  INTEGER(Int16), PARAMETER :: BYInt16 = bit_size(MaxInt16)/8_Int16
  INTEGER(Int8), PARAMETER :: BYInt8 = bit_size(MaxInt8)/8_Int8
  INTEGER(DIP), PARAMETER :: BYInt = bit_size(MaxInt)/8_DIP !default in bytes
  INTEGER(DIP), PARAMETER :: BYI4B = bit_size(MaxInt)/8_DIP ! default in bytes

  REAL( DFP ), PARAMETER :: Pi = 3.14159265359_DFP

  REAL( DFP ), DIMENSION( 3, 3 ) :: Eye3 = RESHAPE(&
  (/1.0_DFP, 0.0_DFP, 0.0_DFP,&
  0.0_DFP, 1.0_DFP, 0.0_DFP,&
  0.0_DFP, 0.0_DFP, 1.0_DFP/),(/3,3/))
  REAL( DFP ), DIMENSION( 2, 2 ) :: Eye2 = RESHAPE(&
  (/1.0_DFP, 0.0_DFP,0.0_DFP, 1.0_DFP/),(/2,2/))

  ! Parameters for iteration data
  INTEGER( I4B ), PARAMETER :: RelativeConvergence = 1, ConvergenceInRes = 1
  INTEGER( I4B ), PARAMETER :: AbsoluteConvergence = 2, ConvergenceInSol = 2
  INTEGER( I4B ), PARAMETER :: NormL1=1, NormL2=2, NormInfinity=3

  ! Precondition sides
  INTEGER( I4B ), PARAMETER :: NoPrecond = 0, PrecondLeft = 1, PrecondRight = 2
  ! Type of shape functions
  INTEGER( I4B ), PARAMETER   ::  LagrangePolynomial = 1
  INTEGER( I4B ), PARAMETER   ::  SerendipityPolynomial = 2
  INTEGER( I4B ), PARAMETER   ::  HeirarchicalPolynomial = 3

  INTEGER( I4B ), PARAMETER   ::  Equidistance = 1
  INTEGER( I4B ), PARAMETER   ::  GaussLegendre = 2
  INTEGER( I4B ), PARAMETER   ::  GaussLobatto = 3
  INTEGER( I4B ), PARAMETER   ::  Chebyshev = 4

  ! Type of Lagrange Interpolation Poitns
  INTEGER( I4B ), PARAMETER   ::  EquidistanceLIP = Equidistance
  INTEGER( I4B ), PARAMETER   ::  GaussLobattoLIP = GaussLobatto
  INTEGER( I4B ), PARAMETER   ::  GaussLegendreLIP = GaussLegendre
  INTEGER( I4B ), PARAMETER   ::  ChebyshevLIP = Chebyshev

  ! Type of quadrature points
  INTEGER( I4B ), PARAMETER   ::  GaussLegendreQP = GaussLegendre
  INTEGER( I4B ), PARAMETER   ::  GaussLobattoQP = GaussLobatto
  INTEGER( I4B ), PARAMETER   ::  ChebyshevQP = Chebyshev

  ! Types of Elements
  INTEGER( I4B ), PARAMETER   ::  Line=1
  INTEGER( I4B ), PARAMETER   ::  Line2=1
  INTEGER( I4B ), PARAMETER   ::  Line3=8
  INTEGER( I4B ), PARAMETER   ::  Line4=26
  INTEGER( I4B ), PARAMETER   ::  Line5=27
  INTEGER( I4B ), PARAMETER   ::  Line6=28

  INTEGER( I4B ), PARAMETER   ::  Triangle=2
  INTEGER( I4B ), PARAMETER   ::  Triangle3=2
  INTEGER( I4B ), PARAMETER   ::  Triangle6=9
  INTEGER( I4B ), PARAMETER   ::  Triangle9=20
  INTEGER( I4B ), PARAMETER   ::  Triangle10=21
  INTEGER( I4B ), PARAMETER   ::  Triangle12=22
  INTEGER( I4B ), PARAMETER   ::  Triangle15a=23
  INTEGER( I4B ), PARAMETER   ::  Triangle15b=24
  INTEGER( I4B ), PARAMETER   ::  Triangle15=24
  INTEGER( I4B ), PARAMETER   ::  Triangle21=25

  INTEGER( I4B ), PARAMETER   ::  Quadrangle=3
  INTEGER( I4B ), PARAMETER   ::  Quadrangle4=3
  INTEGER( I4B ), PARAMETER   ::  Quadrangle9=10
  INTEGER( I4B ), PARAMETER   ::  Quadrangle8=16
  INTEGER( I4B ), PARAMETER   ::  Quadrangle16=160

  INTEGER( I4B ), PARAMETER   ::  Tetrahedron=4
  INTEGER( I4B ), PARAMETER   ::  Tetrahedron4=4
  INTEGER( I4B ), PARAMETER   ::  Tetrahedron10=11
  INTEGER( I4B ), PARAMETER   ::  Tetrahedron20=29
  INTEGER( I4B ), PARAMETER   ::  Tetrahedron35=30
  INTEGER( I4B ), PARAMETER   ::  Tetrahedron56=31

  INTEGER( I4B ), PARAMETER   ::  Hexahedron=5
  INTEGER( I4B ), PARAMETER   ::  Hexahedron8=5
  INTEGER( I4B ), PARAMETER   ::  Hexahedron27=12
  INTEGER( I4B ), PARAMETER   ::  Hexahedron20=17
  INTEGER( I4B ), PARAMETER   ::  Hexahedron64=92
  INTEGER( I4B ), PARAMETER   ::  Hexahedron125=93

  INTEGER( I4B ), PARAMETER   ::  Prism=6
  INTEGER( I4B ), PARAMETER   ::  Prism6=6
  INTEGER( I4B ), PARAMETER   ::  Prism18=13
  INTEGER( I4B ), PARAMETER   ::  Prism15=18

  INTEGER( I4B ), PARAMETER   ::  Pyramid=7
  INTEGER( I4B ), PARAMETER   ::  Pyramid5=7
  INTEGER( I4B ), PARAMETER   ::  Pyramid14=14
  INTEGER( I4B ), PARAMETER   ::  Pyramid13=19

  INTEGER( I4B ), PARAMETER   ::  Point=15
  INTEGER( I4B ), PARAMETER   ::  Point1=15

  ! Read material data from ?
  INTEGER( I4B ), PARAMETER :: PhysicalTag = 1
  INTEGER( I4B ), PARAMETER :: GeometryTag = 2

  ! Constraint type
  INTEGER( I4B ), PARAMETER ::  StrongBC = 1, &
                              NitscheBC = 2, &
                              LagrangeMultiplierBC = 3, &
                              PenaltyBC = 4, &
                              AugmentedBC = 5

  ! Symmetric and Skewsymmertic Nitsche Formulation
  INTEGER( I4B ), PARAMETER :: SkewSymNitsch = 1, SymNitsche = 2

  ! ScalarDOF
  INTEGER( I4B ), PARAMETER :: ScalarDOF = -1

  !! Following are used in ErrorHandling.f90
  INTEGER( I4B ), PARAMETER, PUBLIC :: OPT_OPEN = 1
  !! Constant for file open used by fErr
  INTEGER( I4B ), PARAMETER, PUBLIC :: OPT_READ = 2
  !! Constant for file read used by fErr
  INTEGER( I4B ), PARAMETER, PUBLIC :: OPT_WRITE = 3
  !! Constant for file write used by fErr
  INTEGER( I4B ), PARAMETER, PUBLIC :: OPT_CLOSE = 4
  !! Constant for file close used by fErr
  INTEGER( I4B ), PARAMETER, PUBLIC :: OPT_ALLOC = 1
  INTEGER( I4B ), PARAMETER, PUBLIC :: OPT_DEALLOC = 2


  ! for matrix conversion ( dense to dense )
! element matrix storage may differ from global matrix storage format
INTEGER( I4B ), PARAMETER, PUBLIC :: DofToNodes = 0
  !! It is used in [[RealVector_]] and [[RealMatrix_]]
INTEGER( I4B ), PARAMETER, PUBLIC :: NONE = -1
  !! It is used in [[RealVector_]] and [[RealMatrix_]]
INTEGER( I4B ), PARAMETER, PUBLIC :: NodesToDOF = 1
  !! It is used in [[RealVector_]] and [[RealMatrix_]]
INTEGER( I4B ), PARAMETER, PUBLIC :: DOF_FMT = 0
INTEGER( I4B ), PARAMETER, PUBLIC :: NODES_FMT = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: FMT_DOF = 0
INTEGER( I4B ), PARAMETER, PUBLIC :: FMT_NODES = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: Matrix_ROW = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: Matrix_COLUMN = 2
INTEGER( I4B ), PARAMETER, PUBLIC :: Matrix_DIAGONAL = 0

INTEGER( I4B ), PARAMETER, PUBLIC :: SMALL_MATRIX_ROW = 10
  !! Lenght of small matrix in row dimension
INTEGER( I4B ), PARAMETER, PUBLIC :: SMALL_MATRIX_LEN_COL = 10
  !! Length of small matrix in column dimension
INTEGER( I4B ), PARAMETER, PUBLIC :: SMALL_VECTOR_LEN = 100
  !! Length of small vector

INTEGER( I4B ), PARAMETER, PUBLIC :: OMP_THREADS_FORKED=1
INTEGER( I4B ), PARAMETER, PUBLIC :: OMP_THREADS_JOINED=2

! Related to tensors
INTEGER( I4B ), PARAMETER, PUBLIC :: SymTensor = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: SkewSymTensor = -1
INTEGER( I4B ), PARAMETER, PUBLIC :: GeneralTensor = 0
INTEGER( I4B ), PARAMETER, PUBLIC :: StressTypeVoigt = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: StrainTypeVoigt = -1
INTEGER( I4B ), PARAMETER, PUBLIC :: WithSpectral = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: WithoutSpectral = -1
INTEGER( I4B ), PARAMETER, PUBLIC :: SineLode = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: CosineLode = 0

END MODULE GlobalData
