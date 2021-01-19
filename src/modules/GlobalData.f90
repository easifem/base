! EASIFEM
! By Vikas Sharma
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
  INTEGER, PARAMETER      :: SP= Real32
  INTEGER, PARAMETER      :: DP= Real64
  INTEGER, PARAMETER      :: DFPC = KIND((1.0D0,1.0D0))   !Default Kind cmplx
  INTEGER, PARAMETER      :: SPC = KIND((1.0,1.0))    !Single Precision cmplx
  INTEGER, PARAMETER      :: DPC = KIND((1.0D0,1.0D0))!Double Precision cmplx
  INTEGER, PARAMETER      :: LGT = KIND(.true.)       !Logical

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

END MODULE GlobalData
