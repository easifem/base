
## obj =

## Quadrature Point
 | Weights | Points |
 |:-------:|:------:|
 |   2.0000000     |   0.0000000     |


## obj%N( :, : )=

          -0.000000
           0.000000
           1.000000


## obj%dNdXi( :, :, 1 )( :, : )=

         -0.5000000
          0.5000000
          -0.000000


## obj%dNdXt( :, :, 1 )( :, : )=

           0.000000
           0.000000
           0.000000


## obj%Jacobian( :, :, 1 )( :, : )=

           0.000000


## obj%Js( : ) =

           0.000000


## obj%Thickness( : ) =

           0.000000


## obj%Coord( :, : )=

           0.000000

## obj%RefElem
  - Element Type :: Line3
  - XiDimension  ::    1
  - NSD  ::    1
  - Order  ::    2

| XiDimension  | #Entities |
| ------------ | ---------:|
| 0 |   3|
| 1 |   1|

| Node Number  | x    |   y  |   z   |
| ------------ | ---- | ---- |  ---- |
 |    1 |   -1.000000     |    0.000000     |    0.000000     |
 |    2 |    1.000000     |    0.000000     |    0.000000     |
 |    3 |    0.000000     |    0.000000     |    0.000000     |

## obj%Topology( 1 )
  - Element Type :: Point1
  - XiDimension  ::    0
  - Nptrs  ::    1

## obj%Topology( 2 )
  - Element Type :: Point1
  - XiDimension  ::    0
  - Nptrs  ::    2

## obj%Topology( 3 )
  - Element Type :: Point1
  - XiDimension  ::    0
  - Nptrs  ::    3

## obj%Topology( 4 )
  - Element Type :: Line3
  - XiDimension  ::    1
  - Nptrs  ::    1   2   3

