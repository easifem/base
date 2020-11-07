file=ex
fc=ifort
fopenmp="-qopenmp"
fopt="-fpp"
fflag="-nofor_main"
Mylib="/home/vikas/soafem_pkgs/lis/lib/liblis.a"
MyInclude=-I/home/vikas/soafem_pkgs/lis/include

#comile the file
$fc -c $fopenmp $fopt $file.f90  $MyInclude

#link the file
$fc $fopt $fopenmp  $fflag $file.o $Mylib


