rm *.o *.mod *.smod *.out
file=$1
FC="ifort -fpp"
$FC $file -I$include $lib/*.o -llapack
