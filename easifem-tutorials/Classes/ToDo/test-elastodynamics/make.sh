file=$1
ifort $file -I$include $lib/*.o -llapack

