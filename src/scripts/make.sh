export INCLUDE_DIR=$include
export LIB_DIR=$lib
export EASIFEM_DIR=$easifem

# First we define variables require for compiling

# fortran compiler
#my_fc="ifort"
my_fc="gfortran"

# fortran compiler options
#my_fc_opt="-W0 -O2"
my_fc_opt="-O2"

# openmp flag
#my_openmp="-qopenmp"
my_openmp=" "

# preprocessing directive option
#my_ppd_opt="-fpp"
my_ppd_opt="-cpp"

# PPD
#my_ppd="-DMULTI_THREADED"
my_ppd=" "

# compiler flag
my_c="-c"

my="${my_fc} ${my_fc_opt}  ${my_c} ${my_openmp} ${my_ppd_opt} ${my_ppd}"

red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

for FILE1 in "$@"
do
    while read F  ; do
        echo ${green}$F ${red}
        $my $EASIFEM_DIR/$F -I$INCLUDE_DIR
        mv *.o $LIB_DIR
        mv *.mod *.smod $INCLUDE_DIR
        echo "${reset}"
    done <${FILE1}
done

cd ${lib}
ar rcs ${lib}/libeasifem.a ${lib}/*.o ${lib}/extpkgs/*.o
cd -
