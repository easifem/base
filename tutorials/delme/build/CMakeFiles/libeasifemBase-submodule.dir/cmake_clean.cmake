file(REMOVE_RECURSE
  "liblibeasifemBase-submodule.a"
  "liblibeasifemBase-submodule.pdb"
)

# Per-language clean rules from dependency scanning.
foreach(lang Fortran)
  include(CMakeFiles/libeasifemBase-submodule.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
