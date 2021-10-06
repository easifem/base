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

#define _I_OUT_ INTEGER( C_INT ), INTENT( OUT )
#define _I_IN_ INTEGER( C_INT ), INTENT( IN )
#define _I_V_IN_ INTEGER( C_INT ), VALUE, INTENT( IN )
#define _ST_V_IN_ INTEGER( C_SIZE_T ), VALUE, INTENT( IN )
#define _ST_OUT_ INTEGER( C_SIZE_T ), INTENT( OUT )
#define _ST_IN_ INTEGER( C_SIZE_T ), INTENT( IN )
#define _R_V_IN_ REAL( C_DOUBLE ), VALUE, INTENT( IN )
#define _R_IN_ REAL( C_DOUBLE ), INTENT( IN )
#define _R_OUT_ REAL( C_DOUBLE ), INTENT( OUT )
#define _CPTR_V_IN_ TYPE(C_PTR), VALUE, INTENT( IN )
#define _CPTR_IN_ TYPE(C_PTR), INTENT( IN )


INTEGER( I4B ), PARAMETER, PUBLIC :: MAX_METIS_OPTIONS = 40
  !! Maximum number of METSI OPTIONS

! Return codes
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OK = 1
  !! Flag to indicate that there is no error
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_ERROR_INPUT = -2
  !! Flag to indicate that there is erro during input output
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_ERROR_MEMORY = -3
  !! Flag to indicate error due to the insufficient memory.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_ERROR = -4
  !! Flag to indicate undocumented error

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_PTYPE = 0
  !! It specifies the type of partitioning method; The possible values are
  !! given below.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_PTYPE_RB   = 0
  !! Multilevel recursive bisectioning.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_PTYPE_KWAY = 1
  !! Multilevel k-way partitioning. DEFAULT
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_PTYPE_DEFAULT = METIS_PTYPE_KWAY
  !! Default value of METIS_OPTION_PTYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_OBJTYPE = 1
  !! Specifies the objective type, two values are possible: Edge-cut, which
  !! minimizes the communication time, and Total communication volume
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OBJTYPE_CUT  = 0
  !! Edge-cut minimization.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OBJTYPE_VOL  = 1
  !! Total communication volume minimization.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OBJTYPE_NODE = 2
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OBJTYPE_DEFAULT = METIS_OBJTYPE_CUT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_CTYPE = 2
  !! Specifies the matching scheme to be used during coarsening. Possible
  !! values are as follows:
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_CTYPE_RM   = 0
  !! Random matching.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_CTYPE_SHEM = 1
  !! Sorted heavy-edge matching.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_CTYPE_DEFAULT=METIS_CTYPE_RM

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_IPTYPE = 3
  !! Specify the algorithm used during the initial partitioning of the mesh
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_IPTYPE_GROW    = 0
  !! grows bisection using a greedy strategy.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_IPTYPE_RANDOM  = 1
  !! computes bisection at random followed by a refinement.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_IPTYPE_EDGE    = 2
  !! derives separator form an edge cut.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_IPTYPE_NODE    = 3
  !! grows bisection using a greedy node-based strategy.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_IPTYPE_METISRB = 4
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_IPTYPE_DEFAULT=METIS_IPTYPE_GROW

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_RTYPE = 4
  !! Specify the algorithm used for refinement.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_RTYPE_FM        = 0
  !! FM-basecut refinement.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_RTYPE_GREEDY    = 1
  !!Greedy-based cut and volume refinement.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_RTYPE_SEP2SIDED = 2
  !! Two-sidenode FM refinement.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_RTYPE_SEP1SIDED = 3
  !! One-sidenode FM refinement.

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_DBGLVL = 5
  !! Specifies the amount of progress/debugging information will be printed during the execution of the algorithms. The default value is 0 (no debugging/progress information). A non-zero value can be supplied that is obtained by a bit-wise OR of the following values
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_DBG_INFO = 1
  !! Shows various diagnostic messages.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_DBG_TIME = 2
  !! Perform timing analysis.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_DBG_COARSEN = 4
  !! Shothe coarsening progress.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_DBG_REFINE = 8
  !! Shothe refinement progress.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_DBG_IPART = 16
  !! Shoinfo on initial partitioning.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_DBG_MOVEINFO = 32
  !! Shoinfo on vertex moves during refinement.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_DBG_SEPINFO = 64
  !! Shoinfo on vertex moves during sep refinement.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_DBG_CONNINFO = 128
  !! Shoinfo on minimization of subdomain connectivity.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_DBG_CONTIGINFO = 256
  !! Shoinfo on elimination of connected components.
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_DBG_MEMORY = 2048
  !! Shoinfo related to wspace allocation.

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_NITER = 6
  !! Specifies the number of iterations for the refinement algorithm.
  !! Default value is 10
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_NITER_DEFAULT = 10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_NCUTS = 7
  !! Specifies the number of different partitionings that it will compute. The final partitioning is the one that achieves the best edgecut or communication volume. Default is 1
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_NCUTS_DEFAULT=1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_SEED = 8
  !! Specifies the seed for the random number generator.

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_NO2HOP = 9
  !! Specifies that the coarsening will not perform any 2–hop matchings when the standard matching approach fails to sufficiently coarsen the graph. The 2–hop matching is very effective for graphs with power-law degree distributions.
  !! - 0 means perform 2-hop matching
  !! - 1 means do not perform 2-hop matching

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_MINCONN   = 10
  !! Specifies that the partitioning routines should try to minimize the maximum degree of the subdomain graph, i.e., the graph in which each partition is a node, and edges connect subdomains with a shared interface.
  !! 0 Does not explicitly minimize the maximum connectivity
  !! 1 explicitly minimize the maximum connectivity
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_MINCONN_DEFAULT = 0

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_CONTIG = 11
  !! Specifies that the partitioning routines should try to produce partitionsthat are contigous. If the input graph is not connected then this option is ignored
  !! - 0 does not force contiguous partition
  !! - 1 does force contingous partition
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_CONTIG_DEFAULT = 0

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_COMPRESS = 12
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_COMPRESS_DEFAULT = 0
  !! Specifies that the graph should be compressed by combining togethervertices that have identical adjacency lists.
  !! O- does not compress
  !! 1 compress

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_CCORDER = 13
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_CCORDER_DEFAULT = 0
  !! Specifies if the connected components of the graph should first be identifies and ordered separately.

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_PFACTOR = 14
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_PFACTOR_DEFAULT = 0
  !! Specifies the minimum degree of the vertices that will be ordered last.
  !! If the specified value is x > 0, then
  !! any vertices with a degree greater than 0.1*x*(average degree) are removed from the graph, an ordering of
  !! the rest of the vertices is computed, and an overall ordering is computed by ordering the removed vertices
  !! at the end of the overall ordering. For example if x = 40, and the average degree is 5, then the algorithm
  !! will remove all vertices with degree greater than 20. The vertices that are removed are ordered last (i.e.,
  !! they are automatically placed in the top-level separator). Good values are often in the range of 60 to 200
  !! (i.e., 6 to 20 times more than the average). Default value is 0, indicating that no vertices are removed.
  !! Used to control whether or not the ordering algorithm should remove any vertices with high degree (i.e.,
  !! dense columns). This is particularly helpful for certain classes of LP matrices, in which there a few vertices
  !! that are connected to many other vertices. By removing these vertices prior to ordering, the quality and the
  !! amount of time required to do the ordering improves

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_NSEPS  = 15
  !! Specifies the number of different separators that it will compute at each level of nested dissection. The final separator that is used is the smallest one. Default is 1
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_NSEPS_DEFAULT = 1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_UFACTOR = 16
  !! Specifies the maximum allowed load imbalance among the partitions. A
  !! value of x indicates that the allowed load imbalance is (1 + x)=1000.
  !! The load imbalance for the jth constraint is defined to be maxi(w[j; i])
  !! =t[j; i]), where w[j; i] is the fraction of the overall weight of the
  !! jth constraint that is assigned to the ith partition and t[j; i] is the
  !! desired target weight of the jth constraint for the ith partition (i.e.,
  !! that specified via -tpwgts). For -ptype=rb, the default value is 1 (i.e.
  !! , load imbalance of 1.001) and for -ptype=kway, the default value is 30
  !! (i.e., load imbalance of 1.03).

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_OPTION_NUMBERING = 17
  !! Used to indicate which numbering scheme is used for the adjacency structure of a graph or the element-node structure of a mesh.
  !! The possible values are 0 for C-style, and 1 for Fortran style
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_NUMBERING_DEFAULT = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_NUMBERING_FORTRAN = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: METIS_NUMBERING_C = 0

!----------------------------------------------------------------------------
!                                                  METIS_SetDefaultOptions
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 July 2021
! summary: Initializes the options array into its default values.
!
!# Introduction
! Initializes the options array into its default values.
!
!@note
! The passed array `options` must have the size `MAX_METIS_OPTIONS` (40).
! To be able to use the option parameters specified in the [[metis_interface]] module
! it is recommended to use zero-based indexing for the options array:
!```Fortran
!INTEGER( I4B ) :: opts(0:39)
!```
!@endnote
!
! Other options can also be changed using parameters specified in the [[metis_interface]] module.

INTERFACE
FUNCTION METIS_SetDefaultOptions(options) RESULT(ans) BIND(C,name="METIS_SetDefaultOptions")
  IMPORT
  INTEGER( I4B ), INTENT(OUT) :: options(MAX_METIS_OPTIONS)
    !! The array of options that will be initialized.
  INTEGER( I4B ) :: ans
    !! `METIS_OK` - Indicates that the function returned normally.
END FUNCTION METIS_SetDefaultOptions
END INTERFACE

PUBLIC :: METIS_SetDefaultOptions

!----------------------------------------------------------------------------
!                                                              METIS_NodeND
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 July 2021
! summary: This function computes fill reducing orderings of sparse matrices using the multilevel nested dissection algorithm.
!
!# Introduction
! This function computes fill reducing orderings of sparse matrices using the
! multilevel nested dissection algorithm.
!
! Let $A$ be the original matrix and $A*$ be the permuted matrix.
! The arrays `perm` and `iperm` are defined as follows. Row (column) `i` of $A*$ is the `perm(i)` row (column) of $A$, and row (column) `i` of $A$ is the `iperm(i)` row (column) of $A*$.
! The numbering of this vector starts from either 0 or 1, depending on the value of `options(METIS_OPTION_NUMBERING)`.
!
! If the graph is weighted, meaning `vgwt` was provided, the nested dissection ordering computes vertex separators that minimize the sum of the weights of the vertices on the separators.
!
! THE FOLLOWING OPTIONS ARE VALID:
!
! - `METIS_OPTION_CTYPE`
! - `METIS_OPTION_RTYPE`
! - `METIS_OPTION_NO2HOP`
! - `METIS_OPTION_NSEPS`
! - `METIS_OPTION_NITER`
! - `METIS_OPTION_UFACTOR`
! - `METIS_OPTION_COMPRESS`
! - `METIS_OPTION_CCORDER`
! - `METIS_OPTION_SEED`
! - `METIS_OPTION_PFACTOR`
! - `METIS_OPTION_NUMBERING`
! - `METIS_OPTION_DBGLVL`
!
! CInterface
! int METIS_NodeND(idx_t *nvtxs, idx_t *xadj, idx_t *adjncy, idx_t *vwgt, idx_t *options, idx_t *perm, idx_t *iperm)
!
! Optional argument : vwgt, options

INTERFACE
FUNCTION METIS_NodeND( nvtxs, xadj, adjncy, vwgt, options, perm, iperm ) &
  & RESULT( Ans ) BIND( C, NAME="METIS_NodeND" )
  IMPORT
  INTEGER( I4B ), INTENT( IN ) :: nvtxs
    !! The number of vertices in the graph.
  INTEGER( I4B ), INTENT(IN) :: xadj(*), adjncy(*)
    !! The adjacency structure of the graph as described in Section 5.5 of
    !! the [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
  TYPE(C_PTR), VALUE, INTENT( IN ) :: vwgt
    !! An array of size `nvtxs` specifying the weights of the vertices.
  TYPE(C_PTR), VALUE,  INTENT(IN) :: options !( MAX_METIS_OPTIONS )
    !! This is the array of options as described in Section 5.4 of the [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf). See description for valid options.
  INTEGER( I4B ), INTENT( OUT ) :: perm(nvtxs), iperm(nvtxs)
    !! These are vectors, each of size nvtxs. Upon successful completion,
    !! they store the fill-reducing permutation and inverse-permutation. Let
    !! A be the original matrix and $A*$ be the permuted matrix.
    !! The arrays `perm` and `iperm` are defined as follows.
    !! Row (or, column) $i$ of $A*$ is the `perm[i]` row (column) of A,
    !! and row (column) $i$ of $A$ is the `iperm[i]` row (column) of A0.
    !! The numbering of this vector starts from either 0 or 1,
    !! depending on the value of options[METIS OPTION NUMBERING]
  INTEGER( I4B ) :: ans
    !! `METIS_OK` - Indicates that the function returned normally.<br />
    !! `METIS_ERROR_INPUT` - Indicates an input error.<br />
    !! `METIS_ERROR_MEMORY` - Indicates that it could not allocate the required memory.<br />
    !! `METIS_ERROR` - Indicates some other type of error.
END FUNCTION METIS_NodeND
END INTERFACE

PUBLIC :: METIS_NodeND

!----------------------------------------------------------------------------
!                                                   METIS_PartGraphRecursive
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 July 2021
! summary: this function is used to partition a graph into `nparts`
! parts using recursive bisection.
!
!# Introduction
! This function is used to partition a graph into `nparts` parts using
! recursive bisection.
!
! THE FOLLOWING OPTIONS ARE VALID:
!
! - `METIS_OPTION_CTYPE`
! - `METIS_OPTION_IPTYPE`
! - `METIS_OPTION_RTYPE`
! - `METIS_OPTION_NO2HOP`
! - `METIS_OPTION_NCUTS`
! - `METIS_OPTION_NITER`
! - `METIS_OPTION_SEED`
! - `METIS_OPTION_UFACTOR`
! - `METIS_OPTION_NUMBERING`
! - `METIS_OPTION_DBGLVL`
!
! OPTIONAL :: VWGT, VSIZE, ADJWGT, TPWGTS, UBVEC, OPTIONS
!
! int_METIS_PartGraphRecursive(idx_t *nvtxs, idx_t *ncon, idx_t *xadj, idx_t ! *adjncy, idx_t *vwgt, idx_t *vsize, idx_t *adjwgt, idx_t *nparts, real_t *tpwgts, real_t *ubvec, idx_t *options, idx_t *objval, idx_t *part)

INTERFACE
FUNCTION METIS_PartGraphRecursive( nvtxs, ncon, xadj, adjncy, &
  & vwgt, vsize, adjwgt, nparts, tpwgts, ubvec, options, objval, part ) &
  & RESULT( ans ) BIND(C, name="METIS_PartGraphRecursive")
  IMPORT
  INTEGER( I4B ), INTENT( IN ) :: nvtxs
    !! The number of vertices in the graph.
  INTEGER( I4B ), INTENT( IN ) :: ncon
    !! The number of balancing constraints on each node.
    !! It should be atleast 1.
  INTEGER( I4B ), INTENT( IN ) :: xadj(*), adjncy(*)
    !! The adjacency structure of the graph as described in
    !! section 5.5 of the
    !! [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
  TYPE(C_PTR), VALUE, INTENT( IN ) :: vwgt
    !! The weights of the vertices as described in Section 5.5 of the
    !! [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
  TYPE(C_PTR), VALUE, INTENT( IN ) :: vsize
    !! The size of the vertices for computing the total communication volume
    !!  as described in section 5.7 of the
    !! [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
  TYPE(C_PTR), VALUE, INTENT( IN ) :: adjwgt
    !! The weights of the edges as describe in Section 5.5 of the
    !! [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
  INTEGER( I4B ), INTENT(IN) :: nparts
    !! The number of parts to partition the graph.
  TYPE(C_PTR), VALUE, INTENT( IN ) :: tpwgts
    !! This is an array pf size`nparts*ncon` that specifies the desired
    !! weight for each partition and constraint.
    !! The target partition weight for the ith partition and
    !! jth constraint is specified at `tpwgts[i*ncon+j]`.
    !! The numbering for both partition and constraints starts from 0.
    !! For each constraint, the sum of the `tpwgts[]` entries must be 1.0.
    !! If not present, the graph is divided equally among the partitions.
    !! More in the description.
  TYPE(C_PTR), VALUE, INTENT( IN ) :: ubvec
    !! An array of size `ncon` that specifies the allowed load imbalance
    !! for each constraint.
    !! For the `i`-th partition and `j`-th constraint the allowed
    !! weight is the `ubvec(j)*tpwgts(i*ncon+j)`
    !! fraction of the `j`-th's constraint total weight.
    !! If not present, the load imbalance
    !! tolerance is 1.001 (for `ncon = 1`) or 1.01 (for `ncon > 1`).
  TYPE(C_PTR), VALUE, INTENT( IN ) :: options
    !! An array of options as described in Section 5.4 of the METIS manual.
    !! See description for valid options.
  INTEGER( I4B ), INTENT(OUT) :: objval
    !! Upon successful completion, this variable stores the edge-cut or the
    !! total communication volume of the partitioning
    !! solution. The value returned depends on the partitioning's objective
    !! function.
  INTEGER( I4B ), INTENT(OUT) :: part(nvtxs)
    !! This is a vector of size `nvtxs` that upon successful completion
    !! stores the partition vector of the graph.
    !! The numbering of this vector starts from either 0 or 1,
    !! depending on the value of `options(METIS_OPTION_NUMBERING)`.
  INTEGER( I4B ) :: ans
    !! Error code
    !! `METIS_OK` - Indicates that the function returned normally.
    !! `METIS_ERROR_INPUT` - Indicates an input error.
    !! `METIS_ERROR_MEMORY` - Indicates that it could not allocate the required memory.
    !! `METIS_ERROR` - Indicates some other type of error.
END FUNCTION METIS_PartGraphRecursive
END INTERFACE

PUBLIC :: METIS_PartGraphRecursive

!----------------------------------------------------------------------------
!                                                       METIS_PartGraphKway
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 July 2021
! summary: This function is used to partition a graph into `nparts` parts using multilevel k-way partitioning.
!
!# Introduction
!
! This function is used to partition a graph into `nparts` parts using multilevel k-way partitioning.
!
! THE FOLLOWING OPTIONS ARE VALID:
! - `METIS_OPTION_OBJTYPE`
! - `METIS_OPTION_CTYPE`
! - `METIS_OPTION_IPTYPE`
! - `METIS_OPTION_RTYPE`
! - `METIS_OPTION_NO2HOP`
! - `METIS_OPTION_NCUTS`
! - `METIS_OPTION_NITER`
! - `METIS_OPTION_UFACTOR`
! - `METIS_OPTION_MINCONN`
! - `METIS_OPTION_CONTIG`
! - `METIS_OPTION_SEED`
! - `METIS_OPTION_NUMBERING`
! - `METIS_OPTION_DBGLVL`
!
! OPTIONAL :: VWGT, VSIZE, ADJWGT, TPWGTS, UBVEC, OPTIONS
!
! int_METIS_PartGraphRecursive(idx_t *nvtxs, idx_t *ncon, idx_t *xadj, idx_t ! *adjncy, idx_t *vwgt, idx_t *vsize, idx_t *adjwgt, idx_t *nparts, real_t *tpwgts, real_t *ubvec, idx_t *options, idx_t *objval, idx_t *part)

INTERFACE
FUNCTION METIS_PartGraphKway( nvtxs, ncon, xadj, adjncy, &
  & vwgt, vsize, adjwgt, nparts, tpwgts, ubvec, options, objval, part ) &
  & RESULT( Ans ) BIND( C, NAME="METIS_PartGraphKway" )
  IMPORT

  INTEGER( I4B ), INTENT(IN) :: nvtxs
    !! The number of vertices in the graph.
  INTEGER( I4B ), INTENT(IN) :: ncon
    !! The number of balancing constraints on each node.
    !! It should be atleast 1.
  INTEGER( I4B ), INTENT(IN) :: xadj(*), adjncy(*)
    !! The adjacency structure of the graph as described in
    !! section 5.5 of the
    !! [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
  TYPE(C_PTR), VALUE, INTENT( IN ) :: vwgt
    !! The weights of the vertices as described in Section 5.5 of the
    !! [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
  TYPE(C_PTR), VALUE, INTENT( IN ) :: vsize
    !! The size of the vertices for computing the total communication volume
    !!  as described in section 5.7 of the
    !! [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
  TYPE(C_PTR), VALUE, INTENT( IN ) :: adjwgt
    !! The weights of the edges as describe in Section 5.5 of the
    !! [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
  INTEGER( I4B ), INTENT(IN) :: nparts
    !! The number of parts to partition the graph.
  TYPE(C_PTR), VALUE, INTENT( IN ) :: tpwgts
    !! This is an array pf size`nparts*ncon` that specifies the desired
    !! weight for each partition and constraint.
    !! The target partition weight for the ith partition and
    !! jth constraint is specified at `tpwgts[i*ncon+j]`.
    !! The numbering for both partition and constraints starts from 0.
    !! For each constraint, the sum of the `tpwgts[]` entries must be 1.0.
    !! If not present, the graph is divided equally among the partitions.
    !! More in the description.
  TYPE(C_PTR), VALUE, INTENT( IN ) :: ubvec
    !! An array of size `ncon` that specifies the allowed load imbalance
    !! for each constraint.
    !! For the `i`-th partition and `j`-th constraint the allowed
    !! weight is the `ubvec(j)*tpwgts(i*ncon+j)`
    !! fraction of the `j`-th's constraint total weight.
    !! If not present, the load imbalance
    !! tolerance is 1.001 (for `ncon = 1`) or 1.01 (for `ncon > 1`).
  TYPE(C_PTR), VALUE, INTENT( IN ) :: options
    !! An array of options as described in Section 5.4 of the METIS manual.
    !! See description for valid options.
  INTEGER( I4B ), INTENT(OUT) :: objval
    !! Upon successful completion, this variable stores the edge-cut or the
    !! total communication volume of the partitioning
    !! solution. The value returned depends on the partitioning's objective
    !! function.
  INTEGER( I4B ), INTENT(OUT) :: part(nvtxs)
    !! This is a vector of size `nvtxs` that upon successful completion
    !! stores the partition vector of the graph.
    !! The numbering of this vector starts from either 0 or 1,
    !! depending on the value of `options(METIS_OPTION_NUMBERING)`.
  INTEGER( I4B ) :: ans
    !! Error code
    !! `METIS_OK` - Indicates that the function returned normally.
    !! `METIS_ERROR_INPUT` - Indicates an input error.
    !! `METIS_ERROR_MEMORY` - Indicates that it could not allocate the required memory.
    !! `METIS_ERROR` - Indicates some other type of error.
END FUNCTION METIS_PartGraphKway
END INTERFACE

PUBLIC :: METIS_PartGraphKway

!----------------------------------------------------------------------------
!                                                       METIS_PartMeshDual
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 July 2021
! summary: This function is used to partition a mesh into `nparts` parts based on a partitioning of the mesh's dual graph.
!
!# Introduction
! This function is used to partition a mesh into `nparts` parts based on a partitioning of the mesh's dual graph.
!
! This program first converts the mesh into either a dual graph (i.e., each element becomes a graph vertex) or a nodal graph (i.e., each node becomes a graph vertex), and then uses the graph partitioning API routines to partition this graph. METIS utilizes a flexible approach for creating a graph for a finite element mesh, which allows it to handle meshes with different and possibly mixed element types (e.g., triangles, tetrahedra, hexahedra, etc.). The functionality provided by mpmetis is achieved by the METIS PartMeshNodal and METIS PartMeshDual API routines
!
! THE FOLLOWING OPTIONS ARE VALID
!
! - `METIS_OPTION_PTYPE`
! - `METIS_OPTION_OBJTYPE`
! - `METIS_OPTION_CTYPE`,
! - `METIS_OPTION_IPTYPE`
! - `METIS_OPTION_RTYPE`
! - `METIS_OPTION_NCUTS`,
! - `METIS_OPTION_NITER`
! - `METIS_OPTION_SEED`
! - `METIS_OPTION_UFACTOR`,
! - `METIS_OPTION_NUMBERING`
! - `METIS_OPTION_DBGLVL`
!
! OPTIONAL : VWGT, VSIZE, TPWGTS, OPTIONS
!
! MESH DATA STRUCTURE
!
! All of the mesh partitioning and mesh conversion routines in METIS take as input the element node array of a mesh. This element node array is stored using a pair of arrays called eptr and eind, which are similar to the xadj and adjncy arrays used for storing the adjacency structure of a graph. The size of the eptr array is n+ 1, where n is the number of elements in the mesh. The size of the eind array is of size equal to the sum of the number of nodes in all the elements of the mesh. The list of nodes belonging to the ith element of the mesh are stored in consecutive locations of eind starting at position eptr[i] up to (but not including) position eptr[i+1]. This format makes it easy to specify meshes of any type of elements, including meshes with mixed element types that have different number of nodes per element. As it was the case with the format of the mesh file described in Section 4.1.2, the ordering of the nodes in each element is not important

INTERFACE
FUNCTION METIS_PartMeshDual ( ne,nn,eptr,eind,vwgt,vsize,ncommon, &
  & nparts,tpwgts,options,objval,epart,npart ) &
  & RESULT( Ans ) BIND(C, NAME="METIS_PartMeshDual")
  IMPORT

  INTEGER( I4B ), INTENT(IN) :: ne
    !! The number of elements in the mesh.
  INTEGER( I4B ), INTENT(IN) :: nn
    !! The number of nodes in the mesh.
  INTEGER( I4B ), INTENT(IN) :: eptr( * ), eind( * )
    !! The pair of arrays storing the mesh as described in Section 5.6 of the [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
  TYPE( C_PTR ), VALUE, INTENT(IN) :: vwgt
    !! An array of size `ne` specifying the weights of the elements. If not present, all elements have an equal weight.
  TYPE( C_PTR ), VALUE, INTENT(IN) :: vsize
    !! An array of size `ne` specifying the size of the elements that is used
    !! for computing the total comunication volume as described in Section 5.7 of the  [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
    !! If not present, the objective is cut or all elements have an equal size.
  INTEGER( I4B ), INTENT(IN) :: ncommon
    !! Specifies the number of common nodes that two elements must have in order to put an edge between them in the dual graph. Given two elements e1 and e2, containing n1 and n2 nodes, respectively, then an edge will connect the vertices in the dual graph corresponding to e1 and e2 if the number of common nodes between them is greater than or equal to min(ncommon; n1 − 1; n2 − 1). The default value is 1, indicating that two elements will be connected via an edge as long as they share one node. However, this will tend to create too many edges (increasing the memory and time requirements of the partitioning). The user should select higher values that are better suited for the element types of the mesh that wants to partition. For example, for tetrahedron meshes, ncommon should be 3, which creates an edge between two tets when they share a triangular face (i.e., 3 nodes)
  INTEGER( I4B ), INTENT(IN) :: nparts
    !! The number of parts to partition the mesh.
  TYPE( C_PTR ), VALUE, INTENT(IN) :: tpwgts
    !! An array of size `nparts` that specifies the desired weight for each partition. The *target partition weight* for the `i`-th partition is specified at `tpwgts(i)` (the numbering for the
    !! partitions starts from 0). The sum of the `tpwgts` entries must be 1.0.
    !! If not present, the graph is divided equally among the partitions.
  TYPE( C_PTR ), VALUE, INTENT(IN) :: options
    !! An array of options as described in Section 5.4 of the [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf). See description for valid options.
  INTEGER( I4B ), INTENT(OUT) :: objval
    !! Upon successful completion, this variable stores either the edgecut or the total communication
    !! volume of the dual graph's partitioning.
  INTEGER( I4B ), INTENT(OUT) :: epart(ne)
    !! A vector of size `ne` that upon successful completion stores the partition vector for the elements
    !! of the mesh. The numbering of this vector starts from either 0 or 1, depending on the value of
    !! `options(METIS_OPTION_NUMBERING)`.
  INTEGER( I4B ), INTENT(OUT) :: npart(nn)
    !! A vector of size `nn` that upon successful completion stores the partition vector for the nodes
    !! of the mesh. The numbering of this vector starts from either 0 or 1, depending on the value of
    !! `options(METIS_OPTION_NUMBERING)`.
  INTEGER( I4B ) :: ans
    !! `METIS_OK` - Indicates that the function returned normally.<br />
    !! `METIS_ERROR_INPUT` - Indicates an input error.<br />
    !! `METIS_ERROR_MEMORY` - Indicates that it could not allocate the required memory.<br />
    !! `METIS_ERROR` - Indicates some other type of error.
END FUNCTION METIS_PartMeshDual
END INTERFACE

PUBLIC :: METIS_PartMeshDual

!----------------------------------------------------------------------------
!                                                        METIS_PartMeshNodal
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 July 2021
! summary: This function us used to partition a mesh into `nparts` parts based on a partitioning of the mesh's nodal graph.
!
!# Introduction
! This function us used to partition a mesh into `nparts` parts based on a
!  partitioning of the mesh's nodal graph.
!
! THE FOLLOWING OPTIONS ARE VALID:
! - `METIS_OPTION_PTYPE`
! - `METIS_OPTION_OBJTYPE`
! - `METIS_OPTION_CTYPE`
! - `METIS_OPTION_IPTYPE`
! - `METIS_OPTION_RTYPE`
! - `METIS_OPTION_NCUTS`,
! - `METIS_OPTION_NITER`
! - `METIS_OPTION_SEED`
! - `METIS_OPTION_UFACTOR`,
! - `METIS_OPTION_NUMBERING`
! - `METIS_OPTION_DBGLVL`
!
!
! MESH DATA STRUCTURE
!
! All of the mesh partitioning and mesh conversion routines in METIS take as input the element node array of a mesh. This element node array is stored using a pair of arrays called eptr and eind, which are similar to the xadj and adjncy arrays used for storing the adjacency structure of a graph. The size of the eptr array is n+ 1, where n is the number of elements in the mesh. The size of the eind array is of size equal to the sum of the number of nodes in all the elements of the mesh. The list of nodes belonging to the ith element of the mesh are stored in consecutive locations of eind starting at position eptr[i] up to (but not including) position eptr[i+1]. This format makes it easy to specify meshes of any type of elements, including meshes with mixed element types that have different number of nodes per element. As it was the case with the format of the mesh file described in Section 4.1.2, the ordering of the nodes in each element is not important

INTERFACE
FUNCTION METIS_PartMeshNodal( ne, nn, eptr, eind, vwgt, vsize,  &
  & nparts, tpwgts, options, objval, epart, npart ) &
  & RESULT( Ans ) BIND( C, NAME="METIS_PartMeshNodal" )
  IMPORT

  INTEGER( I4B ), INTENT(IN) :: ne
    !! The number of elements in the mesh.
  INTEGER( I4B ), INTENT(IN) :: nn
    !! The number of nodes in the mesh.
  INTEGER( I4B ), INTENT(IN) :: eptr( * ), eind( * )
    !! The pair of arrays storing the mesh as described in Section 5.6 of the [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
  TYPE( C_PTR ), VALUE, INTENT( IN ) :: vwgt
    !! An array of size `nn` specifying weights of the nodes. If not passed, all nodes have an equal weight.
  TYPE( C_PTR ), VALUE, INTENT( IN ) :: vsize
    !! An array of size `nn` specifying the size of the nodes that is used for computing the
    !! total comunication volume as described in Section 5.7 of the [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf). If not passed,
    !! the objective is cut or all nodes have an equal size.
  INTEGER( I4B ), INTENT(IN) :: nparts
    !! The number of parts to partition the mesh.
  TYPE( C_PTR ), VALUE, INTENT( IN ) :: tpwgts
    !! An array of size `nparts` that specifies the desired weight for each partition. The *target
    !! partition weight* for the `i`-th partition is specified at `tpwgts(i)` (the numbering for the
    !! partitions starts from 0). The sum of the `tpwgts` entries must be 1.0. If not passed, the graph
    !! is divided equally among the partitions.
  TYPE( C_PTR ), VALUE, INTENT( IN ) :: options
    !! An array of options as described in Section 5.4 of the [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf). See description for valid options.
  INTEGER( I4B ), INTENT(OUT) :: objval
    !! Upon successful completion, this variable stores either the edgecut or the total communication
    !! volume of the nodal graph's partitioning.
  INTEGER( I4B ), INTENT(OUT) :: epart(ne)
    !! A vector of size `ne` that upon successful completion stores the partition vector for the elements
    !! of the mesh. The numbering of this vector starts from either 0 or 1, depending on the value of
    !! `options(METIS_OPTION_NUMBERING)`.
  INTEGER( I4B ), INTENT(OUT) :: npart(nn)
    !! A vector of size `nn` that upon successful completion stores the partition vector for the nodes
    !! of the mesh. The numbering of this vector starts from either 0 or 1, depending on the value of
    !! `options(METIS_OPTION_NUMBERING)`.
  INTEGER( I4B ) :: ans
    !! `METIS_OK` - Indicates that the function returned normally.
    !! `METIS_ERROR_INPUT` - Indicates an input error.
    !! `METIS_ERROR_MEMORY` - Indicates that it could not allocate the required memory.
    !! `METIS_ERROR` - Indicates some other type of error.
END FUNCTION METIS_PartMeshNodal
END INTERFACE

PUBLIC :: METIS_PartMeshNodal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 July 2021
! summary: 	This function is used to generate the dual graph of a mesh.
!
!# Introduction
! This function is used to generate the dual graph of a mesh.
!
!@note
! To use the returned arrays `xadj` and `adjncy`, these must be first converted from
! a C pointer to a Fortran pointer using the subroutine `c_f_pointer(cptr,fptr,shape)`
! that assigns the target of the C pointer `cptr` to the Fortran pointer `fptr` and
! specifies its shape. The `shape` is an integer rank-one array, storing the size `ne+1`
! in case of the dual graph. The size of the new `adjncy` array is stored in the
! last element of `xadj` when using C-style numbering. An example is shown below.
!@endnote
!
!@warning
! Memory for the returned arrays `xadj` and `adjncy` is allocated by METIS' API in C
! using the standard `malloc` function. It is the responsibility of the application to free
! this memory by calling `free`. Therefore, METIS provides the [[METIS_Free]] function that is a wrapper to
! C's `free`function.
!@endwarning
!
! int METIS MeshToDual(idx t *ne, idx t *nn, idx t *eptr, idx t *eind, idx t *ncommon, idx t *numflag, idx t **xadj, idx t **adjncy)
!


INTERFACE
FUNCTION METIS_MeshToDual( ne, nn, eptr, eind, ncommon, numflag, xadj, &
  & adjncy) RESULT(Ans) BIND(C,NAME="METIS_MeshToDual")
  IMPORT

  INTEGER( I4B ), INTENT(IN) :: ne
    !! The number of elements in the mesh.
  INTEGER( I4B ), INTENT(IN) :: nn
    !! The number of nodes in the mesh.
  INTEGER( I4B ), INTENT(IN) :: eptr( * ), eind( * )
    !! The pair of arrays storing the mesh as described in
    !! Section 5.6 of the [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
  INTEGER( I4B ), INTENT(IN) :: ncommon
    !! Specifies the number of common nodes that two elements must have in order to put an edge between them in the dual graph. Given two elements e1 and e2, containing n1 and n2 nodes, respectively, then an edge will connect the vertices in the dual graph corresponding to e1 and e2 if the number of common nodes between them is greater than or equal to min(ncommon; n1 − 1; n2 − 1). The default value is 1, indicating that two elements will be connected via an edge as long as they share one node. However, this will tend to create too many edges (increasing the memory and time requirements of the partitioning). The user should select higher values that are better suited for the element types of the mesh that wants to partition. For example, for tetrahedron meshes, ncommon should be 3, which creates an edge between two tets when they share a triangular face (i.e., 3 nodes)
  INTEGER( I4B ), INTENT(IN) :: numflag
    !! Used to indicate which numbering scheme is used for `eptr` and `eind`.
    !! The possible values are: <br />
    !! 0 - C-style numbering is assumed that starts from 0 <br />
    !! 1 - Fortran-style numbering is assumed that starts from 1
  TYPE(C_PTR), INTENT(OUT) :: xadj, adjncy
    !! These arrays store the adjacency structure of the generated dual graph. The format of the adjacency structure is described in Section 5.5 of the [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf). Memory for these arrays is allocated by METIS’ API using the standard malloc function. It is the responsibility of the application to free this memory by calling free. METIS provides the METIS Free that is a wrapper to C’s free function.
  INTEGER( I4B ) :: Ans
    !! `METIS_OK` - Indicates that the function returned normally.<br />
    !! `METIS_ERROR_INPUT` - Indicates an input error.<br />
    !! `METIS_ERROR_MEMORY` - Indicates that it could not allocate the required memory.<br />
    !! `METIS_ERROR` - Indicates some other type of error.
END FUNCTION METIS_MeshToDual
END INTERFACE

PUBLIC :: METIS_MeshToDual

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 July 2021
! summary: This function is used to generate the nodal graph of a mesh.
!
!# Introduction
! This function is used to generate the nodal graph of a mesh.
!
!@note
! To use the returned arrays `xadj` and `adjncy`, these must be first converted from
! a C pointer to a Fortran pointer using the subroutine `c_f_pointer(cptr,fptr,shape)`
! that assigns the target of the C pointer `cptr` to the Fortran pointer `fptr` and
! specifies its shape. The `shape` is an integer rank-one array, storing the size `nn+1`
! in case of the nodal graph. The size of the new `adjncy` array is stored in the
! last element of `xadj` when using C-style numbering. An example is shown below.
!@endnote
!
!@warning
! Memory for the returned arrays `xadj` and `adjncy` is allocated by METIS' API in C
! using the standard `malloc` function. It is the responsibility of the application to free
! this memory by calling `free`. Therefore, METIS provides the [[METIS_Free]] function that is a wrapper to
! C's `free`function.
!@endwarning

INTERFACE
FUNCTION METIS_MeshToNodal(ne,nn,eptr,eind,numflag,xadj,adjncy) &
  & RESULT(Ans) BIND(C,name="METIS_MeshToNodal")
  IMPORT

  INTEGER( I4B ), INTENT(IN) :: ne
    !! The number of elements in the mesh.
  INTEGER( I4B ), INTENT(IN) :: nn
    !! The number of nodes in the mesh.
  INTEGER( I4B ), INTENT(IN) :: eptr(*), eind(*)
    !! The pair of arrays storing the mesh as described in Section 5.6 of the [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf).
  INTEGER( I4B ), INTENT(IN) :: numflag
    !! Used to indicate which numbering scheme is used for `eptr` and `eind`.
    !! The possible values are:
    !! 0 - C-style numbering is assumed that starts from 0
    !! 1 - Fortran-style numbering is assumed that starts from 1
  TYPE( C_PTR ), INTENT(OUT) :: xadj, adjncy
    !! These arrays store the adjacency structure of the generated dual graph. The format of the adjacency structure is described in Section 5.5 of the [manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf). Memory for these arrays is allocated by METIS’ API using the standard malloc function. It is the responsibility of the application to free this memory by calling free. METIS provides the METIS Free that is a wrapper to C’s free function.
  INTEGER( I4B ) :: ans
    !! `METIS_OK` - Indicates that the function returned normally.
    !! `METIS_ERROR_INPUT` - Indicates an input error.
    !! `METIS_ERROR_MEMORY` - Indicates that it could not allocate the required memory.
    !! `METIS_ERROR` - Indicates some other type of error.
END FUNCTION METIS_MeshToNodal
END INTERFACE

PUBLIC :: METIS_MeshToNodal

!----------------------------------------------------------------------------
!                                                               METIS_Free
!----------------------------------------------------------------------------

!> Frees the memory that was allocated by either the [[METIS_MeshToDual]] or the
!  [[METIS_MeshToNodal]] routines for returning the dual or nodal graph of a mesh.
!
!@warning Memory deallocation should always happen on the same side it was allocated!
! Also check the descriptions of the above-mentioned routines.
!
!# Example
!
!```Fortran
! type(c_ptr) :: xadj(:),adjncy(:)
!
! call METIS_MeshToNodal(...,xadj,adjncy)
!
! ! xadj and adjncy should be deallocated on the C side! ;)
! call METIS_Free(xadj)
! call METIS_Free(adjncy)
!```

INTERFACE
FUNCTION METIS_Free(ptr) RESULT( ans ) BIND(C,NAME="METIS_Free")
  IMPORT
  TYPE( C_PTR ), VALUE :: ptr
    !! The pointer to be freed. This pointer should be one of the `xadj` or `adjncy`, arrays returned by METIS' API routines
  INTEGER( I4B ) :: ans
  !! `METIS_OK` Indicates that the function returned normally.
END FUNCTION METIS_Free
END INTERFACE

PUBLIC :: METIS_FREE





