# This file is a part of easifem-base
# (c) 2021, Vikas Sharma, Ph.D.
# All right reserved
#
# log
# 26/02/2021 this file was created
#-----------------------------------------------------------------------

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")

LIST(APPEND l95_src
${src_path}/la_sgesv.f90 ${src_path}/la_sgesv1.f90 ${src_path}/la_sgesvx.f90 ${src_path}/la_sgesvx1.f90
${src_path}/la_sgetrf.f90 ${src_path}/la_slagge.f90 ${src_path}/la_slange.f90 ${src_path}/la_slange1.f90
${src_path}/la_sgetri.f90 ${src_path}/la_sgetrs.f90 ${src_path}/la_sgetrs1.f90 ${src_path}/la_sgeequ.f90
${src_path}/la_sgerfs.f90 ${src_path}/la_sgerfs1.f90 ${src_path}/la_ssytrd.f90 ${src_path}/la_ssygst.f90
${src_path}/la_sorgtr.f90 ${src_path}/la_spotrf.f90 ${src_path}/la_sgbtrf.f90 ${src_path}/la_slamch.f90
${src_path}/la_sgbsv.f90 ${src_path}/la_sgbsv1.f90 ${src_path}/la_sgbsvx.f90 ${src_path}/la_sgbsvx1.f90
${src_path}/la_sgtsv.f90 ${src_path}/la_sgtsv1.f90 ${src_path}/la_sgtsvx.f90 ${src_path}/la_sgtsvx1.f90
${src_path}/la_sposv.f90 ${src_path}/la_sposv1.f90 ${src_path}/la_sposvx.f90 ${src_path}/la_sposvx1.f90
${src_path}/la_sppsv.f90 ${src_path}/la_sppsv1.f90 ${src_path}/la_sppsvx.f90 ${src_path}/la_sppsvx1.f90
${src_path}/la_spbsv.f90 ${src_path}/la_spbsv1.f90 ${src_path}/la_spbsvx.f90 ${src_path}/la_spbsvx1.f90
${src_path}/la_sptsv.f90 ${src_path}/la_sptsv1.f90 ${src_path}/la_sptsvx.f90 ${src_path}/la_sptsvx1.f90
${src_path}/la_ssysv.f90 ${src_path}/la_ssysv1.f90 ${src_path}/la_ssysvx.f90 ${src_path}/la_ssysvx1.f90
${src_path}/la_sspsv.f90 ${src_path}/la_sspsv1.f90 ${src_path}/la_sspsvx.f90 ${src_path}/la_sspsvx1.f90
${src_path}/la_sgels.f90 ${src_path}/la_sgels1.f90 ${src_path}/la_sgelsx.f90 ${src_path}/la_sgelsx1.f90
${src_path}/la_sgelss.f90 ${src_path}/la_sgelss1.f90 ${src_path}/la_sgglse.f90 ${src_path}/la_sggglm.f90
${src_path}/la_ssyev.f90 ${src_path}/la_ssyevd.f90 ${src_path}/la_ssyevx.f90 ${src_path}/la_sspev.f90
${src_path}/la_sspevd.f90 ${src_path}/la_sspevx.f90 ${src_path}/la_ssbev.f90 ${src_path}/la_ssbevd.f90
${src_path}/la_ssbevx.f90 ${src_path}/la_sstev.f90 ${src_path}/la_sstevd.f90 ${src_path}/la_sstevx.f90
${src_path}/la_sgees.f90 ${src_path}/la_sgeesx.f90 ${src_path}/la_sgeev.f90 ${src_path}/la_sgeevx.f90
${src_path}/la_sgesvd.f90 ${src_path}/la_ssygv.f90 ${src_path}/la_sspgv.f90 ${src_path}/la_ssbgv.f90
${src_path}/la_sgegs.f90 ${src_path}/la_sgegv.f90 ${src_path}/la_sggsvd.f90 ${src_path}/la_sgelsy.f90
${src_path}/la_sgelsy1.f90 ${src_path}/la_sspgvd.f90 ${src_path}/la_ssbgvd.f90 ${src_path}/la_ssygvd.f90
${src_path}/la_ssbgvx.f90 ${src_path}/la_sspgvx.f90 ${src_path}/la_sggev.f90 ${src_path}/la_sgges.f90
${src_path}/la_sstevr.f90 ${src_path}/la_sgelsd.f90 ${src_path}/la_sgelsd1.f90 ${src_path}/la_ssyevr.f90
${src_path}/la_sggevx.f90 ${src_path}/la_ssygvx.f90 ${src_path}/la_sggesx.f90 ${src_path}/la_sgesdd.f90
)

LIST(APPEND l95_src
${src_path}/la_dgesv.f90 ${src_path}/la_dgesv1.f90 ${src_path}/la_dgesvx.f90 ${src_path}/la_dgesvx1.f90
${src_path}/la_dgetrf.f90 ${src_path}/la_dlagge.f90 ${src_path}/la_dlange.f90 ${src_path}/la_dlange1.f90
${src_path}/la_dgetri.f90 ${src_path}/la_dgetrs.f90 ${src_path}/la_dgetrs1.f90 ${src_path}/la_dgeequ.f90
${src_path}/la_dgerfs.f90 ${src_path}/la_dgerfs1.f90 ${src_path}/la_dsytrd.f90 ${src_path}/la_dsygst.f90
${src_path}/la_dorgtr.f90 ${src_path}/la_dpotrf.f90 ${src_path}/la_dgbtrf.f90 ${src_path}/la_dlamch.f90
${src_path}/la_dgbsv.f90 ${src_path}/la_dgbsv1.f90 ${src_path}/la_dgbsvx.f90 ${src_path}/la_dgbsvx1.f90
${src_path}/la_dgtsv.f90 ${src_path}/la_dgtsv1.f90 ${src_path}/la_dgtsvx.f90 ${src_path}/la_dgtsvx1.f90
${src_path}/la_dposv.f90 ${src_path}/la_dposv1.f90 ${src_path}/la_dposvx.f90 ${src_path}/la_dposvx1.f90
${src_path}/la_dppsv.f90 ${src_path}/la_dppsv1.f90 ${src_path}/la_dppsvx.f90 ${src_path}/la_dppsvx1.f90
${src_path}/la_dpbsv.f90 ${src_path}/la_dpbsv1.f90 ${src_path}/la_dpbsvx.f90 ${src_path}/la_dpbsvx1.f90
${src_path}/la_dptsv.f90 ${src_path}/la_dptsv1.f90 ${src_path}/la_dptsvx.f90 ${src_path}/la_dptsvx1.f90
${src_path}/la_dsysv.f90 ${src_path}/la_dsysv1.f90 ${src_path}/la_dsysvx.f90 ${src_path}/la_dsysvx1.f90
${src_path}/la_dspsv.f90 ${src_path}/la_dspsv1.f90 ${src_path}/la_dspsvx.f90 ${src_path}/la_dspsvx1.f90
${src_path}/la_dgels.f90 ${src_path}/la_dgels1.f90 ${src_path}/la_dgelsx.f90 ${src_path}/la_dgelsx1.f90
${src_path}/la_dgelss.f90 ${src_path}/la_dgelss1.f90 ${src_path}/la_dgglse.f90 ${src_path}/la_dggglm.f90
${src_path}/la_dsyev.f90 ${src_path}/la_dsyevd.f90 ${src_path}/la_dsyevx.f90 ${src_path}/la_dspev.f90
${src_path}/la_dspevd.f90 ${src_path}/la_dspevx.f90 ${src_path}/la_dsbev.f90 ${src_path}/la_dsbevd.f90
${src_path}/la_dsbevx.f90 ${src_path}/la_dstev.f90 ${src_path}/la_dstevd.f90 ${src_path}/la_dstevx.f90
${src_path}/la_dgees.f90 ${src_path}/la_dgeesx.f90 ${src_path}/la_dgeev.f90 ${src_path}/la_dgeevx.f90
${src_path}/la_dgesvd.f90 ${src_path}/la_dsygv.f90 ${src_path}/la_dspgv.f90 ${src_path}/la_dsbgv.f90
${src_path}/la_dgegs.f90 ${src_path}/la_dgegv.f90 ${src_path}/la_dggsvd.f90 ${src_path}/la_dgelsy.f90
${src_path}/la_dgelsy1.f90 ${src_path}/la_dspgvd.f90 ${src_path}/la_dsbgvd.f90 ${src_path}/la_dsygvd.f90
${src_path}/la_dsbgvx.f90 ${src_path}/la_dspgvx.f90 ${src_path}/la_dggev.f90 ${src_path}/la_dgges.f90
${src_path}/la_dstevr.f90 ${src_path}/la_dgelsd.f90 ${src_path}/la_dgelsd1.f90 ${src_path}/la_dsyevr.f90
${src_path}/la_dggevx.f90 ${src_path}/la_dsygvx.f90 ${src_path}/la_dggesx.f90 ${src_path}/la_dgesdd.f90
)

LIST(APPEND l95_src
${src_path}/la_cgesv.f90 ${src_path}/la_cgesv1.f90 ${src_path}/la_cgesvx.f90 ${src_path}/la_cgesvx1.f90
${src_path}/la_cgetrf.f90 ${src_path}/la_clagge.f90 ${src_path}/la_clange.f90 ${src_path}/la_clange1.f90
${src_path}/la_cgetri.f90 ${src_path}/la_cgetrs.f90 ${src_path}/la_cgetrs1.f90 ${src_path}/la_cgeequ.f90
${src_path}/la_cgerfs.f90 ${src_path}/la_cgerfs1.f90 ${src_path}/la_chetrd.f90 ${src_path}/la_chegst.f90
${src_path}/la_cungtr.f90 ${src_path}/la_cpotrf.f90 ${src_path}/la_cgbtrf.f90
${src_path}/la_cgbsv.f90 ${src_path}/la_cgbsv1.f90 ${src_path}/la_cgbsvx.f90 ${src_path}/la_cgbsvx1.f90
${src_path}/la_cgtsv.f90 ${src_path}/la_cgtsv1.f90 ${src_path}/la_cgtsvx.f90 ${src_path}/la_cgtsvx1.f90
${src_path}/la_cposv.f90 ${src_path}/la_cposv1.f90 ${src_path}/la_cposvx.f90 ${src_path}/la_cposvx1.f90
${src_path}/la_cppsv.f90 ${src_path}/la_cppsv1.f90 ${src_path}/la_cppsvx.f90 ${src_path}/la_cppsvx1.f90
${src_path}/la_cpbsv.f90 ${src_path}/la_cpbsv1.f90 ${src_path}/la_cpbsvx.f90 ${src_path}/la_cpbsvx1.f90
${src_path}/la_cptsv.f90 ${src_path}/la_cptsv1.f90 ${src_path}/la_cptsvx.f90 ${src_path}/la_cptsvx1.f90
${src_path}/la_csysv.f90 ${src_path}/la_csysv1.f90 ${src_path}/la_csysvx.f90 ${src_path}/la_csysvx1.f90
${src_path}/la_cspsv.f90 ${src_path}/la_cspsv1.f90 ${src_path}/la_cspsvx.f90 ${src_path}/la_cspsvx1.f90
${src_path}/la_chesv.f90 ${src_path}/la_chesv1.f90 ${src_path}/la_chesvx.f90 ${src_path}/la_chesvx1.f90
${src_path}/la_chpsv.f90 ${src_path}/la_chpsv1.f90 ${src_path}/la_chpsvx.f90 ${src_path}/la_chpsvx1.f90
${src_path}/la_cgels.f90 ${src_path}/la_cgels1.f90 ${src_path}/la_cgelsx.f90 ${src_path}/la_cgelsx1.f90
${src_path}/la_cgelss.f90 ${src_path}/la_cgelss1.f90 ${src_path}/la_cgglse.f90 ${src_path}/la_cggglm.f90
${src_path}/la_cheev.f90 ${src_path}/la_cheevd.f90 ${src_path}/la_cheevx.f90 ${src_path}/la_chpev.f90
${src_path}/la_chpevd.f90 ${src_path}/la_chpevx.f90 ${src_path}/la_chbev.f90 ${src_path}/la_chbevd.f90
${src_path}/la_chbevx.f90 ${src_path}/la_chbgvx.f90 ${src_path}/la_chpgvx.f90
${src_path}/la_cgees.f90 ${src_path}/la_cgeesx.f90 ${src_path}/la_cgeev.f90 ${src_path}/la_cgeevx.f90
${src_path}/la_cgesvd.f90 ${src_path}/la_chegv.f90 ${src_path}/la_chpgv.f90 ${src_path}/la_chbgv.f90
${src_path}/la_cgegs.f90 ${src_path}/la_cgegv.f90 ${src_path}/la_cggsvd.f90 ${src_path}/la_cgelsy.f90
${src_path}/la_cgelsy1.f90 ${src_path}/la_chpgvd.f90 ${src_path}/la_chbgvd.f90 ${src_path}/la_chegvd.f90
${src_path}/la_cggev.f90 ${src_path}/la_cgges.f90 ${src_path}/la_cgelsd.f90 ${src_path}/la_cgelsd1.f90
${src_path}/la_cheevr.f90 ${src_path}/la_cggevx.f90 ${src_path}/la_chegvx.f90 ${src_path}/la_cggesx.f90
${src_path}/la_cgesdd.f90
)

LIST(APPEND l95_src
${src_path}/la_zgesv.f90 ${src_path}/la_zgesv1.f90 ${src_path}/la_zgesvx.f90 ${src_path}/la_zgesvx1.f90
${src_path}/la_zgetrf.f90 ${src_path}/la_zlagge.f90 ${src_path}/la_zlange.f90 ${src_path}/la_zlange1.f90
${src_path}/la_zgetri.f90 ${src_path}/la_zgetrs.f90 ${src_path}/la_zgetrs1.f90 ${src_path}/la_zgeequ.f90
${src_path}/la_zgerfs.f90 ${src_path}/la_zgerfs1.f90 ${src_path}/la_zhetrd.f90 ${src_path}/la_zhegst.f90
${src_path}/la_zungtr.f90 ${src_path}/la_zpotrf.f90 ${src_path}/la_zgbtrf.f90
${src_path}/la_zgbsv.f90 ${src_path}/la_zgbsv1.f90 ${src_path}/la_zgbsvx.f90 ${src_path}/la_zgbsvx1.f90
${src_path}/la_zgtsv.f90 ${src_path}/la_zgtsv1.f90 ${src_path}/la_zgtsvx.f90 ${src_path}/la_zgtsvx1.f90
${src_path}/la_zposv.f90 ${src_path}/la_zposv1.f90 ${src_path}/la_zposvx.f90 ${src_path}/la_zposvx1.f90
${src_path}/la_zppsv.f90 ${src_path}/la_zppsv1.f90 ${src_path}/la_zppsvx.f90 ${src_path}/la_zppsvx1.f90
${src_path}/la_zpbsv.f90 ${src_path}/la_zpbsv1.f90 ${src_path}/la_zpbsvx.f90 ${src_path}/la_zpbsvx1.f90
${src_path}/la_zptsv.f90 ${src_path}/la_zptsv1.f90 ${src_path}/la_zptsvx.f90 ${src_path}/la_zptsvx1.f90
${src_path}/la_zsysv.f90 ${src_path}/la_zsysv1.f90 ${src_path}/la_zsysvx.f90 ${src_path}/la_zsysvx1.f90
${src_path}/la_zspsv.f90 ${src_path}/la_zspsv1.f90 ${src_path}/la_zspsvx.f90 ${src_path}/la_zspsvx1.f90
${src_path}/la_zhesv.f90 ${src_path}/la_zhesv1.f90 ${src_path}/la_zhesvx.f90 ${src_path}/la_zhesvx1.f90
${src_path}/la_zhpsv.f90 ${src_path}/la_zhpsv1.f90 ${src_path}/la_zhpsvx.f90 ${src_path}/la_zhpsvx1.f90
${src_path}/la_zgels.f90 ${src_path}/la_zgels1.f90 ${src_path}/la_zgelsx.f90 ${src_path}/la_zgelsx1.f90
${src_path}/la_zgelss.f90 ${src_path}/la_zgelss1.f90 ${src_path}/la_zgglse.f90 ${src_path}/la_zggglm.f90
${src_path}/la_zheev.f90 ${src_path}/la_zheevd.f90 ${src_path}/la_zheevx.f90 ${src_path}/la_zhpev.f90
${src_path}/la_zhpevd.f90 ${src_path}/la_zhpevx.f90 ${src_path}/la_zhbev.f90 ${src_path}/la_zhbevd.f90
${src_path}/la_zhbevx.f90 ${src_path}/la_zhbgvx.f90 ${src_path}/la_zhpgvx.f90
${src_path}/la_zgees.f90 ${src_path}/la_zgeesx.f90 ${src_path}/la_zgeev.f90 ${src_path}/la_zgeevx.f90
${src_path}/la_zgesvd.f90 ${src_path}/la_zhegv.f90 ${src_path}/la_zhpgv.f90 ${src_path}/la_zhbgv.f90
${src_path}/la_zgegs.f90 ${src_path}/la_zgegv.f90 ${src_path}/la_zggsvd.f90 ${src_path}/la_zgelsy.f90
${src_path}/la_zgelsy1.f90 ${src_path}/la_zhpgvd.f90 ${src_path}/la_zhbgvd.f90 ${src_path}/la_zhegvd.f90
${src_path}/la_zggev.f90 ${src_path}/la_zgges.f90 ${src_path}/la_zgelsd.f90 ${src_path}/la_zgelsd1.f90
${src_path}/la_zheevr.f90 ${src_path}/la_zggevx.f90 ${src_path}/la_zhegvx.f90 ${src_path}/la_zggesx.f90
${src_path}/la_zgesdd.f90
)

# as per original README, only one kind can be compiled
# target_sources(lapack95 PRIVATE
#     ${src_path}/la_auxmod.f90 ${src_path}/la_erinfo.f90 ${src_path}/la_ws_gels.f90 ${src_path}/la_ws_gelss.f90)
# target_sources(lapack95 PRIVATE ${${arith}_sources})

target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/la_auxmod.f90
  ${src_path}/la_erinfo.f90
  ${src_path}/la_ws_gels.f90
  ${src_path}/la_ws_gelss.f90
  ${src_path}/EASIFEM_LAPACK.f90
  ${src_path}/EASIFEM_F77_LAPACK.f90
  ${l95_src}
)
