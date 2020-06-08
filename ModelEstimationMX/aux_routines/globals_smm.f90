module globals_smm
  use prec

  integer, parameter:: &
       ni00 = 71, &
       nrii = 31, &
       nrff = 31, &
       nlbb = 31

  type estimation
     real(dp) grid_i00(ni00), min_i00, max_i00
     real(dp) grid_rii(nrii), min_rii, max_rii
     real(dp) grid_rff(nrff), min_rff, max_rff
     real(dp) grid_lbb(nlbb), min_lbb, max_lbb
     real(dp) dist(ni00,nrii,nrff,nlbb)
     integer imindist(4)
  end type estimation

  type(estimation) smm
end module globals_smm
