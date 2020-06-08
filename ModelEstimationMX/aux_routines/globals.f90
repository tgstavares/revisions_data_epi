module globals
  use prec

  ! parameters to be estimated
  type param
     real(dp) p0,i0,ri,rf,lambda
  end type param

  ! other variables
  real(dp) tt1,tt2

end module globals
