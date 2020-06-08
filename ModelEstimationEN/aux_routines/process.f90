module process
  use prec; use parameters; use globals
contains

  subroutine get_simulation(p,tsimul,x)
    implicit none
    type(param), intent(in):: p
    integer, intent(in):: tsimul
    real(dp), intent(out)::x(tsimul,ncols)
    real(dp) betai,betaf,beta,s1,s0,i1,i0,r1,r0,c1,c0,d1,d0,p1,p0,rc,nd,dl,ic
    integer t

    betai = p%ri * gamma
    betaf = p%rf * gamma

    p0 = p%p0
    i0 = p%i0
    s0 = p0-i0
    r0 = 0d0
    d0 = 0d0
    c0 = 0d0
    dl = d0

    do t=1,tsimul
       beta = betaf + (betai - betaf) * exp(-p%lambda * dble(t-1))
       s1   = s0 - beta*s0*i0/p0
       i1   = i0 + beta*s0*i0/p0 - gamma*i0
       r1   = r0 + gamma*i0 - theta*r0
       c1   = c0 + theta*(1d0-delta)*r0
       d1   = d0 + theta*delta*r0
       p1   = p0 - theta*delta*r0
       rc   = beta*(s0/p0)/gamma
       nd   = d0 - dl
       dl   = d0
       ic   = sigma * r0

       x(t,:) = (/dble(t-1),-(s1-s0),s0,i0,r0,c0,d0,nd,p0,p%p0-s1,ic,rc,beta/)

       s0=s1; i0=i1; r0=r1; c0=c1; d0=d1; p0=p1
    end do
  end subroutine get_simulation

  subroutine get_mss(p,tstart,tfinal,vt,vd,mss)
    implicit none
    type(param), intent(in):: p
    integer, intent(in):: tstart,tfinal
    real(dp), intent(in):: vt(:),vd(:)
    integer t
    real(dp) xx(tfinal+1,ncols),mss

    call get_simulation(p,tfinal+1,xx)
    mss = 0d0
    do t=tstart+1,tfinal+1
       !write(*,'(15f13.2)')xx(t,:)
       !write(*,'(15f13.2)')vt(t),vd(t),xx(t,:)
       !write(*,'(15f13.2)')vt(t),vd(t),xx(t,7)

       mss = mss + (log(xx(t,7)) - log(vd(t)))**2
       !mss = mss + (xx(t,7) - vd(t))**2       
    end do
    !stop
    mss = mss/dble((tfinal+1)-(tstart+1) + 1)
  end subroutine get_mss

  subroutine get_eqspace(n,min,max,x)
    implicit none
    integer, intent(in)::n
    real(dp), intent(in)::min,max
    real(dp), intent(out)::x(n)
    integer i
    real(dp) step
    if(n.eq.1)then
       x(1) = (max+min)/2d0
    else
       step = (max-min)/dble(n-1)
       x(1) = min
       do i=2,n-1
          x(i) = x(i-1) + step
       end do
       x(n) = max
    end if
  end subroutine get_eqspace

end module process
