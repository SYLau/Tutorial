program test1
    use odeSolver_example_mod,only:rk45ad, tp, xp
    implicit none
        real(8)::t,tb
        real(8),dimension(:),allocatable::x
        real(8)::h
        real(8)::emax
        real(8)::hmin
        integer::itmax
        integer::iflag

        integer::uni
        integer::i

        !===================================================================
        ! Solve ODE
        !===================================================================
        h= 1.d-4                !set initial step size
        emax= 1.d-10            !set_emax
        hmin= 0                 !set_hmin
        itmax= 100              !set_itmax

        t=0                     !initial time
        tb=8.d0                !final time
        x = [1.d0, 0.d0]        !initial condition
        call rk45ad(ode_SHM,t,x,h,tb,itmax,emax,iflag,saveData=.true.,hmin=hmin)

        !===================================================================
        ! Output
        !===================================================================
        open(newunit=uni,file='result_SHM.dat',status='replace')
        write(uni, '(3a12)') 'time', 'x(t)', 'v(t)'
        do i=1,size(tp)
            write(uni,'(3es12.4)') tp(i), xp(1,i), xp(2,i)
        end do
        close(uni)

    contains

    subroutine ode_SHM(t,x,f)
        real(8), intent(in):: t, x(:)
        real(8), intent(out):: f(:)

        f(1) = x(2)
        f(2) = -x(1)
    end subroutine ode_SHM

end program test1
