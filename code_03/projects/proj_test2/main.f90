program test2
    use odeSolver_example_mod,only:rk45ad, tp, xp
    implicit none
        real(8)::t,tb                           ! (8) in real(8) represents Double precision. Each real number uses 8 Bytes (64 bits) memory
        real(8),dimension(:),allocatable::x     ! allocatable array. Can have variable number of elements.
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
        emax= 1.d-10            !set limit on maximum local fractional error
        hmin= 0                 !set minimum step size
        itmax= 100              !set max. number of iterations

        t=0                     !initial time
        tb=4.d0                 !final time
        x = [0.d0, 20.d0]       !initial condition
        call rk45ad(ode_para,t,x,h,tb,itmax,emax,iflag,saveData=.true.,hmin=hmin) !adaptive step size RK4 integrator. Solutions are saved in variables tp, xp

        !===================================================================
        ! Output
        !===================================================================
        open(newunit=uni,file='result_parabola.dat',status='replace')
        write(uni, '(3a12)') 'time', 'x(t)', 'v(t)'
        do i=1,size(tp)
            write(uni,'(3es12.4)') tp(i), xp(1,i), xp(2,i)
        end do
        close(uni)

    contains

    !===================================================================
    ! Define ordinary differential equations
    ! Equation ddx = - 10
    ! Define x1 = x, x2 = dx
    !===================================================================

    subroutine ode_para(t,x,f)
        real(8), intent(in):: t, x(:)
        real(8), intent(out):: f(:)

        f(1) = x(2)
        f(2) = -10.d0
    end subroutine ode_para

end program test2

