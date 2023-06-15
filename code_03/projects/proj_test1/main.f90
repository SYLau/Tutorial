program test1
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
        emax= 1.d-10            !set_emax
        hmin= 0                 !set_hmin
        itmax= 100              !set_itmax

        t=0                     !initial time
        tb=8.d0                 !final time
        x = [1.d0, 0.d0]        !initial condition
        call rk45ad(ode_SHM,t,x,h,tb,itmax,emax,iflag,saveData=.true.,hmin=hmin) !adaptive step size RK4 integrator. Solutions are saved in variables tp, xp

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

    !===================================================================
    ! Define ordinary differential equations
    ! Equation ddx = - x
    ! Define x1 = x, x2 = dx
    !===================================================================
    subroutine ode_SHM(t,x,dx)
        real(8), intent(in):: t, x(:)
        real(8), intent(out):: dx(:)

        dx(1) = x(2)
        dx(2) = -x(1)
    end subroutine ode_SHM

end program test1
