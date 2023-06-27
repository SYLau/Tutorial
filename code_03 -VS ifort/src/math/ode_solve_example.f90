module odeSolver_example_mod
    implicit none
    private
    public::rk45ad
    public::tp, xp

    !precision
    integer, parameter::dp=kind(1.d0)
    integer, parameter::qp=kind(1.q0)
    integer, parameter::wp=dp

    !Solution of the integration are stored in tp and xp
    real(wp), dimension(:), allocatable :: tp
    real(wp), dimension(:,:), allocatable :: xp

    real(wp):: tSAS
    integer:: iStep, dataSize

    interface reallocate_a
        module procedure reallocate_rv_a, reallocate_rm_a
    endinterface reallocate_a

contains

!===================================================================
! Example block
!===================================================================
!    solve_ode: block
!        use odeSolver_mod,only:rk45ad
!        real(wp)::t,tb
!        real(wp),dimension(:),allocatable::x
!        real(wp)::h
!        real(wp)::emax
!        real(wp)::hmin
!        integer::itmax
!        integer::iflag
!
!        h=!set_hi
!        emax=!set_emax
!        hmin=!set_hmin
!        itmax=!set_itmax
!
!        t=!ti
!        tb=!tf
!        x=!xi
!        call rk45ad(fcn,t,x,h,tb,itmax,emax,iflag,saveData=.true.,hmin=hmin)
!    end block solve_ode

    subroutine rk45ad(fcn,t,x,h,tb,itmax,emax,iflag,saveData,xScaleIn,hmin,dtSaveIn)
        ! Adaptive step size RK4 ODE solver
        ! Modified from numerical recipes
        implicit none
        integer, intent(in):: itmax
        logical, intent(in):: saveData
        real(wp),intent(in):: tb,emax
        real(wp), intent(inout):: t,x(:),h
        integer, intent(out):: iflag
        real(wp), optional, intent(in) :: xScaleIn(:),hmin
        real(wp), optional, intent(in):: dtSaveIn
!        real(wp) ::delta=0.5d-14
        real(wp)::d,xsave(1:size(x)),tsave, e, xScale(1:size(x)), f(1:size(x)), dtSave
        integer :: n, k
        real(wp), parameter:: safety=0.9_wp, pGrow= -0.2_wp, pShrink= -0.25_wp, errcon=1.89e-4_wp, tiny= 1.e-30_wp !The value errcon equals (5/safety)**(1/pGrow), requires scale-up factor to be at most 5
        logical,save:: warn=.true.

        interface deriv
            subroutine fcn(t,x,f)
                import wp
                real(wp), intent(in):: t, x(:)
                real(wp), intent(out):: f(:)
            endsubroutine fcn
        endinterface deriv
        n = size(x)
        iflag = 1 ! iflag 0  :-> integration finished
        k = 0
        iStep = 0
        dataSize = 0
        dtSave = 0._wp
        if (present(dtSaveIn)) dtSave = dtSaveIn
        tSAS = t-2._wp*dtSave
        if (allocated(tp)) deallocate(tp)
        if (allocated(xp)) deallocate(xp)
        allocate(tp(256))
        allocate(xp(size(x),size(tp)))
        do
            k = k + 1
            if (k > itmax) exit
            d = abs(tb - t)
            if(d <= abs(h))  then !integration range smaller than stepsize  :-> integrate for one more step
                iflag = 0
!                if(d <= delta*max(abs(tb),abs(t))) then
!                    !if (saveData) call save_a_step !no increase in step, simply terminate
!                    if (saveData) call truncate_data
!                    exit    !integration range smaller than bounds * delta :-> integration finished
!                endif
                h = sign(1._wp,h)*d     !make the last stepsize = d
            end if
            xsave = x
            tsave = t

            if (present(xScaleIn)) then
                xScale(1:n) = xScaleIn(1:n)
            else
                call fcn(t, x, f)
                xScale(1:n) = abs(x(1:n))+abs(h*f(1:n)) + tiny !modify this line to give a custom xScale
            endif

            call rkck45(fcn,t,x,h,xScale(1:n),e) !integrate a single step using Runge-Kutta-Cash-Karp method
            if (iflag == 0) then !exit and save last step
                iStep = iStep +1
                if (saveData) call save_a_step
                if (saveData) call truncate_data
                exit
            endif

            if (present(hmin)) then !new
                if (abs(h)<abs(hmin)) then
                    if (warn) print*, 'warning:rk45ad stepsize smaller than hmin; replacing h by hmin'
                    warn = .false.
                    iStep = iStep +1
                    k = 0
                    if (abs(t-tSAS) > abs(dtSave) .and. saveData) call save_a_step
                    h=sign(abs(hmin),h)
                    cycle
                endif
            endif
            if (e > emax) then !integrate again with shrinked step size
                h= sign(max(abs(safety*h*(e/emax)**pShrink), abs(h)/10._wp), h)
                x = xsave
                t = tsave
                if(t == t+h) print*,t,h
                if(t == t+h) then
                    write(*,*) 'stepsize underflow rk45ad'
                    stop
                end if

            else
                iStep = iStep +1
                k = 0
                if (abs(t-tSAS) > abs(dtSave) .and. saveData) call save_a_step

                if (e/emax > errcon) then !check if the scale-up process is too fast
                    h= safety*h*(e/emax)**pGrow !next step size is scaled up
                else
                    h= 5._wp*h !next step size is scaled up by at most 5
                endif
            endif
            !if (present(hmin)) then
            !  if (abs(h)<abs(hmin)) pause 'err:rk45ad stepsize smaller than hmin'
            !endif

        enddo

    contains
        subroutine save_a_step
            dataSize = dataSize +1
            if (size(tp)<dataSize) then
                tp=reallocate_a(tp,2*size(tp))
                xp=reallocate_a(xp,size(xp,1),2*size(tp))
            endif
            tp(dataSize)=t
            xp(:,dataSize)=x(:)
            tSAS = t
        end subroutine
        subroutine truncate_data
            tp=reallocate_a(tp,dataSize)
            xp=reallocate_a(xp,size(xp,1),dataSize)
        end subroutine
    end subroutine rk45ad

    subroutine rkck45(fcn,t,x,h,xScale,e)   !Runge-Kutta-Cash-Karp method
        implicit none
        real(wp), intent(inout):: t,h,e,x(:),xScale(:)
        integer :: n
        real(wp), dimension(1:size(x)):: xi, f, f1,f2,f3,f4,f5,f6, x5, err
        real(wp):: c20,c21,c30,c31,c32,c40,c41,c42,c43,c51,c52,c53,c54,c60,c61,c62,c63,c64,c65,a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6
        interface deriv
            subroutine fcn(t,x,f)
                import wp
                real(wp), intent(in):: t, x(:)
                real(wp), intent(out):: f(:)
            endsubroutine fcn
        endinterface deriv

        c20=0.2_wp; c21=0.2_wp; c30=0.3_wp; c31=0.075_wp; c32=0.225_wp
        c40=3._wp/5._wp; c41=3._wp/10._wp; c42=-9._wp/10._wp; c43=6._wp/5._wp
        c51=-11._wp/54._wp; c52=5._wp/2._wp; c53=-70._wp/27._wp; c54=35._wp/27._wp
        c60=0.875_wp; c61=1631._wp/55296; c62=175._wp/512; c63=575._wp/13824; c64=44275._wp/110592; c65=253._wp/4096
        a1=2825._wp/27648._wp; a2=0._wp; a3=18575._wp/48384._wp; a4=13525._wp/55296._wp; a5=277._wp/14336._wp; a6=0.25_wp
        b1=37._wp/378._wp; b2=0._wp; b3=250._wp/621._wp; b4=125._wp/594._wp; b5=0._wp; b6=512._wp/1771._wp
        n = size(x)
        xi(1:n) = x(1:n)
        call fcn(t, xi, f)
        f1(1:n) = h*f(1:n)

        xi(1:n) = x(1:n) + c21*f1(1:n)
        call fcn(t+ c20*h, xi, f)
        f2(1:n) = h*f(1:n)

        xi(1:n) = x(1:n) + c31*f1(1:n) + c32*f2(1:n)
        call fcn(t+ c30*h, xi, f)
        f3(1:n) = h*f(1:n)

        xi(1:n) = x(1:n) + c41*f1(1:n) + c42*f2(1:n) + c43*f3(1:n)
        call fcn(t+ c40*h, xi, f)
        f4(1:n) = h*f(1:n)

        xi(1:n) = x(1:n) + c51*f1(1:n) + c52*f2(1:n) + c53*f3(1:n) + c54*f4(1:n)
        call fcn(t+h, xi, f)
        f5(1:n) = h*f(1:n)

        xi(1:n) = x(1:n) + c61*f1(1:n) + c62*f2(1:n) + c63*f3(1:n) + c64*f4(1:n) + c65*f5(1:n)
        call fcn(t+ c60*h, xi, f)
        f6(1:n) = h*f(1:n)

        x5(1:n) = x(1:n) + b1*f1(1:n) + b3*f3(1:n) + b4*f4(1:n) + b5*f5(1:n) + b6*f6(1:n)
        x(1:n) = x(1:n) + a1*f1(1:n) + a3*f3(1:n) + a4*f4(1:n) + a5*f5(1:n)  +  a6*f6(1:n)
        t = t + h
        err(1:n) = abs(x(1:n) - x5(1:n)) /abs(xScale(1:n))
        e= maxval(err(1:n))
    end subroutine rkck45


    function reallocate_rv_a(p,n)
        real(wp), dimension(:), allocatable :: p, reallocate_rv_a
        integer, intent(in) :: n
        integer :: nold,ierr
        allocate(reallocate_rv_a(n),stat=ierr)
        if (ierr /= 0) call err_msg('reallocate_rv_a: problem in attempt to allocate memory')
        if (.not. allocated(p)) return
        nold=size(p,1)
        reallocate_rv_a(1:min(nold,n))=p(1:min(nold,n))
        deallocate(p)
    endfunction reallocate_rv_a
    function reallocate_rm_a(p,n,m)
        real(wp), dimension(:,:), allocatable :: p, reallocate_rm_a
        integer, intent(in) :: n,m
        integer :: nold,mold,ierr
        allocate(reallocate_rm_a(n,m),stat=ierr)
        if (ierr /= 0) call err_msg('reallocate_rm_a: problem in attempt to allocate memory')
        if (.not. allocated(p)) return
        nold=size(p,1)
        mold=size(p,2)
        reallocate_rm_a(1:min(nold,n),1:min(mold,m))=p(1:min(nold,n),1:min(mold,m))
        deallocate(p)
    endfunction reallocate_rm_a

    subroutine err_msg(msg)
        character(len=*)::msg
        write(*,*) msg
        stop
    end subroutine err_msg

endmodule odeSolver_example_mod

