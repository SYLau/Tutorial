module type_mod
    implicit none

    !precision
    integer, parameter::dp=kind(1.d0)
    integer, parameter::wp=dp

    !constants
     real(wp), parameter:: G=6.67430e-8_wp, M_sun=1.98855e33_wp, c= 2.99792458e10_wp
     real(wp), parameter:: pi= 3.14159265358979323846_wp, AU= 1.495978707e13_wp, pi2=pi*2, pi4=pi*4
     real(wp), parameter:: year=86400._wp*365
     real(wp), parameter:: Mpc=3.08567758e24_wp
     real(wp), parameter:: R_sun=6.957e10_wp
     real(wp), parameter:: h=6.626176e-27_wp

end module
