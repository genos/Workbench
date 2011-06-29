program ppraxis_crt
implicit none
integer :: i
do i = 1, 1716
    if (mod(i,13) == 12 .and. mod(i,12) == 4 .and. mod(i,11) == 10) then
        write(*,*) i
        stop
    end if
end do
stop
end program ppraxis_crt
