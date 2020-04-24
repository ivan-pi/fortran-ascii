program reduce_ascii_table
use iso_fortran_env, only: int16
use mod_functional, only: set
implicit none

integer(int16), parameter :: full_table(0:127) = [16,16,16,16,16,16,16,16,16,400,144,&
144,144,144,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,448,104,104,&
104,104,104,104,104,104,104,104,104,104,104,104,104,6246,6246,6246,6246,6246,&
6246,6246,6246,4198,4198,104,104,104,104,104,104,104,5221,5221,5221,5221,5221,&
5221,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,&
1125,1125,1125,1125,1125,1125,104,104,104,104,104,104,4709,4709,4709,4709,&
4709,4709,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,&
613,613,613,104,104,104,104,16]

integer(int16), allocatable :: reduced_table(:)
integer(int16) :: i, j

reduced_table = set(full_table)

write(*,'(A)') "Reduced table:"
write(*,'(*(I0,:,","))') reduced_table

write(*,'(/,A)') "Indexes:"
do i=1,size(reduced_table)
    write(*,'(*(I0,:,","))') pack([(j,j=0,127)],full_table==reduced_table(i))
end do

end program