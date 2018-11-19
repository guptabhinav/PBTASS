program histogram
implicit none
real*8, allocatable :: p(:)
real*8 :: grid_min, grid_max, grid_width, x, dum, norm, value
integer :: nbin, bin, i, steps

!open(1,file='input')
open(2,file='COLVAR',status='old')

!read(1,*) grid_min, grid_max
!read(1,*) nbin

!nbin=314
nbin=1000
grid_min=-3.14d0
grid_max=3.14d0
!CALL grid_min_max(2,grid_min,grid_max)

print*, 'grid_min and grid_max=', grid_min, grid_max

allocate(p(nbin))

grid_width = (grid_max - grid_min)/dfloat(nbin-1)

print *, 'grid width =', grid_width

rewind(2)

call step_count(2,steps)
print *, 'steps=', steps

rewind(2)

norm=0.d0
p(1:nbin)=0.d0
do i=1,steps
   read(2,*) dum,value
   bin = nint((value-grid_min)/grid_width)+1 
   if(bin.gt.0.and.bin.le.nbin) then
      p(bin) = p(bin)+ 1.d0
   end if
   norm = norm + 1.d0
end do

print*, 'norm =', norm
!norm = norm*grid_width
!print*, 'norm*area =', norm

open(10,file='output',status='replace')
do i=1,nbin
 x= grid_min + float(i-1)*grid_width
write(10,'(2f18.9)') x,p(i)/norm/grid_width
end do

print *, 'done'

end program

subroutine step_count(file_number,steps)
integer :: file_number, steps, ios,i
steps=0
do
 read(file_number,*,iostat=ios)
 if(ios.ne.0) exit
  steps=steps+1
end do
end subroutine

SUBROUTINE grid_min_max(num,grid_min,grid_max)
implicit none
integer :: num, ios
real*8 :: grid_min, grid_max
real*8 :: cv, dumm

rewind(num)
read(num,*,iostat=ios) dumm,cv
if (ios.ne.0) stop 'error reading colvar file'
grid_min=cv
grid_max=cv

rloop : DO
     read(num,*,iostat=ios)dumm,cv 
        if(ios.ne.0) exit rloop
     grid_min=MIN(cv,grid_min)
     grid_max=MAX(cv,grid_max)
end do rloop
end subroutine

