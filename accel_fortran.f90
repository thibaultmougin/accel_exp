program testit
use m_timer,only : timer, say_hello
use OMP_LIB
implicit none


real :: a, b, c
real, allocatable :: array1(:,:,:),  array2(:,:,:),  array4(:,:,:)
real, allocatable :: array3(:)
integer,parameter           :: nx = 200
integer,parameter           :: ny = 200
integer,parameter           :: nz = 1000
type(timer)                 :: clock
integer                     :: i,j,k
character(len=*),parameter  :: all='(*(g0,1x))'

clock=timer()

print all, 'test 1'
print all, ''

allocate(array1(nx,ny,nz))
allocate(array2(nx,ny,nz))
allocate(array3(nx*ny*nz))
allocate(array4(nx,ny,nz))

array1 = 1.
array2 = 1.
array4 = 1.

a = 3.

call clock%tic()

!$OMP PARALLEL DO
do i = 1,nx
    do j = 1,ny
        do k = 1,nz
            array4(i,j,k) = 2.5*array1(i,j,k) + array2(i,j,k)
        enddo
    enddo
enddo
call clock%toc()
print*, array4(2,2,2), array4(4,2,5), array4(20,2,1)


call clock%print()

print all, ''
print all, 'test 2'
print all, ''

array1 = 1.
array2 = 1.
array4 = 1.

call clock%tic()

array4 = 2.5*array1+array2

call clock%toc()
print*, array4(2,2,2), array4(4,2,5), array4(20,2,1)

call clock%print()


end program testit