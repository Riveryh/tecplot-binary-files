program main
  implicit none
  call example1()
  
contains

  subroutine example1()
    ! use tecplot module
    use tecplot

    ! define a tecplot object
    type(tecplot_time_file) :: plt_file
    integer,allocatable :: locations(:)
    integer,allocatable :: type_list(:)
    integer,allocatable :: shared_list(:)
    integer,parameter :: num_of_variables = 6
    integer :: nx,ny,nz,i,j,k,d
    character(len=50) :: filename='test.plt'
    real(kind=4),allocatable :: your_datas(:,:,:,:)
    real(kind=4) :: physics_time
    real :: xyz(3), ijk(3)

    ! set dimensions
    nx = 20
    ny = 10
    nz = 5

    allocate(your_datas(nx,ny,nz,num_of_variables))
    allocate(locations(num_of_variables))
    allocate(type_list(num_of_variables))
    allocate(shared_list(num_of_variables))

    ! locations = 0 means data in node, 1 means data in cell(not supported yet)
    locations = 0
    ! shared_list(i)=-1 means the i-th data is not shared in this zone. If shared_list(i)=m, 
    ! it means the i-th data is shared with zone m in this file
    shared_list = -1
    ! type_list(i) = 0 means the i-th data is of type float. (Other data type not supported yet.)
    type_list = 1

    ! call init subroutine first
    ! nx, ny, nz means the dimension of the data
    ! 'x,y,z,u,v,w' is a string contains names of variables, must be divided by ','
    call plt_file%init(filename,nx,ny,nz,'Tecplot File Title','x,y,z,u,v,w')

    ! for each zone, call the two subroutines
    ! physics_time can be any value, it will only be used when there are more than 1 zone in a file.
    call plt_file%write_zone_header('zone name', physics_time, 0, locations) 

    ! your_datas(:,:,:,1:3) =  x,y,z coordinates(Variable assignment is omitted in this example)
    ! your_datas(:,:,:,4:6) =  u,v,w datas (Variable assignment is omitted in this example)
    ! ALL datas are stored in sequence like (((x(ix,iy,iz),ix=1,nx),iy=1,ny),iz=1,nz)
    ! set coordinate
    do d = 1, 3
       do concurrent(i=1:nx, j=1:ny, k=1:nz)
          xyz = [i-1., j-1., k-1.]
          your_datas(i,j,k,d) = xyz(d)
       end do
    end do
    ! set value
    do d = 4, num_of_variables
       do concurrent(i=1:nx, j=1:ny, k=1:nz)
          ijk = [i, j, k]
          your_datas(i,j,k,d) = ijk(d-3)
       end do
    end do
    call plt_file%write_zone_data(type_list, shared_list, your_datas)

    ! before exit, you must call complete subroutine
    call plt_file%complete

  end subroutine example1

end program main
