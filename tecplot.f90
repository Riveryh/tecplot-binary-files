!-----------------------------------------
!	author : huangyuhan@pku.edu.cn
!	purpose : generate tecplot binary files
!-----------------------------------------
module tecplot
implicit none
type,abstract,public :: tecplot_file
	private
	integer :: max_I, max_J, max_K
	integer :: n_data
	character(len=10),allocatable :: name_variables(:)
	logical :: isInitialized = .false.
	logical :: headerWrote = .false.
	integer :: n_zone_header = 0
	integer :: n_zone_data = 0
	integer :: fid,sfid
	real(kind=4) :: EOHMARKER = 357.0

	contains
		procedure,private :: write_header => plt_write_header_sb
		procedure,private :: write_char => plt_write_char_sb
		procedure,private :: write_str => plt_write_str_sb
		procedure,private :: count_var => plt_count_var_sb
end type tecplot_file

type,public,extends(tecplot_file) :: tecplot_time_file
! bound methods to subroutine
	contains
		procedure :: init => plt_init_sb
		procedure :: write_mesh => plt_write_mesh_sb
		procedure :: write_data => plt_write_time_data_sb
		procedure :: write_zone_data => plt_write_zone_data_sb
		procedure :: write_zone_header => plt_write_zone_header_sb
		procedure :: complete => plt_complete_sb
end type tecplot_time_file
private :: plt_init_sb, plt_write_mesh_sb

contains
	subroutine plt_init_sb(this, fname, nnx, nny, nnz, title, variables)
	implicit none
	class(tecplot_time_file) :: this
	character(len=*),intent(in) :: fname
	integer,intent(in) :: nnx,nny,nnz
	character(len=*),intent(in) :: title
	character(len=*),intent(in) :: variables
	real(kind=4) :: rand_num
	character(len=20) :: temp_str
	
	if(this%isInitialized) then
		write(*,*) 'TECPLOT, ERROR : plt file already initialized'
		return
	endif

	this%max_I = nnx
	this%max_J = nny
	this%max_K = nnz
	call RANDOM_NUMBER(rand_num)
	this%fid = int(rand_num*1000+10)
	open(unit=this%fid, file=fname, status='replace', form='binary')
	!open scratch files
	call RANDOM_NUMBER(rand_num)
	this%sfid = int(rand_num*1000+10)
	open(unit=this%sfid, status='scratch', form='binary')
	call this%write_header(nnx,nny,nnz,title,variables)
	this%isInitialized = .true.
	this%n_zone_header = 0
	this%n_zone_data = 0
	end subroutine plt_init_sb
	
	subroutine plt_write_mesh_sb(this, mesh)
	implicit none
	class(tecplot_time_file) :: this
	real(kind=4),intent(in) :: mesh(this%max_I,this%max_J,this%max_K,3)
	end subroutine plt_write_mesh_sb

	subroutine plt_write_time_data_sb(this, data, time)
	implicit none
	class(tecplot_time_file) :: this
	real(kind=4),intent(in) :: data(this%max_I,this%max_J,this%max_K,this%n_data)
	real(kind=4),intent(in) :: time
	integer,allocatable :: var_locations(:)
	call this%write_zone_header('zone',time,0,var_locations)
	deallocate(var_locations)
	end subroutine plt_write_time_data_sb

	subroutine plt_complete_sb(this)
	implicit none
	class(tecplot_time_file) :: this
	integer :: status
	character(kind=1) :: byte
	character(kind=1),allocatable :: bytes(:)
	integer(kind=8) :: max_single_data_length = 1000000
	integer(kind=8) :: file_length, current_pos

	if(.not. this%isInitialized)then
		write(*,*) 'TECPLOT, ERROR : plt file not initialized'
		return
	endif
	if(this%n_zone_data .ne. this%n_zone_header)then
		write(*,*) 'TECPLOT, WARNING : num of header is not equal to num of data, &
				 the plt file may be wrong'
	endif

	allocate(bytes(max_single_data_length))

	inquire(UNIT = this%sfid, SIZE=file_length)
	WRITE(*,*) 'TECPLOT: zone data length : ', file_length*1.0/(1024.0*1024.0), 'MB'
	! add eohmarker
	write(this%fid) this%eohmarker

	! move the scratch file pointer to the begin of the file
	rewind(this%sfid) 

	! move the data in scratch file to the end of plt file
	current_pos = 0
	do while(current_pos+max_single_data_length < file_length)
		read(this%sfid,iostat=status) bytes
		if(status.ne.0) exit
		write(this%fid) bytes
		current_pos = current_pos + max_single_data_length
	enddo
	do while(.true.)
		read(this%sfid,iostat=status) byte
		if(status.ne.0) exit
		write(this%fid) byte
	enddo
	! close two files
	close(this%fid)
	close(this%sfid)
	deallocate(bytes)
	deallocate(this%name_variables)
	this%isInitialized = .false.
	this%headerWrote = .false.
	this%n_zone_header = 0
	this%n_zone_data = 0
	end subroutine plt_complete_sb

	subroutine plt_write_header_sb(this,nnx,nny,nnz,title,variables)
	use string
	implicit none
	class(tecplot_file) :: this
	integer(kind=1) :: temp_int_1
	integer,intent(in) :: nnx,nny,nnz
	character(len=*),intent(in) :: title
	character(len=*),intent(in) :: variables
	type(string_splitter) :: splitter
	integer :: i

	
	if(this%headerWrote)then
		write(*,*) 'TECPLOT, ERROR : plt file header already wrote.'
		return
	endif

	write(this%fid) "#!TDV112"
	temp_int_1 = 1
	write(this%fid) temp_int_1

	temp_int_1 = 0 
	write(this%fid) temp_int_1
	write(this%fid) temp_int_1
	write(this%fid) temp_int_1
	write(this%fid) 0

	call this%write_str(title)

	call splitter%splite(variables,',')
	call splitter%count(this%n_data)
	!!!!!!!!!!!!!!!!!!!!!!
	write(this%fid) this%n_data
	!!!!!!!!!!!!!!!!!!!!!
	allocate(this%name_variables(this%n_data))
	! write(*,*)this%n_data
	do i = 1,this%n_data
		this%name_variables(i) = ''
		call splitter%next(this%name_variables(i))
		! write(*,*) this%name_variables(i), len(this%name_variables(i)), &
		! 		   len_trim(this%name_variables(i))
		call this%write_str(this%name_variables(i))
	enddo
	this%headerWrote = .true.
	end subroutine plt_write_header_sb

	subroutine plt_write_zone_header_sb(this, zone_name, sol_time, data_packing, var_locations)
	implicit none
	class(tecplot_time_file) :: this
	character(len=*),intent(in) :: zone_name
	real(kind=4),intent(in) :: sol_time
	integer,intent(in) :: data_packing
	integer,intent(in) :: var_locations(this%n_data)

	real(kind=4),parameter :: ZONEMARKER = 299.0
	integer :: zone_type = 0 ! ordered

	if(.not.this%headerWrote)then
		write(*,*) 'TECPLOT, ERROR: plt file header must be wrote before zone header'
		return
	endif

	if(data_packing .ne. 0 .and. data_packing .ne. 1)then
		write(*,*)"plt header input errer: data packing can only be 0 or 1"
		return
	endif

	write(this%fid) ZONEMARKER 
	call this%write_str(zone_name)
	write(this%fid) -1	! parent zone of this zone
	write(this%fid) -2	! StrandID
	write(this%fid) real(sol_time,kind=8)	! solution time, double
	write(this%fid) -1  ! reserved data position, useless
	write(this%fid) zone_type
	! write(this%fid) data_packing ! 0=block , 1=point
	write(this%fid) 1	! specify var location, 0 = do not specify
	write(this%fid) var_locations
	write(this%fid) 0
	write(this%fid) 0
	write(this%fid) this%max_I
	write(this%fid) this%max_J
	write(this%fid) this%max_K	

	write(this%fid) 0 ! no auxiliary name / value pair to follow

	this%n_zone_header = this%n_zone_header + 1

	end subroutine plt_write_zone_header_sb

	subroutine plt_write_char_sb(this, ch)
	implicit none
	class(tecplot_file) :: this
	character,intent(in) :: ch
	character(kind=1) :: temp_char
	integer(kind=1) :: temp_int
	temp_char = ch
	write(this%fid) temp_char
	temp_int = 0
	write(this%fid) temp_int,temp_int,temp_int
	end subroutine plt_write_char_sb

	subroutine plt_write_str_sb(this,str)
	implicit none
	class(tecplot_file) :: this
	character(len=*),intent(in) :: str
	integer :: i

	do i=1,len_trim(str)
		call this%write_char(str(i:i))
	enddo
	call this%write_char(char(0))
	end subroutine plt_write_str_sb

	subroutine plt_count_var_sb(this,variables,num)
	implicit none
	class(tecplot_file) :: this
	character,intent(in) :: variables(:)
	integer,intent(out) :: num
	num = 1
	end subroutine plt_count_var_sb

	! passive variables not supported yet.
	subroutine plt_write_zone_data_sb(this, type_list, shared_list, time_data)
	implicit none
	class(tecplot_time_file) :: this
	integer,intent(in) :: type_list(this%n_data)
	integer,intent(in) :: shared_list(this%n_data)
	real(kind=4), intent(in) :: time_data(this%max_I,this%max_J,this%max_K,this%n_data)
	real(kind=4) :: ZONEMARKER = 299.0
	real(kind=4),allocatable :: temp_data(:,:,:)
	integer :: i,j,k,m
	integer :: max_I,max_J,max_K

	if(.not.this%isInitialized)then
		write(*,*) 'TECPLOT, ERROR: plt file not initialized.'
		return
	endif

	! check input parameters
	if(size(type_list).ne.this%n_data .or. size(shared_list).ne.this%n_data)then
		write(*,*) 'TECPLOT, ERROR : wrong type list size or shared list size'
		return
	endif
	do i=1,this%n_data
		if(type_list(i) .ne. 1)then
			write(*,*) 'TECPLOT, ERROR : only float data format supported now'
			return
		endif
	enddo

	if(size(time_data).ne.(this%max_I*this%max_J*this%max_K*this%n_data))then
		write(*,*) 'TECPLOT ERROR : wrong data array size. size must be nx*ny*nz*n_data.',&
			' Shared data will be omitted but still must be provided in the data array'
		return
	endif


	max_I = this%max_I
	max_J = this%max_J
	max_K = this%max_K

	allocate(temp_data(this%max_I,this%max_J,this%max_K))
	write(this%sfid) ZONEMARKER
	! type: 1=float, 2=double, 3=long int, 4=short int, 5=byte, 6=bit
	write(this%sfid) type_list 
	write(this%sfid) 0 ! no passive variables
	write(this%sfid) 1 ! has shared variables
	write(this%sfid) shared_list
	write(this%sfid) -1
	do i=1,this%n_data
		! omit shared data
		if(shared_list(i).eq.(-1))then
			temp_data = time_data(:,:,:,i)
			write(this%sfid) real(minval(temp_data),kind=8)
			write(this%sfid) real(maxval(temp_data),kind=8)
		endif
	enddo
	do m=1,this%n_data
		! omit shared data
		if(shared_list(m).eq.-1)then
			write(this%sfid) (((time_data(i,j,k,m),i=1,max_I),j=1,max_J),k=1,max_K)
		endif
	enddo
	deallocate(temp_data)

	this%n_zone_data = this%n_zone_data + 1
	end subroutine plt_write_zone_data_sb

end module tecplot