module string
implicit none

type,public :: string_splitter
	private
	character(len=:),allocatable :: buffer
	character :: delimeter
	integer :: current_pos
	integer :: size
	contains
		procedure :: splite => string_splitt
		procedure :: next => next_substring
		procedure :: count => count_substring
		final :: destroy_splitter
end type string_splitter

private :: string_splitt,next_substring

contains
	subroutine string_splitt(this,source,delimeter)
	implicit none
	class(string_splitter) :: this
	character(len=*),intent(in) :: source
	character,intent(in) :: delimeter
	integer :: buffer_size
	
	buffer_size = len(source)
	allocate(character(len=buffer_size) :: this%buffer)

	this%buffer = source
	this%delimeter = delimeter
	this%current_pos = 0
	this%size = len(trim(source))
	write(*,*) source
	write(*,*) this%buffer
	write(*,*) this%size
	end subroutine string_splitt

	! next out_str must be at least 50 chars long
	subroutine next_substring(this,out_str)
	implicit none
	class(string_splitter) :: this
	character(len=*),intent(out) :: out_str
	! character :: temp(50)
	integer :: sub_pos
	if(this%current_pos>this%size)then
		! out_str(1:1) = char(0)
		return
	endif
	sub_pos = 1
	this%current_pos = this%current_pos + 1
	do while(this%buffer(this%current_pos : this%current_pos) &
			.ne. this%delimeter)
		
		out_str(sub_pos:sub_pos) = this%buffer(this%current_pos : this%current_pos)
		sub_pos = sub_pos + 1
		this%current_pos = this%current_pos + 1
		if(this%current_pos > this%size)then
			! out_str(sub_pos:sub_pos) = char(0)
			return
		endif
	enddo
	! out_str(sub_pos:sub_pos) = char(0)
	end subroutine next_substring


	subroutine count_substring(this,num)
	implicit none
	class(string_splitter) :: this
	integer, intent(out) :: num
	integer :: i
	num = 0
	if(this%size .eq. 0 ) then
		return
	endif
	do i = 1,this%size
		if(this%buffer(i:i).eq.this%delimeter)then
			num = num + 1
		endif
	enddo
	num = num + 1
	end subroutine count_substring

	subroutine destroy_splitter(this)
	implicit none
	type(string_splitter) :: this
	if(allocated(this%buffer))then
		deallocate(this%buffer)
	endif
	end subroutine destroy_splitter
end module string