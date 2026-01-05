submodule(co_decomp) co_decomp_util
implicit none (type, external)
contains

!> Convert index from array form to integer form.
pure module function convert_arr2int(shapes, index_arr) result(index_int)
  integer, intent(in) :: shapes(:), index_arr(:)
  integer :: index_int
  integer :: i

  if (size(shapes) /= size(index_arr)) &
    & error stop "[convert_arr2int] Invalid index."
  if (size(shapes) == 1) then
    index_int = index_arr(1)
    return
  end if

  index_int = index_arr(1)
  do i = size(index_arr), 2, -1
    index_int = index_int + (index_arr(i) - 1) * product(shapes(:i - 1))
  end do
end function convert_arr2int

!> Convert index from integer form to array form.
pure module function convert_int2arr(shapes, index_int) result(index_arr)
  integer, intent(in) :: shapes(:), index_int
  integer, allocatable :: index_arr(:)
  integer :: tmp, rem, i

  if (index_int > product(shapes)) &
    & error stop "[convert_int2arr] Invalid index."
  if (size(shapes) == 1) then
    index_arr = [index_int]
    return
  end if

  associate (n => size(shapes))
    if (.not. allocated(index_arr)) then
      allocate (index_arr(n))
    else if (size(index_arr) < n) then
      deallocate (index_arr)
      allocate (index_arr(n))
    end if
  end associate

  tmp = index_int
  do i = size(shapes), 2, -1
    associate (p => product(shapes(1:i - 1)))
      rem = tmp - (tmp - 1) / p * p
      index_arr(i) = (tmp - rem) / p + 1
      tmp = rem
    end associate
  end do
  index_arr(1) = rem
end function convert_int2arr

!> Fill optional argument (int32) with default value if not present.
pure module function optional_arg_int32(opt_arg, default_val) result(ret)
  integer, intent(in), optional :: opt_arg
  integer, intent(in) :: default_val
  integer :: ret

  if (present(opt_arg)) then
    ret = opt_arg
  else
    ret = default_val
  end if
end function optional_arg_int32

!> Fill optional argument (char) with default value if not present.
pure module function optional_arg_char(opt_arg, default_val) result(ret)
  character(len=*), intent(in), optional :: opt_arg
  character(len=*), intent(in) :: default_val
  character(len=:), allocatable :: ret

  if (present(opt_arg)) then
    ret = opt_arg
  else
    ret = default_val
  end if
end function optional_arg_char

end submodule co_decomp_util