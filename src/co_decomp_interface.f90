submodule(co_decomp) co_decomp_interface
implicit none (type, external)
contains

!> The "size" function for decomposition_type
pure module function get_size(decomp, dim, opt) result(ret)
  type(decomposition_type(rank=*)), intent(in) :: decomp
  integer, intent(in), optional :: dim
  character(len=*), intent(in), optional :: opt
  integer :: ret
  integer :: dim_
  character(len=:), allocatable :: opt_

  dim_ = optional_argument(dim, 0)
  opt_ = optional_argument(opt, "local")
  if (dim_ < 0 .or. dim_ > decomp%rank) &
    & error stop "[get_size] Invalid dim."
  select case (trim(opt_))
  case ("local")
    if (dim_ == 0) then
      ret = product(decomp%local_size)
    else
      ret = decomp%local_size(dim_)
    end if
  case ("global")
    if (dim_ == 0) then
      ret = product(decomp%global_size)
    else
      ret = decomp%global_size(dim_)
    end if
  end select
end function get_size

!> The `shape` function for decomposition_type
pure module function get_shape(decomp, opt) result(ret)
  type(decomposition_type(rank=*)), intent(in) :: decomp
  character(len=*), intent(in), optional :: opt
  integer, allocatable :: ret(:)
  character(len=:), allocatable :: opt_

  opt_ = optional_argument(opt, "local")
  select case (trim(opt_))
  case ("local")
    ret = decomp%local_size
  case ("local_max")
    ret = decomp%local_size_max
  case ("global")
    ret = decomp%global_size
  case default
    error stop "[get_shape] Invalid option."
  end select
end function get_shape

!> Get base index
pure module function base_index(decomp) result(ret)
  type(decomposition_type(rank=*)), intent(in) :: decomp
  integer, allocatable :: ret(:)

  ret = decomp%base_index
end function base_index

!> Get remainder
pure module function remainder(decomp) result(ret)
  type(decomposition_type(rank=*)), intent(in) :: decomp
  integer, allocatable :: ret(:)

  ret = decomp%remainder
end function remainder

!> Get number of processors
pure module function get_coshape(decomp) result(ret)
  type(decomposition_type(rank=*)), intent(in) :: decomp
  integer, allocatable :: ret(:)

  ret = decomp%num_procs
end function get_coshape

!> Get co_index
pure module function get_thisimage(decomp, dim) result(ret)
  type(decomposition_type(rank=*)), intent(in) :: decomp
  integer, intent(in), optional :: dim
  integer :: ret
  integer :: dim_

  dim_ = optional_argument(dim, 0)
  if (dim_ < 0 .or. dim_ > decomp%rank) &
    & error stop "[get_thisimage] Invalid dim."
  select case (dim_)
  case (0)
    ret = convert(decomp%num_procs, decomp%co_index)
  case default
    ret = decomp%co_index(dim_)
  end select
end function get_thisimage

end submodule co_decomp_interface
