module co_decomp
implicit none (type, external)

public :: decomposition_type
public :: decompose
public :: write(formatted)
public :: size
public :: shape
public :: coshape
public :: this_image
public :: base_index
private

!> Decomposition type
type :: decomposition_type(rank)
  integer, len :: rank
  integer :: global_size(rank)
  integer :: num_procs(rank)
  integer :: local_size_max(rank)
  integer :: local_size(rank)
  integer :: co_index(rank)
  integer :: remainder(rank)
  integer :: base_index(rank)
end type decomposition_type

!> Wrapper decompose
interface decompose
  module procedure :: decompose_manual_vector
  module procedure :: decompose_manual_scalar
end interface decompose

!> UDDTIO for decomposition
interface write(formatted)
  module procedure :: write_formatted
end interface write(formatted)

!> Override intrinsic size
interface size
  module procedure :: get_size
end interface size

!> Override intrinsic shape
interface shape
  module procedure :: get_shape
end interface shape

!> Override intrinsic coshape
interface coshape
  module procedure :: get_coshape
end interface coshape

!> Override intrinsic this_image
interface this_image
  module procedure :: get_thisimage
end interface this_image

!> Convert index from array/integer form to integer/array form.
interface convert
  module procedure :: convert_arr2int
  module procedure :: convert_int2arr
end interface convert

!> Fill optional argument with default value if not present.
interface optional_argument
  module procedure :: optional_arg_int32
  module procedure :: optional_arg_char
end interface optional_argument

interface
  !> Decomposition type constructor
  module function decompose_manual_vector(num_tasks, num_procs) result(decomp)
    integer, intent(in) :: num_tasks(:), num_procs(:)
    type(decomposition_type(rank=:)), allocatable :: decomp
  end function decompose_manual_vector

  !> Wrapper of rank 1 decomposition.
  module function decompose_manual_scalar(num_tasks, num_procs) result(decomp)
    integer, intent(in) :: num_tasks, num_procs
    type(decomposition_type(rank=:)), allocatable :: decomp
  end function decompose_manual_scalar

  !> write(formatted)
  module subroutine write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    class(decomposition_type(rank=*)), intent(in) :: dtv
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
  end subroutine write_formatted

  !> Compute local index from global index
  module function get_location(decomp, global_index, recompute) result(local_index)
    type(decomposition_type(rank=*)), intent(inout) :: decomp
    integer, intent(in) :: global_index(:)
    logical, intent(in), optional :: recompute
    integer, allocatable :: local_index(:)
  end function get_location

  !> The "size" function for decomposition
  pure module function get_size(decomp, dim, opt) result(ret)
    type(decomposition_type(rank=*)), intent(in) :: decomp
    integer, intent(in), optional :: dim
    character(len=*), intent(in), optional :: opt
    integer :: ret
  end function get_size

  !> The `shape` function for decomposition
  pure module function get_shape(decomp, opt) result(ret)
    type(decomposition_type(rank=*)), intent(in) :: decomp
    character(len=*), intent(in), optional :: opt
    integer, allocatable :: ret(:)
  end function get_shape

  !> Get base index
  pure module function base_index(decomp) result(ret)
    type(decomposition_type(rank=*)), intent(in) :: decomp
    integer, allocatable :: ret(:)
  end function base_index

  !> Get remainder
  pure module function remainder(decomp) result(ret)
    type(decomposition_type(rank=*)), intent(in) :: decomp
    integer, allocatable :: ret(:)
  end function remainder

  !> Get number of processors
  pure module function get_coshape(decomp) result(ret)
    type(decomposition_type(rank=*)), intent(in) :: decomp
    integer, allocatable :: ret(:)
  end function get_coshape

  !> Get co_index
  pure module function get_thisimage(decomp, dim) result(ret)
    type(decomposition_type(rank=*)), intent(in) :: decomp
    integer, intent(in), optional :: dim
    integer :: ret
  end function get_thisimage

  !> Convert index from array form to integer form.
  pure module function convert_arr2int(shapes, index_arr) result(index_int)
    integer, intent(in) :: shapes(:), index_arr(:)
    integer :: index_int
  end function convert_arr2int

  !> Convert index from integer form to array form.
  pure module function convert_int2arr(shapes, index_int) result(index_arr)
    integer, intent(in) :: shapes(:), index_int
    integer, allocatable :: index_arr(:)
  end function convert_int2arr

  !> Fill optional argument (int32) with default value if not present.
  pure module function optional_arg_int32(opt_arg, default_val) result(ret)
    integer, intent(in), optional :: opt_arg
    integer, intent(in) :: default_val
    integer :: ret
  end function optional_arg_int32

  !> Fill optional argument (char) with default value if not present.
  pure module function optional_arg_char(opt_arg, default_val) result(ret)
    character(len=*), intent(in), optional :: opt_arg
    character(len=*), intent(in) :: default_val
    character(len=:), allocatable :: ret
  end function optional_arg_char
end interface

end module co_decomp
