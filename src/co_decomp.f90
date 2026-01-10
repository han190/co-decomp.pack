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
type :: decomposition_type
  integer :: rank
  integer, allocatable :: global_size(:)
  integer, allocatable :: num_procs(:)
  integer, allocatable :: local_size_max(:)
  integer, allocatable :: local_size(:)
  integer, allocatable :: co_index(:)
  integer, allocatable :: remainder(:)
  integer, allocatable :: base_index(:)
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
    !> Number of tasks in each dimension (length = rank).
    integer, intent(in) :: num_tasks(:)
    !> Number of processors in each dimension (length = rank).
    integer, intent(in) :: num_procs(:)
    !> Resulting decomposition object.
    type(decomposition_type) :: decomp
  end function decompose_manual_vector

  !> Wrapper of rank 1 decomposition.
  module function decompose_manual_scalar(num_tasks, num_procs) result(decomp)
    !> Number of tasks (scalar).
    integer, intent(in) :: num_tasks
    !> Number of processors (scalar).
    integer, intent(in) :: num_procs
    !> Resulting decomposition object.
    type(decomposition_type) :: decomp
  end function decompose_manual_scalar

  !> write(formatted)
  module subroutine write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    !> Decomposition object to be written.
    class(decomposition_type), intent(in) :: dtv
    !> Fortran unit number to write to.
    integer, intent(in) :: unit
    !> I/O type string (format specifier).
    character(len=*), intent(in) :: iotype
    !> Value list to be written.
    integer, intent(in) :: v_list(:)
    !> Returned I/O status.
    integer, intent(out) :: iostat
    !> I/O message buffer (in/out).
    character(len=*), intent(inout) :: iomsg
  end subroutine write_formatted

  !> Compute local index from global index
  module function get_location(decomp, global_index, recompute) result(local_index)
    !> Decomposition object (may be modified/recomputed).
    type(decomposition_type), intent(inout) :: decomp
    !> Global multi-dimensional index to locate.
    integer, intent(in) :: global_index(:)
    !> Optional flag to force recomputation.
    logical, intent(in), optional :: recompute
    !> Returned local index corresponding to `global_index`.
    integer, allocatable :: local_index(:)
  end function get_location

  !> The "size" function for decomposition
  pure module function get_size(decomp, dim, opt) result(ret)
    !> Decomposition object to query.
    type(decomposition_type), intent(in) :: decomp
    !> Optional dimension index to query.
    integer, intent(in), optional :: dim
    !> Optional option string (e.g., "local" or "global").
    character(len=*), intent(in), optional :: opt
    !> Returned size value.
    integer :: ret
  end function get_size

  !> The `shape` function for decomposition
  pure module function get_shape(decomp, opt) result(ret)
    !> Decomposition object to query.
    type(decomposition_type), intent(in) :: decomp
    !> Optional option string influencing returned shape.
    character(len=*), intent(in), optional :: opt
    !> Returned shape array (one entry per dimension).
    integer, allocatable :: ret(:)
  end function get_shape

  !> Get base index
  pure module function base_index(decomp) result(ret)
    !> Decomposition object to query.
    type(decomposition_type), intent(in) :: decomp
    !> Returned base index array for each dimension.
    integer, allocatable :: ret(:)
  end function base_index

  !> Get remainder
  pure module function remainder(decomp) result(ret)
    !> Decomposition object to query.
    type(decomposition_type), intent(in) :: decomp
    !> Returned remainder array (per-dimension leftover counts).
    integer, allocatable :: ret(:)
  end function remainder

  !> Get number of processors
  pure module function get_coshape(decomp) result(ret)
    !> Decomposition object to query.
    type(decomposition_type), intent(in) :: decomp
    !> Returned number-of-processors array (co-shape) per dimension.
    integer, allocatable :: ret(:)
  end function get_coshape

  !> Get co_index
  pure module function get_thisimage(decomp, dim) result(ret)
    !> Decomposition object to query.
    type(decomposition_type), intent(in) :: decomp
    !> Optional dimension index for which to return this image id.
    integer, intent(in), optional :: dim
    !> Returned image index (processor coordinate or linear id).
    integer :: ret
  end function get_thisimage

  !> Convert index from array form to integer form.
  pure module function convert_arr2int(shapes, index_arr) result(index_int)
    !> Shape array describing the multi-dimensional layout.
    integer, intent(in) :: shapes(:)
    !> Index array (multi-dimensional indices) to convert.
    integer, intent(in) :: index_arr(:)
    !> Resulting linear/integer index.
    integer :: index_int
  end function convert_arr2int

  !> Convert index from integer form to array form.
  pure module function convert_int2arr(shapes, index_int) result(index_arr)
    !> Shape array describing the multi-dimensional layout.
    integer, intent(in) :: shapes(:)
    !> Integer (linear) index to convert into an array-form index.
    integer, intent(in) :: index_int
    !> Returned multi-dimensional index array.
    integer, allocatable :: index_arr(:)
  end function convert_int2arr

  !> Re-allocate integer array.
  module pure subroutine reallocate(array, n)
    !> Allocatable integer array to (re)allocate.
    integer, allocatable, intent(inout) :: array(:)
    !> New length to allocate for `array`.
    integer, intent(in) :: n
  end subroutine reallocate

  !> Fill optional argument (int32) with default value if not present.
  pure module function optional_arg_int32(opt_arg, default_val) result(ret)
    !> Optional integer argument provided by caller.
    integer, intent(in), optional :: opt_arg
    !> Default value used when `opt_arg` is not present.
    integer, intent(in) :: default_val
    !> Returned integer: either `opt_arg` or `default_val`.
    integer :: ret
  end function optional_arg_int32

  !> Fill optional argument (char) with default value if not present.
  pure module function optional_arg_char(opt_arg, default_val) result(ret)
    !> Optional character/string argument provided by caller.
    character(len=*), intent(in), optional :: opt_arg
    !> Default string used when `opt_arg` is not present.
    character(len=*), intent(in) :: default_val
    !> Returned string (allocated to appropriate length).
    character(len=:), allocatable :: ret
  end function optional_arg_char
end interface

end module co_decomp
