program main

  use, non_intrinsic :: co_decomp
  implicit none

  type(decomposition_type) :: decomp
  integer, dimension(2) :: num_tasks, num_procs
  integer :: i

  if (num_images() < 2) error stop "At least 2 images required."
  num_tasks = [47, 47]

  do i = 1, 2
    num_procs = [num_images()/i, i]
    if (this_image() == 1) print "(a, *(i0, 1x))", "num_procs=", num_procs
    sync all

    decomp = decompose(num_tasks, num_procs)
    critical
      print "(dt(1,7))", decomp
      print "(a, '=', i0)", "this_image", this_image(decomp)
    end critical
    sync all
  end do

  decomp = decompose([49], [num_images()])
  if (this_image() == 1) print "(a, *(i0, 1x))", "num_procs=", 49
  sync all
  critical
    print "(dt)", decomp
    print "(a, '=', i0, ',', i0, '->', i0)", &
      & "this_image", this_image(decomp), &
      & base_index(decomp) + 1, &
      & base_index(decomp) + size(decomp)
  end critical

end program main
