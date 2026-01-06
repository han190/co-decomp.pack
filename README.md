# co-decomp
Coarray decomposition/forward partition tool

## Compile
### Fortran Package Manager (fpm)
Compile and test with OpenCoarrays (built with OpenMPI),
```bash
fpm build --compiler caf
fpm test --compiler caf --runner "cafrun -n 8 --use-hwthread-cpus"
```
### CMake
```bash
mkdir build && cd build
cmake .. -G "Ninja"
```

## Example
```fortran
program main
  use, non_intrinsic :: co_decomp
  implicit none

  type(decomposition_type(rank=:)), allocatable :: decomp
  integer, dimension(2) :: num_tasks, num_procs

  num_tasks = [48, 32]
  num_procs = [2, num_images()/2]
  decomp = decompose(num_tasks, num_procs)
  if (this_image() == 1) print *, decomp
end program main
```
