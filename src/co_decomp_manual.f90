submodule(co_decomp) co_decomp_manual
implicit none (type, external)
contains

!> Wrapper of rank 1 decomposition.
module function decompose_manual_scalar(num_tasks, num_procs) result(decomp)
  integer, intent(in) :: num_tasks, num_procs
  type(decomposition_type) :: decomp

  decomp = decompose_manual_vector([num_tasks], [num_procs])
end function decompose_manual_scalar

!> Decomposition type constructor
module function decompose_manual_vector(num_tasks, num_procs) result(decomp)
  integer, intent(in) :: num_tasks(:), num_procs(:)
  type(decomposition_type) :: decomp
  integer, dimension(size(num_tasks)) :: m, r, alpha, m_alpha, k0_alpha
  integer :: n

  decomp%rank = size(num_tasks)
  !> Runtime check
  n = size(num_tasks)
  if (n /= size(num_procs)) error stop &
    & "[decompose_manual_vector] Invalid processors or size."
  
  !> Allocate decomp
  call reallocate(decomp%global_size, decomp%rank)
  call reallocate(decomp%num_procs, decomp%rank)
  call reallocate(decomp%local_size_max, decomp%rank)
  call reallocate(decomp%local_size, decomp%rank)
  call reallocate(decomp%co_index, decomp%rank)
  call reallocate(decomp%remainder, decomp%rank)
  call reallocate(decomp%base_index, decomp%rank)
  
  m = get_maxlocalsize(num_tasks, num_procs)
  r = get_remainder(num_tasks, num_procs)
  alpha = convert(num_procs, this_image())
  m_alpha = get_localsize(r, alpha, m)
  k0_alpha = get_baseindex(r, alpha, m_alpha)

  decomp%global_size = num_tasks
  decomp%num_procs = num_procs
  decomp%local_size_max = m
  decomp%remainder = r
  decomp%co_index = alpha
  decomp%local_size = m_alpha
  decomp%base_index = k0_alpha
end function decompose_manual_vector

!> Compute local index from global index.
module function get_location(decomp, global_index, recompute) result(local_index)
  type(decomposition_type), intent(inout) :: decomp
  integer, intent(in) :: global_index(:)
  logical, intent(in), optional :: recompute
  integer, allocatable :: local_index(:)

  if (size(global_index) /= decomp%rank) error stop &
    & "[get_location] Invalid global index."
  if (present(recompute)) then
    if (.not. recompute) then
      local_index = global_index - decomp%base_index
      return
    end if
  end if

  associate (k => global_index, &
    & r => decomp%remainder, &
    & alpha => decomp%co_index, &
    & m => decomp%local_size_max, &
    & m_alpha => decomp%local_size, &
    & k0 => decomp%base_index)

    alpha = get_coindex(r, m, k)
    m_alpha = get_localsize(r, alpha, m)
    k0 = get_baseindex(r, alpha, m_alpha)
    local_index = k - k0
  end associate
end function get_location

!> Compute base index
impure elemental function get_baseindex(r, alpha, m_alpha) result(k0_alpha)
  integer, intent(in) :: r, alpha, m_alpha
  integer :: k0_alpha

  if (r == 0) then
    k0_alpha = (alpha - 1) * m_alpha
    return
  end if

  if (alpha <= r) then
    k0_alpha = (alpha - 1) * m_alpha
  else
    k0_alpha = (alpha - 1) * m_alpha + r
  end if
end function get_baseindex

!> Compute co-index
impure elemental function get_coindex(r, m, k) result(alpha)
  integer, intent(in) :: r, m, k
  integer :: alpha

  if (r == 0) then
    alpha = (k - 1) / m + 1
    return
  end if

  if (k <= m * r) then
    alpha = (k - 1) / m + 1
  else
    alpha = (k - r - 1) / (m - 1) + 1
  end if
end function get_coindex

!> Compute local size
impure elemental function get_localsize(r, alpha, m) result(m_alpha)
  integer, intent(in) :: r, alpha, m
  integer :: m_alpha

  if (r == 0) then
    m_alpha = m
    return
  end if

  if (alpha <= r) then
    m_alpha = m
  else
    m_alpha = m - 1
  end if
end function get_localsize

!> Compute maximum local size
impure elemental function get_maxlocalsize(n, p) result(m)
  integer, intent(in) :: n, p
  integer :: m

  m = (n - 1) / p + 1
end function get_maxlocalsize

!> Compute remainder
impure elemental function get_remainder(n, p) result(r)
  integer, intent(in) :: n, p
  integer :: r

  r = mod(n, p)
end function get_remainder

end submodule co_decomp_manual
