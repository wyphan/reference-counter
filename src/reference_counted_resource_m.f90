module reference_counted_resource_m
  use hermetic_interface ,only: hermetic
  use ref_counter_implementation ,only: ref_counter
  implicit none

  private
  public :: reference_counted_resource_t

  type, abstract, extends(hermetic) :: reference_counted_resource_t
    type(ref_counter) :: counter
  contains
    procedure, non_overridable :: force_finalize
    procedure, non_overridable :: register_self
  end type

  interface

    module subroutine force_finalize (this)
      implicit none
      class(reference_counted_resource_t), intent(inout) :: this
    end subroutine

    module subroutine register_self (this)
      implicit none
      class(reference_counted_resource_t), intent(inout) :: this
    end subroutine

  end interface

end module reference_counted_resource_m
