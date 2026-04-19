module mf6swatp_module
  implicit none

  logical :: mf6swatp_enabled = .false.
  integer :: mf6swatp_debug = 1

  character(len=512) :: mf6swatp_exchange_out_dir = "exchange/out"
  character(len=512) :: mf6swatp_exchange_in_dir  = "exchange/in"

  integer :: mf6swatp_day_index = 0

contains

  subroutine mf6swatp_initialize_defaults()
    implicit none
    mf6swatp_enabled = .true.
    mf6swatp_day_index = 0
  end subroutine mf6swatp_initialize_defaults

end module mf6swatp_module