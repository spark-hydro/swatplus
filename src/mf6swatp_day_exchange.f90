subroutine mf6swatp_day_exchange()
  use time_module
  use mf6swatp_module
  implicit none

  integer :: iu
  character(len=512) :: fname

  if (.not. mf6swatp_enabled) return

  mf6swatp_day_index = mf6swatp_day_index + 1

  write(fname,'(A,"/mf6swatp_day_",I6.6,".txt")') trim(mf6swatp_exchange_out_dir), mf6swatp_day_index

  open(newunit=iu, file=trim(fname), status="replace", action="write")
  write(iu,'(A)') "# MF6SWATp Stage A daily export"
  write(iu,'(A,I0)') "day_index ", mf6swatp_day_index
  write(iu,'(A,I0)') "year ", time%yrc
  write(iu,'(A,I0)') "jday ", time%day
  write(iu,'(A,L1)') "mf6swatp_enabled ", mf6swatp_enabled
  close(iu)

end subroutine mf6swatp_day_exchange