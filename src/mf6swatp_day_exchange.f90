subroutine mf6swatp_day_exchange()
  use time_module
  use mf6swatp_module
  implicit none

  integer :: iu, ios
  character(len=512) :: fname

  write(*,*) "MF6SWATp hook reached for day ", mf6swatp_day_index + 1

  if (.not. mf6swatp_enabled) return

  mf6swatp_day_index = mf6swatp_day_index + 1

  write(fname,'(A,"/mf6swatp_day_",I6.6,".txt")') trim(mf6swatp_exchange_out_dir), mf6swatp_day_index

  open(newunit=iu, file=trim(fname), status="replace", action="write", iostat=ios)
  if (ios /= 0) then
    write(*,*) "MF6SWATp failed to open: ", trim(fname), " iostat=", ios
    return
  end if

  write(iu,'(A)') "# MF6SWATp Stage A daily export"
  write(iu,'(A,I0)') "day_index ", mf6swatp_day_index
  write(iu,'(A,I0)') "year ", time%yrc
  write(iu,'(A,I0)') "jday ", time%day
  close(iu)

  write(*,*) "MF6SWATp wrote: ", trim(fname)
end subroutine mf6swatp_day_exchange