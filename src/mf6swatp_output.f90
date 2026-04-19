subroutine mf6swatp_output_message(msg)
  implicit none
  character(len=*), intent(in) :: msg
  write(*,'(A)') trim(msg)
end subroutine mf6swatp_output_message