      module mf6swatp_module

!!    MF6SWATp coupling module (Stage A, milestone A1).
!!    Holds the on/off switch for the daily coupling hook and the state it
!!    needs across days. See docs (vault) plan_stageA_revised.md section 6.

      implicit none

      !! activation: MF6 mode is on only if mf6swatp_config_file exists in the
      !! working directory at the time of the first daily hook call. Checked
      !! once (mf6swatp_checked) and cached in mf6swatp_enabled. With no
      !! config file, mf6swatp_enabled stays .false. and the hook is a no-op,
      !! so plain SWAT+ (gwflow=0 or gwflow=1) behaves exactly as unmodified.
      logical :: mf6swatp_enabled = .false.
      logical :: mf6swatp_checked = .false.

      character(len=512) :: mf6swatp_config_file = "mf6swatp.cfg"
      character(len=512) :: mf6swatp_exchange_out_dir = "exchange/out"
      character(len=512) :: mf6swatp_exchange_in_dir  = "exchange/in"

      integer :: mf6swatp_day_index = 0

      contains

      subroutine mf6swatp_check_enabled()

!!    Checked once per run, on the first call to mf6swatp_day_exchange.
!!    Sets mf6swatp_enabled from whether mf6swatp_config_file exists in the
!!    working directory. Also refuses to run coupled with GWFLOW active
!!    (bsn_cc%gwflow == 1), since MF6SWATp replaces the lumped aquifers and
!!    running both would double-count groundwater.

      use basin_module, only : bsn_cc

      implicit none

      logical :: file_exists

      if (mf6swatp_checked) return
      mf6swatp_checked = .true.

      inquire (file=trim(mf6swatp_config_file), exist=file_exists)
      mf6swatp_enabled = file_exists

      if (mf6swatp_enabled) then
        write (*,*) "MF6SWATp: enabled (found ", trim(mf6swatp_config_file), ")"
        if (bsn_cc%gwflow == 1) then
          write (*,*) "MF6SWATp: FATAL - ", trim(mf6swatp_config_file), &
            " is present but GWFLOW is active (gwflow = 1)."
          write (*,*) "MF6SWATp: MF6 replaces the lumped aquifers and must run with gwflow = 0."
          write (*,*) "MF6SWATp: remove ", trim(mf6swatp_config_file), &
            " or set gwflow = 0 in codes.bsn, then rerun."
          stop 1
        end if
      end if

      end subroutine mf6swatp_check_enabled

      end module mf6swatp_module
