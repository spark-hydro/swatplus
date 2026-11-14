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

      !! Feedback path (plan_feedback_path.md, F1): water that MF6 returns to a
      !! channel. Python (through the API) puts a volume per channel in
      !! mf6swatp_gw_to_channel; the channel routine adds it to the channel's
      !! storage the next time it runs (same variable and same place as GWFLOW's
      !! own groundwater exchange), then zeroes the request, so a day that is
      !! not set means zero flow, never a repeat of yesterday. The switch stays
      !! false until the API's first set_value, and the arrays are allocated only
      !! by the library, so an ordinary SWAT+ executable run never touches any of this.
      logical :: mf6swatp_feedback_enabled = .false.
      real(8), allocatable, target :: mf6swatp_gw_to_channel(:)   !! m3 requested, positive = into the channel
      real(8), allocatable, target :: mf6swatp_gw_applied(:)      !! m3 actually added to the channel storage the last time

      contains

      subroutine mf6swatp_feedback_init(n_channels)

!!    Allocates the two arrays (all zero). Does NOT turn the feedback on: the
!!    API does that at the first set_value, so a run that never sets anything
!!    behaves exactly as without the feedback. A second call keeps the first
!!    allocation.

      implicit none

      integer, intent(in) :: n_channels

      if (.not. allocated(mf6swatp_gw_to_channel)) then
        allocate (mf6swatp_gw_to_channel(n_channels), source = 0.d0)
        allocate (mf6swatp_gw_applied(n_channels), source = 0.d0)
      end if

      end subroutine mf6swatp_feedback_init

      subroutine mf6swatp_apply_gw_to_channel(ich)

!!    Adds the requested groundwater volume to the water of channel `ich`.
!!    Called from sd_channel_control3 right after GWFLOW's own exchange block
!!    and before the routing.
!!
!!    The water is added to BOTH ch_stor and tot_stor. ch_rtmusk routes the
!!    channel's water from tot_stor (inflow is added to it, the outflow is
!!    taken from it) and then rebuilds ch_stor from it, so a change to ch_stor
!!    alone is overwritten and lost (found by the F3 pulse test, 2026-11-14).
!!    ch_stor is changed too because ch_rtmusk takes the start-of-day storage
!!    for its own water balance from ch_stor and tot_stor = ch_stor + fp_stor
!!    must keep holding; the injected water then counts as water that was
!!    already in the channel at the start of the day.
!!
!!    A removal cannot exceed what the channel holds (the same rule as
!!    gwflow_channel_exch). The amount recorded in mf6swatp_gw_applied is the
!!    change that really happened to the 32-bit storage, so it can be audited.

      use hydrograph_module, only : ch_stor, tot_stor

      implicit none

      integer, intent(in) :: ich
      real(8) :: wanted
      real :: before

      if (.not. mf6swatp_feedback_enabled) return
      if (ich < 1 .or. ich > size(mf6swatp_gw_to_channel)) return

      wanted = mf6swatp_gw_to_channel(ich)
      if (wanted < 0.d0) wanted = max(wanted, -dble(ch_stor(ich)%flo))
      before = ch_stor(ich)%flo
      ch_stor(ich)%flo = before + real(wanted)
      mf6swatp_gw_applied(ich) = dble(ch_stor(ich)%flo) - dble(before)
      tot_stor(ich)%flo = tot_stor(ich)%flo + real(mf6swatp_gw_applied(ich))
      mf6swatp_gw_to_channel(ich) = 0.d0

      end subroutine mf6swatp_apply_gw_to_channel

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
