      subroutine mf6swatp_day_exchange()

!!    MF6SWATp daily coupling hook (Stage A, milestone A1).
!!    Called once per simulated day, right after `call command` in
!!    time_control.f90, i.e. after every HRU/routing unit/channel/aquifer
!!    object has been processed for the day (so soil percolation, sepbtm,
!!    is final for every HRU). A no-op unless mf6swatp_enabled is true
!!    (see mf6swatp_module.f90).
!!
!!    Milestone A2 (vault plan_stageA_revised.md section 9, "scalar
!!    recharge" strategy): recharge_mm is a single, basin-wide,
!!    area-weighted daily recharge depth, meant to be applied uniformly
!!    to every MF6 cell for the first end-to-end test. It is raw HRU
!!    deep percolation (sepbtm) with NO delay/lag applied -- unlike
!!    GWFLOW's own gwflow_rech.f90, which smooths percolation into
!!    recharge with an exponential delay factor. Whether to add a delay
!!    here is still open (vault plan section 6.1.4); deferred until the
!!    scalar, uniform-recharge workflow itself is proven end to end.
!!    The rest of the file (sepbtm_sum/min/max) is the milestone A1
!!    smoke-test, kept for a quick per-day sanity check. Real per-object
!!    recharge export (landscape-unit aggregation, gwflow.lsucell
!!    mapping to MF6 cells) is a later milestone.

      use time_module
      use mf6swatp_module
      use hydrograph_module, only : sp_ob
      use hru_module, only : sepbtm, hru

      implicit none

      integer :: iu, ios
      integer :: j
      character(len=512) :: fname
      real :: sepbtm_sum, sepbtm_min, sepbtm_max
      real :: area_sum, area_wtd_sum
      real :: recharge_mm

      call mf6swatp_check_enabled()
      if (.not. mf6swatp_enabled) return

      mf6swatp_day_index = mf6swatp_day_index + 1

      sepbtm_sum = 0.
      sepbtm_min = huge(sepbtm_min)
      sepbtm_max = -huge(sepbtm_max)
      area_sum = 0.
      area_wtd_sum = 0.
      do j = 1, sp_ob%hru
        sepbtm_sum = sepbtm_sum + sepbtm(j)
        sepbtm_min = min(sepbtm_min, sepbtm(j))
        sepbtm_max = max(sepbtm_max, sepbtm(j))
        area_sum = area_sum + hru(j)%area_ha
        area_wtd_sum = area_wtd_sum + sepbtm(j) * hru(j)%area_ha
      end do

      if (area_sum > 0.) then
        recharge_mm = area_wtd_sum / area_sum
      else
        recharge_mm = 0.
      end if

      write (fname,'(A,"/mf6swatp_day_",I6.6,".txt")') trim(mf6swatp_exchange_out_dir), mf6swatp_day_index

      open (newunit=iu, file=trim(fname), status="replace", action="write", iostat=ios)
      if (ios /= 0) then
        write (*,*) "MF6SWATp: failed to open ", trim(fname), " iostat=", ios
        write (*,*) "MF6SWATp: does the directory ", trim(mf6swatp_exchange_out_dir), " exist?"
        return
      end if

      write (iu,'(A)')       "# MF6SWATp Stage A daily export (provisional format)"
      write (iu,'(A,I0)')    "day_index ", mf6swatp_day_index
      write (iu,'(A,I0)')    "year ", time%yrc
      write (iu,'(A,I0)')    "jday ", time%day
      write (iu,'(A,I0)')    "num_hru ", sp_ob%hru
      write (iu,'(A,F14.6)') "recharge_mm ", recharge_mm
      write (iu,'(A,F14.4)') "basin_area_ha ", area_sum
      write (iu,'(A,F14.4)') "sepbtm_sum_mm ", sepbtm_sum
      write (iu,'(A,F14.4)') "sepbtm_min_mm ", sepbtm_min
      write (iu,'(A,F14.4)') "sepbtm_max_mm ", sepbtm_max
      close (iu)

      end subroutine mf6swatp_day_exchange
