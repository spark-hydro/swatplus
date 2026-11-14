      module swatplus_bmi

!!    MF6SWATp (S4, proof-of-concept for a SWAT+ API; plan_swatplus_api.md).
!!    The control surface of libswatplus_api.so, following the BMI/XMI
!!    conventions MODFLOW 6 uses, so Python can drive it with xmipy:
!!    every function returns a status (0 = success, 1 = failure) and
!!    get_last_bmi_error gives the message. Compiled only into the library,
!!    never into the swatplus executable (see CMakeLists.txt).
!!
!!    SWAT+ keeps all of its state in module variables, so only one model can
!!    live in a process: initialize works once, and a finalized library
!!    cannot be started again.
!!
!!    update() advances exactly one simulated day. It owns the year/day
!!    counters that the loops in time_control used to own, and calls the same
!!    pieces in the same order: time_control_year_start on the first day of a
!!    year, time_control_day, time_control_year_end after the last.

      use, intrinsic :: iso_c_binding, only : c_int, c_double, c_char, c_null_char, c_ptr, c_loc
      use time_module, only : time
      use basin_module, only : prog
      use hru_module, only : sepbtm, hru
      use hydrograph_module, only : sp_ob

      implicit none

      integer, parameter :: LEN_VERSION = 256
      integer, parameter :: LEN_GRIDTYPE = 17
      integer, parameter :: LEN_COMPONENT_NAME = 256
      integer, parameter :: LEN_VAR_ADDRESS = 1024
      integer, parameter :: LEN_VAR_TYPE = 51
      integer, parameter :: LEN_ERR_MESSAGE = 1024

      !! sizes of the character buffers the caller allocates; xmipy reads
      !! these as integer globals by name
      integer(c_int), bind(C, name="BMI_LENVERSION") :: BMI_LENVERSION = LEN_VERSION
      integer(c_int), bind(C, name="BMI_LENGRIDTYPE") :: BMI_LENGRIDTYPE = LEN_GRIDTYPE
      integer(c_int), bind(C, name="BMI_LENCOMPONENTNAME") :: BMI_LENCOMPONENTNAME = LEN_COMPONENT_NAME
      integer(c_int), bind(C, name="BMI_LENVARADDRESS") :: BMI_LENVARADDRESS = LEN_VAR_ADDRESS
      integer(c_int), bind(C, name="BMI_LENVARTYPE") :: BMI_LENVARTYPE = LEN_VAR_TYPE
      integer(c_int), bind(C, name="BMI_LENERRMESSAGE") :: BMI_LENERRMESSAGE = LEN_ERR_MESSAGE

      integer(c_int), parameter :: BMI_SUCCESS = 0
      integer(c_int), parameter :: BMI_FAILURE = 1

      logical, save, private :: api_initialized = .false.
      logical, save, private :: api_ever_initialized = .false.
      logical, save, private :: api_year_open = .false.
      integer, save, private :: api_curyr = 0            !! sequential year being run
      integer, save, private :: api_julian_day = 0       !! day of year last run
      integer, save, private :: api_day_last = 0         !! last day of the open year
      integer, save, private :: api_days_done = 0        !! days run so far
      integer, save, private :: api_days_total = 0       !! days in the whole run
      character(len=LEN_ERR_MESSAGE), save, private :: api_last_error = ""

      !! variables readable through get_value_ptr (S5). The name lookup is
      !! var_code(); adding a variable means one new code, one case in
      !! var_code, and one case each in get_var_type / get_var_shape /
      !! get_value_ptr.
      integer, parameter, private :: VAR_NONE = 0
      integer, parameter, private :: VAR_SEPBTM = 1     !! HRU/SEPBTM  (float, one value per HRU)
      integer, parameter, private :: VAR_AREA_HA = 2    !! HRU/AREA_HA (double, copy made at initialize)

      !! hru(j)%area_ha as a flat double-precision array: area_ha is a
      !! component of an array of derived types, so it has no flat memory of
      !! its own to point at
      real(c_double), allocatable, target, save, private :: api_area_ha(:)

      contains

      function bmi_initialize(config_file) result(bmi_status) bind(C, name="initialize")
        character(kind=c_char), intent(in) :: config_file(*)
        integer(c_int) :: bmi_status
        external :: swat_initialize, time_control_start
        integer :: j

        bmi_status = BMI_FAILURE
        if (api_ever_initialized) then
          call set_error("SWAT+ was already initialized in this process; it keeps its state in module variables and cannot be started twice")
          return
        end if
        api_ever_initialized = .true.

        call swat_initialize

        if (time%step < 0) then
          call set_error("export-coefficient mode (time%step < 0) is not supported by the API")
          return
        end if

        call time_control_start
        api_days_total = count_run_days()
        api_curyr = 0
        api_julian_day = 0
        api_days_done = 0
        api_year_open = .false.

        allocate (api_area_ha(sp_ob%hru))
        do j = 1, sp_ob%hru
          api_area_ha(j) = real(hru(j)%area_ha, c_double)
        end do

        api_initialized = .true.
        bmi_status = BMI_SUCCESS
      end function bmi_initialize

      function bmi_update() result(bmi_status) bind(C, name="update")
        integer(c_int) :: bmi_status
        external :: time_control_year_start, time_control_day, time_control_year_end

        bmi_status = BMI_FAILURE
        if (.not. api_initialized) then
          call set_error("update called before initialize, or after finalize")
          return
        end if
        if (api_days_done >= api_days_total) then
          call set_error("the run is already at its end time")
          return
        end if

        if (.not. api_year_open) then
          api_curyr = api_curyr + 1
          call time_control_year_start (api_curyr)
          api_julian_day = time%day_start - 1
          api_day_last = time%day_end_yr
          if (api_day_last <= api_julian_day) then
            call set_error("the year to run contains no days")
            return
          end if
          api_year_open = .true.
        end if

        api_julian_day = api_julian_day + 1
        call time_control_day (api_julian_day)
        api_days_done = api_days_done + 1

        if (api_julian_day == api_day_last) then
          call time_control_year_end
          api_year_open = .false.
        end if
        bmi_status = BMI_SUCCESS
      end function bmi_update

      function bmi_finalize() result(bmi_status) bind(C, name="finalize")
        integer(c_int) :: bmi_status
        external :: time_control_finish, swat_finish

        bmi_status = BMI_FAILURE
        if (.not. api_initialized) then
          call set_error("finalize called before initialize, or twice")
          return
        end if
        call time_control_finish
        call swat_finish
        api_initialized = .false.
        bmi_status = BMI_SUCCESS
      end function bmi_finalize

      function bmi_get_start_time(start_time) result(bmi_status) bind(C, name="get_start_time")
        real(c_double), intent(out) :: start_time
        integer(c_int) :: bmi_status
        start_time = 0.0_c_double
        bmi_status = BMI_SUCCESS
      end function bmi_get_start_time

      function bmi_get_end_time(end_time) result(bmi_status) bind(C, name="get_end_time")
        real(c_double), intent(out) :: end_time
        integer(c_int) :: bmi_status
        end_time = 0.0_c_double
        if (.not. api_initialized) then
          call set_error("get_end_time called before initialize")
          bmi_status = BMI_FAILURE
          return
        end if
        end_time = real(api_days_total, c_double)
        bmi_status = BMI_SUCCESS
      end function bmi_get_end_time

      function bmi_get_current_time(current_time) result(bmi_status) bind(C, name="get_current_time")
        real(c_double), intent(out) :: current_time
        integer(c_int) :: bmi_status
        current_time = real(api_days_done, c_double)
        bmi_status = BMI_SUCCESS
      end function bmi_get_current_time

      function bmi_get_time_step(time_step) result(bmi_status) bind(C, name="get_time_step")
        real(c_double), intent(out) :: time_step
        integer(c_int) :: bmi_status
        time_step = 1.0_c_double
        bmi_status = BMI_SUCCESS
      end function bmi_get_time_step

      function bmi_get_component_name(name) result(bmi_status) bind(C, name="get_component_name")
        character(kind=c_char), intent(out) :: name(*)
        integer(c_int) :: bmi_status
        call to_c_string("SWATPLUS", name, LEN_COMPONENT_NAME)
        bmi_status = BMI_SUCCESS
      end function bmi_get_component_name

      function bmi_get_version(version) result(bmi_status) bind(C, name="get_version")
        character(kind=c_char), intent(out) :: version(*)
        integer(c_int) :: bmi_status
        if (api_ever_initialized) then
          call to_c_string(trim(adjustl(prog)), version, LEN_VERSION)
        else
          call to_c_string("SWAT+ (not initialized)", version, LEN_VERSION)
        end if
        bmi_status = BMI_SUCCESS
      end function bmi_get_version

      function bmi_get_last_error(bmi_error) result(bmi_status) bind(C, name="get_last_bmi_error")
        character(kind=c_char), intent(out) :: bmi_error(*)
        integer(c_int) :: bmi_status
        call to_c_string(api_last_error, bmi_error, LEN_ERR_MESSAGE)
        bmi_status = BMI_SUCCESS
      end function bmi_get_last_error

      !! ---- variable access (S5) ---------------------------------------------

      function bmi_get_var_rank(c_var_address, var_rank) result(bmi_status) bind(C, name="get_var_rank")
        character(kind=c_char), intent(in) :: c_var_address(*)
        integer(c_int), intent(out) :: var_rank
        integer(c_int) :: bmi_status

        var_rank = 0
        bmi_status = BMI_FAILURE
        if (var_code(c_var_address) == VAR_NONE) return
        var_rank = 1          !! both variables are 1-D, one value per HRU
        bmi_status = BMI_SUCCESS
      end function bmi_get_var_rank

      function bmi_get_var_type(c_var_address, var_type) result(bmi_status) bind(C, name="get_var_type")
        character(kind=c_char), intent(in) :: c_var_address(*)
        character(kind=c_char), intent(out) :: var_type(*)
        integer(c_int) :: bmi_status

        bmi_status = BMI_FAILURE
        select case (var_code(c_var_address))
        case (VAR_SEPBTM)
          call to_c_string("FLOAT", var_type, LEN_VAR_TYPE)
        case (VAR_AREA_HA)
          call to_c_string("DOUBLE", var_type, LEN_VAR_TYPE)
        case default
          return
        end select
        bmi_status = BMI_SUCCESS
      end function bmi_get_var_type

      function bmi_get_var_shape(c_var_address, var_shape) result(bmi_status) bind(C, name="get_var_shape")
        character(kind=c_char), intent(in) :: c_var_address(*)
        integer(c_int), intent(out) :: var_shape(*)
        integer(c_int) :: bmi_status

        bmi_status = BMI_FAILURE
        select case (var_code(c_var_address))
        case (VAR_SEPBTM)
          var_shape(1) = size(sepbtm)
        case (VAR_AREA_HA)
          var_shape(1) = size(api_area_ha)
        case default
          return
        end select
        bmi_status = BMI_SUCCESS
      end function bmi_get_var_shape

      !! hands out the address of SWAT+'s own memory; nothing is copied, so
      !! the caller sees every later update. The address stays valid until
      !! finalize (sepbtm is allocated once, in allocate_parms)
      function bmi_get_value_ptr(c_var_address, x) result(bmi_status) bind(C, name="get_value_ptr")
        character(kind=c_char), intent(in) :: c_var_address(*)
        type(c_ptr), intent(out) :: x
        integer(c_int) :: bmi_status

        bmi_status = BMI_FAILURE
        select case (var_code(c_var_address))
        case (VAR_SEPBTM)
          x = c_loc(sepbtm(1))
        case (VAR_AREA_HA)
          x = c_loc(api_area_ha(1))
        case default
          return
        end select
        bmi_status = BMI_SUCCESS
      end function bmi_get_value_ptr

      !! which variable a C address names (upper-cased, so "hru/sepbtm"
      !! works too). Sets the error message and returns VAR_NONE when the
      !! library is not running or the name is unknown.
      function var_code(c_var_address) result(code)
        character(kind=c_char), intent(in) :: c_var_address(*)
        integer :: code
        character(len=LEN_VAR_ADDRESS) :: name
        integer :: i, ch

        code = VAR_NONE
        if (.not. api_initialized) then
          call set_error("variables can only be read between initialize and finalize")
          return
        end if

        name = ""
        do i = 1, LEN_VAR_ADDRESS - 1
          if (c_var_address(i) == c_null_char) exit
          ch = iachar(c_var_address(i))
          if (ch >= iachar("a") .and. ch <= iachar("z")) ch = ch - 32
          name(i:i) = achar(ch)
        end do

        select case (trim(name))
        case ("HRU/SEPBTM")
          code = VAR_SEPBTM
        case ("HRU/AREA_HA")
          code = VAR_AREA_HA
        case default
          call set_error("unknown variable '" // trim(name) // "'; available: HRU/SEPBTM, HRU/AREA_HA")
        end select
      end function var_code

      !! number of days the run will simulate, from the same rules
      !! time_control_year_start applies (leap years, first and last day)
      function count_run_days() result(total)
        integer :: total
        integer :: iy, yr, days_in_year, first, last

        total = 0
        do iy = 1, time%nbyr
          yr = time%yrc_start + iy - 1
          days_in_year = 365
          if (mod(yr, 4) == 0) then
            if (mod(yr, 100) /= 0 .or. mod(yr, 400) == 0) days_in_year = 366
          end if
          first = 1
          if (iy == 1) first = max(time%day_start, 1)
          last = days_in_year
          if (iy == time%nbyr .and. time%day_end > 0) last = min(time%day_end, days_in_year)
          total = total + max(last - first + 1, 0)
        end do
      end function count_run_days

      subroutine set_error(message)
        character(len=*), intent(in) :: message
        api_last_error = message
      end subroutine set_error

      subroutine to_c_string(text, buffer, buffer_length)
        character(len=*), intent(in) :: text
        character(kind=c_char), intent(out) :: buffer(*)
        integer, intent(in) :: buffer_length
        integer :: n, i

        n = min(len_trim(text), buffer_length - 1)
        do i = 1, n
          buffer(i) = text(i:i)
        end do
        buffer(n + 1) = c_null_char
      end subroutine to_c_string

      end module swatplus_bmi
