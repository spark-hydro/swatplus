"""Check of libswatplus_api (Windows DLL, macOS dylib or Linux so): the library loads and exports every function and variable that the Python driver (xmipy) needs."""
import ctypes
import glob
import os
import sys

NAMES = ["BMI_LENVERSION", "BMI_LENGRIDTYPE", "BMI_LENCOMPONENTNAME", "BMI_LENVARADDRESS", "BMI_LENVARTYPE", "BMI_LENERRMESSAGE",
         "initialize", "update", "finalize", "get_start_time", "get_end_time", "get_current_time", "get_time_step", "get_component_name",
         "get_version", "get_last_bmi_error", "get_var_rank", "get_var_type", "get_var_shape", "get_value_ptr", "set_value",
         "mf6swatp_update_land", "mf6swatp_update_routing"]

dll = sys.argv[1] if len(sys.argv) > 1 else sorted(glob.glob("build/*swatplus_api*.dll") + glob.glob("build/*swatplus_api*.dylib") + glob.glob("build/*swatplus_api*.so"))[0]
print("library:", dll)
for d in os.environ.get("EXTRA_DLL_DIRS", "").split(os.pathsep):
    if d and os.path.isdir(d) and hasattr(os, "add_dll_directory"):
        os.add_dll_directory(d)
lib = ctypes.CDLL(os.path.abspath(dll))
print("library loads")
missing = [n for n in NAMES if not hasattr(lib, n)]
print("exports missing:", missing if missing else "none")
sys.exit(1 if missing else 0)
