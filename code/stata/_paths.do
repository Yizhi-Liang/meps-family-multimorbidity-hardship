/*
  _paths.do — resolves $proj, $data, $code, $rawdata, $rscript.
  $proj: env MEPS_PROTECTION_PROJ, else cwd (must contain code/00_master.do).
  $rawdata: env MEPS_RAWDATA (required). $rscript: env MEPS_RSCRIPT or `Rscript` on $PATH.
*/

if "$proj" == "" {

    local proj_env : env MEPS_PROTECTION_PROJ
    if "`proj_env'" != "" {
        global proj "`proj_env'"
    }
    else {
        capture confirm file "`c(pwd)'/code/00_master.do"
        if _rc == 0 {
            global proj "`c(pwd)'"
        }
        else {
            di as error "ERROR: cannot resolve project root."
            di as error "       Either set MEPS_PROTECTION_PROJ or cd to the"
            di as error "       repository root (directory containing code/00_master.do)."
            exit 601
        }
    }

    global data "$proj/data"
    global code "$proj/code"

    local rawdata_env : env MEPS_RAWDATA
    if "`rawdata_env'" != "" {
        global rawdata "`rawdata_env'"
    }
    else {
        di as error "ERROR: environment variable MEPS_RAWDATA is required."
        di as error "       Set it to the directory containing your MEPS PUFs:"
        di as error "         export MEPS_RAWDATA=/path/to/your/meps/downloads"
        exit 601
    }

    di as text "[_paths.do] resolved proj    = $proj"
    di as text "[_paths.do] resolved rawdata = $rawdata"
}

if "$rscript" == "" {
    local rscript_env : env MEPS_RSCRIPT
    if "`rscript_env'" != "" {
        global rscript "`rscript_env'"
    }
    else {
        global rscript "Rscript"
    }
}
