#!/bin/bash   

# Trap to handle shell script errors 
trap 'error_handler' ERR
error_handler() {
    error_time=$(date '+%Y-%m-%d %H:%M:%S')
    echo -e "\n\033[0;31mWarning\033[0m: make.sh failed at ${error_time}. Check above for details." # display warning in terminal
    exit 1 # early exit with error code
}

# Set paths
# (Make sure REPO_ROOT is set to point to the root of the repository!)
MAKE_SCRIPT_DIR="$(cd "$(dirname -- "$0")" && pwd -P)"
REPO_ROOT="$(cd "$MAKE_SCRIPT_DIR/../" && pwd -P)"
MODULE=$(basename "$MAKE_SCRIPT_DIR")
LOGFILE="${MAKE_SCRIPT_DIR}/output/make.log"

# Check setup
source "${REPO_ROOT}/lib/shell/check_setup.sh"

# Tell user what we're doing
echo -e "\n\nMaking module \033[35m${MODULE}\033[0m with shell ${SHELL}"

# Load settings & tools
source "${REPO_ROOT}/local_env.sh"
source "${REPO_ROOT}/lib/shell/run_shell.sh"
source "${REPO_ROOT}/lib/shell/run_R.sh"
source "${REPO_ROOT}/lib/shell/run_python.sh"


# Clear output directory
# (Guarantees that all output is produced from a clean run of the code)
rm -rf "${MAKE_SCRIPT_DIR}/output"
mkdir -p "${MAKE_SCRIPT_DIR}/output"

# Add symlink input files to local /input/ directory
# (Make sure get_inputs.sh is updated to pull in all needed input files!)
(   cd ${MAKE_SCRIPT_DIR}
    source "${MAKE_SCRIPT_DIR}/get_inputs.sh"
)

# Run scripts
# (Do this in a subshell so we return to the original working directory
# after scripts are run)
 echo -e "\nmake.sh started at $(date '+%Y-%m-%d %H:%M:%S')"

(
cd "${MAKE_SCRIPT_DIR}/source"

# YEARS=({2007..2020})
# QUARTERS=(T1 T2 T3 T4)

# for year in "${YEARS[@]}"; do
#   for quarter in "${QUARTERS[@]}"; do
#     echo -e "\nProcessing ${year} ${quarter}..."
#     run_R make_quarterly_enoe.r "${LOGFILE}" "$year" "$quarter" || exit 1
#     echo -e "\nFinished Processing ${year} ${quarter}..."
#   done
# done

# for ((i=1; i<=245; i+=5)); do
#   cohort_number=$(( (i-1) / 5 + 1))
#   percent=$(( 100 * $cohort_number / 49 ))
#   echo -e "\nProcessing cohort $cohort_number of 49 ... ($percent% done)"
#   run_R make_cohorts_enoe.r "${LOGFILE}" "$i" || exit 1
#   echo -e "\nFinished processing cohort $cohort_number of 49."
#   echo -e "\nSaved cohort data as Cohort_$cohort_number to Output folder."
# done

# for ((i=1; i<=49; i+=1)); do
#      percent_2=$(( 100 * $i / 49 ))
#    echo -e "\nEditing cohort $i of 49 ... ($percent_2% done)"
#    run_R edit_cohorts_enoe.r "${LOGFILE}" "$i" || exit 1
#    echo -e "\nFinished Editing cohort $i of 49. Saved as CleanCohort_$i in Output folder."
# done

#run_R crime_and_pop.r "${LOGFILE}" || exit 1

run_python staple_cohorts.py "${LOGFILE}" || exit 1

) || false

echo -e "\nmake.sh finished at $(date '+%Y-%m-%d %H:%M:%S')" | tee -a "${LOGFILE}"

