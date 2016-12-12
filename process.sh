#!/bin/bash
source ./util.sh

if [[ $OSTYPE == "darwin15" ]]; then
    shopt -s expand_aliases

    alias readlink="greadlink"

    alias md5sum="md5"
fi

function create_backend_file() {

    info "Creating ${backend_file}"

    check_path not_exists $backend_file

    check_result=$?

    if [[ $check_result = 0 || $check_result = 1 ]]; then
	rm -rf ${backend_file}

	time ingest $plate_dir -o sqlite:///${backend_file} -c ingest_config.ini --no-munge
    fi

    check_path exists $backend_file

    info "Indexing ${backend_file}"

    time sqlite3 ${backend_file} < indices.sql

}

function create_aggregated_file () {

    info "Aggregating ${backend_file}"

    check_path not_exists $aggregated_file

    check_result=$?

    if [[ $check_result == 0 || $check_result == 1 ]]; then
	rm -rf $aggregated_file

	time ./create_profiles.R ${backend_file} -o ${aggregated_file}

    fi

    check_path exists $aggregated_file
}

function archive_backend_file () {

    info "Moving ${backend_file} to ${backend_archive_file}"

    rsync -a ${backend_file} ${backend_archive_file}

    compare_md5 ${backend_file} ${backend_archive_file}

    rm ${backend_file}

}

function archive_aggregated_file () {

    info "Moving ${aggregated_file} to ${aggregated_archive_file}"

    rsync -a ${aggregated_file} ${aggregated_archive_file}

    compare_md5 ${aggregated_file} ${aggregated_archive_file}

    rm ${aggregated_file}
}

programname=$0

ingest --help >/dev/null 2>&1 || { echo >&2 "ingest required but not installed. Exiting."; exit 1; }

while [[ $# -gt 1 ]]
do
key="$1"

case $key in
    -b|--batchid)
    batch_id="$2"
    shift
    ;;
    -p|--plate)
    plate_id="$2"
    shift
    ;;
    -t|--tmpdir)
    tmp_dir="$2"
    shift
    ;;
    *)
    echo Unknown option
    ;;
esac
shift
done

tmp_dir="${tmp_dir:-/tmp}"

pipeline="${pipeline:-analysis}"

for var in batch_id pipeline plate_id tmp_dir;
do 
    if [[  -z "${!var}"  ]];
    then
        echo ${var} not defined.
        exit 1
    fi
done

plate_dir=`readlink -e ../../analysis/${batch_id}/${plate_id}/${pipeline}/`

backend_dir=${tmp_dir}/${batch_id}/${plate_id}/

backend_file=${backend_dir}/${plate_id}.sqlite

aggregated_file=${backend_dir}/${plate_id}.csv

backend_archive_dir="../../backend/${batch_id}/${plate_id}/"

backend_archive_file=${backend_archive_dir}/${plate_id}.sqlite

aggregated_archive_file=${backend_archive_dir}/${plate_id}.csv

aggregated_with_metadata_archive_file=${backend_archive_dir}/${plate_id}_augmented.csv

if [[ (! -e $backend_archive_file) || (! -e $aggregated_archive_file) ]]; then

    check_path exists ${plate_dir}
    
    backend_dir=$(create_and_check_dir $backend_dir)

    create_backend_file

    create_aggregated_file

    backend_archive_dir=$(create_and_check_dir $backend_archive_dir)

    archive_backend_file

    archive_aggregated_file

else
    
    info "$backend_archive_file and $aggregated_archive_file already exist. Will not recreate them."

fi

# join with metadata

./join_metadata.R -b $batch_id -p $plate_id

check_path exists $aggregated_with_metadata_archive_file

