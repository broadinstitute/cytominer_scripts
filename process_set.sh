#!/bin/bash
source ./util.sh

programname=$0

ingest --help >/dev/null 2>&1 || { echo >&2 "ingest required but not installed. Exiting."; exit 1; }

while [[ $# -gt 1 ]]
do
key="$1"

case $key in
    -b|--batchid)
    BATCH_ID="$2"
    shift
    ;;
    -s|--setid)
    SET_ID="$2"
    shift
    ;;
    -t|--tmpdir)
    TMP_DIR="$2"
    shift
    ;;
    *)
    echo Unknown option
    ;;
esac
shift
done

TMP_DIR="${TMP_DIR:-/tmp}"

for var in BATCH_ID SET_ID TMP_DIR;
do 
    if [[  -z "${!var}"  ]];
    then
        echo ${var} not defined.
        exit 1
    fi
done

PLATES=`csvjoin -c Plate_Map_Name ../../metadata/${BATCH_ID}/barcode_platemap.csv <(printf "Plate_Map_Name\n${SET_ID}\n")|csvcut -c Assay_Plate_Barcode|tail -n +2|tr '\n' ' '`

echo Running process on $PLATES

tmux new-session -d -s s${SET_ID} "parallel --dryrun --results ../../scratch/${SET_ID} ./process.sh --batchid 2016_04_01_a549_48hr_batch1 --plate {1} --tmpdir ~/tmp ::: ${PLATES}; bash -i"
