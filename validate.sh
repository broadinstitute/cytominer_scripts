#!/bin/bash
source ./util.sh

programname=$0

while [[ $# -gt 1 ]]
do
key="$1"

case $key in
    -b|--batchid)
    BATCH_ID="$2"
    shift
    ;;
    -p|--plate)
    PLATE_ID="$2"
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

for var in BATCH_ID PLATE_ID TMP_DIR;
do 
    if [[  -z "${!var}"  ]];
    then
        echo ${var} not defined.
        exit 1
    fi
done

BACKEND_ARCHIVE_DIR="../../backend/${BATCH_ID}/${PLATE_ID}/"

BACKEND_ARCHIVE_FILE=${BACKEND_ARCHIVE_DIR}/${PLATE_ID}.sqlite

AGGREGATED_ARCHIVE_FILE=${BACKEND_ARCHIVE_DIR}/${PLATE_ID}.csv

AGGREGATED_WITH_METADATA_ARCHIVE_FILE=${BACKEND_ARCHIVE_DIR}/${PLATE_ID}_augmented.csv

CHECK_PATH EXISTS $BACKEND_ARCHIVE_FILE
    
CHECK_PATH EXISTS $AGGREGATED_ARCHIVE_FILE
    
CHECK_PATH EXISTS $AGGREGATED_WITH_METADATA_ARCHIVE_FILE
   

