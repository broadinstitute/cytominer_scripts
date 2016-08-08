#!/bin/bash

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

PLATE_DIR=`readlink -e ../../analysis/${BATCH_ID}/${PLATE_ID}/`

if [[ -z ${PLATE_DIR} ]];
then
    (>&2 echo "$PLATE_ID does not exist. Exiting.")
    exit
fi

date

(>&2 echo "Ingesting $PLATE_ID")

BACKEND_DIR=${TMP_DIR}/${BATCH_ID}/${PLATE_ID}/

mkdir -p $BACKEND_DIR

BACKEND_FILE=${BACKEND_DIR}/${PLATE_ID}.sqlite

time ingest $PLATE_DIR -o sqlite:///${BACKEND_FILE} -c ingest_config.ini

BACKEND_FILE_OUT=`readlink -e $BACKEND_FILE`

if [[ -z ${BACKEND_FILE_OUT} ]];
then
    (>&2 echo "${BACKEND_FILE} not created. Exiting.")

    exit
fi

date

(>&2 echo "Indexing ${BACKEND_FILE}")

time sqlite3 ${BACKEND_FILE} < indices.sql

date

(>&2 echo "Aggregating ${BACKEND_FILE}")

AGGREGATED_FILE=${BACKEND_DIR}/${PLATE_ID}.csv

time Rscript create_profiles.R ${BACKEND_FILE} ${AGGREGATED_FILE}

date
