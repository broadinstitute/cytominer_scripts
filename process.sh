#!/bin/bash

function CHECK_PATH(){
    STATE=$1

    PATH=$2

    if [[ (STATE == "EXISTS" && ! -a "$PATH") || (STATE == "NOT_EXISTS" && -a "$PATH") ]]; then

	tstamp=`date`

	if [[ STATE == "EXISTS" ]]; then 
	    MESSAGE="not created / does not exist."

	elif [[ STATE == "NOT_EXISTS" ]]; then
	    MESSAGE="exists."

	else
	    (>&2 echo "Unknown state: $STATE")

	    exit

	fi

	(>&2 echo "[$tstamp] ${PATH}" $MESSAGE "Exiting.")

	exit
    fi
}

function INFO() {
    tstamp=`date`

    (>&2 echo "[$tstamp] Ingesting $1")
}

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

CHECK_PATH EXISTS ${PLATE_DIR}

BACKEND_DIR=${TMP_DIR}/${BATCH_ID}/${PLATE_ID}/

mkdir -p $BACKEND_DIR

BACKEND_FILE=${BACKEND_DIR}/${PLATE_ID}.sqlite

CHECK_PATH NOT_EXISTS $BACKEND_FILE

time ingest $PLATE_DIR -o sqlite:///${BACKEND_FILE} -c ingest_config.ini

CHECK_PATH EXISTS $BACKEND_FILE

INFO "Indexing ${BACKEND_FILE}"

time sqlite3 ${BACKEND_FILE} < indices.sql

INFO "Aggregating ${BACKEND_FILE}"

AGGREGATED_FILE=${BACKEND_DIR}/${PLATE_ID}.csv

CHECK_PATH NOT_EXISTS $AGGREGATED_FILE

time Rscript -e "extends <- methods::extends; source('create_profiles.R')" ${BACKEND_FILE} ${AGGREGATED_FILE}

CHECK_PATH EXISTS $AGGREGATED_FILE


