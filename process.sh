#!/bin/bash

function CHECK_PATH {

    STATE=$1

    PATHVAR=$2

    if [[ ($STATE != "EXISTS") && ($STATE != "NOT_EXISTS") ]]; then	
	(>&2 echo "Unknown state: $STATE")

	exit
    fi

    tstamp=`date`

    if [[ $STATE == "EXISTS" && ! -a "$PATHVAR" ]]; then
	MESSAGE="not created / does not exist."

	(>&2 echo "[$tstamp] ${PATHVAR}" $MESSAGE "Exiting.")
	    
	exit
    fi
    
    if [[ $STATE == "NOT_EXISTS" && -a "$PATHVAR" ]]; then
	    MESSAGE="exists."

	    (>&2 echo "[$tstamp] ${PATHVAR} $MESSAGE")

	    while true; do
		read -p "Overwrite? (Y/N)" yn
		case $yn in
		    [Yy]* ) return 1 ;;
		    [Nn]* ) return 2 ;;
		    * ) echo "Please answer yes or no.";;
		esac
	    done

    fi

    return 0
}

function INFO {
    tstamp=`date`

    (>&2 echo "[$tstamp] $1")
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

CHECK_RESULT=$?

if [[ $CHECK_RESULT = 0 || $CHECK_RESULT = 1 ]]; then
    rm -rf ${BACKEND_FILE}

    time ingest $PLATE_DIR -o sqlite:///${BACKEND_FILE} -c ingest_config.ini

fi

CHECK_PATH EXISTS $BACKEND_FILE

INFO "Indexing ${BACKEND_FILE}"

time sqlite3 ${BACKEND_FILE} < indices.sql

INFO "Aggregating ${BACKEND_FILE}"

AGGREGATED_FILE=${BACKEND_DIR}/${PLATE_ID}.csv

CHECK_PATH NOT_EXISTS $AGGREGATED_FILE

CHECK_RESULT=$?

if [[ $CHECK_RESULT == 0 || $CHECK_RESULT == 1 ]]; then
    rm -rf $AGGREGATED_FILE

    time Rscript -e "extends <- methods::extends; source('create_profiles.R')" ${BACKEND_FILE} ${AGGREGATED_FILE}

fi

CHECK_PATH EXISTS $AGGREGATED_FILE
