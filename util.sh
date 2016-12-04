function CHECK_PATH {

    STATE=$1

    PATHVAR=$2

    if [[ ($STATE != "EXISTS") && ($STATE != "NOT_EXISTS") ]]; then	
	(>&2 echo "Unknown state: $STATE")

	exit 1
    fi

    tstamp=`date`

    if [[ $STATE == "EXISTS" && ! -a "$PATHVAR" ]]; then
	MESSAGE="not created / does not exist."

	(>&2 echo "[$tstamp] ${PATHVAR}" $MESSAGE "Exiting.")
	    
	exit 1
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

function COMPARE_MD5() {

    A=$1

    B=$2

    MD5_A=`md5sum ${A} | cut -f1 -d' '`

    MD5_B=`md5sum ${B} | cut -f1 -d' '`
    
    if [[ $MD5_A != $MD5_B ]]; then
	
	tstamp=`date`
	
	(>&2 echo "[$tstamp] md5 check failed: ${A} and ${B} are different")

	exit 1

    fi
}

function CREATE_AND_CHECK_DIR () {

    DNAME=$1

    mkdir -p ${DNAME}

    CHECK_PATH EXISTS ${DNAME}
    
    echo `readlink -e $DNAME`
}

function CHECK_CMD_EXISTS () {

    CMDSTR=$1

    eval type $CMDSTR >/dev/null 2>&1 || { 

	echo >&2 "$CMDSTR not installed.  Aborting."

	exit 1
    }

}
