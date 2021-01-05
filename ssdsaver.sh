#!/bin/bash

# check `work dir`
DIST_DIR_PATH=$(stack path --dist-dir)
WORK_DIR=${DIST_DIR_PATH%%/*}
REAL_WORK_DIR=$(readlink -f ${WORK_DIR})

# clear `work dir`
stack clean --full

# clear real `work dir`
if [ -d "${REAL_WORK_DIR}" ]; then
    REAL_WORK_DIR_LS=$(ls -A ${REAL_WORK_DIR})
    echo "real work dir ls ${REAL_WORK_DIR_LS}"
    echo "real work dir ${REAL_WORK_DIR}"
    if [ -z "${REAL_WORK_DIR_LS}" ]; then
        echo "rm real work dir ${REAL_WORK_DIR}"
        rm -r ${REAL_WORK_DIR}
    fi
fi

# make tmp `work dir`
NEW_TMP_WORK_DIR=$(mktemp -d)

echo "work dir ${WORK_DIR}"
if [ -d "${WROK_DIR}" ]; then
    WORK_DIR_LS=$(ls -A ${WORK_DIR})
    if [ -z "${WORK_DIR_LS}" ]; then
        echo "rm work dir what is not link ${WORK_DIR}"
        rm -r ${WORK_DIR}
    fi
elif [ -L "${WORK_DIR}" ]; then
    echo "rm work dir what is link"
    rm ${WORK_DIR}
fi

ln -s ${NEW_TMP_WORK_DIR} ${WORK_DIR}
