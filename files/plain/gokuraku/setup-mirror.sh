#!/bin/sh
# SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
#
# SPDX-License-Identifier: CC0-1.0

if [ "x$BASH_VERSION" = "x" ]; then
    exec bash "$0" "$@"
fi

die()
{
    echo "ERROR: $*"
    exit 1
}

PATH=/run/current-system/profile/bin
RCLONE_ARGS="\
    --config /run/secrets/rclone \
    --verbose --size-only --no-traverse \
    --s3-no-check-bucket --s3-upload-concurrency=8 --s3-chunk-size=16M"

NARINFO_OLD=/var/cache/r2/narinfo-old.txt
NARINFO_NEW=/var/cache/r2/narinfo-new.txt
NARINFO_DEL=/var/cache/r2/narinfo-to-delete.txt
NARINFO_CPY=/var/cache/r2/narinfo-to-copy.txt

NAR_OLD=/var/cache/r2/nar-old.txt
NAR_NEW=/var/cache/r2/nar-new.txt
NAR_DEL=/var/cache/r2/nar-to-delete.txt
NAR_CPY=/var/cache/r2/nar-to-copy.txt

list_narinfo() {
    pushd /var/cache/guix/publish
    fd '\.narinfo$' --type f | sort > $NARINFO_NEW
    popd

    pushd /var/cache/r2
    diff --unified $NARINFO_OLD $NARINFO_NEW | tail +4 > narinfo.diff
    rg '^-'  narinfo.diff --no-line-number --replace '' > $NARINFO_DEL
    rg '^\+' narinfo.diff --no-line-number --replace '' > $NARINFO_CPY
    rm --force narinfo.diff
    popd
}

list_nar() {
    pushd /var/cache/guix/publish/nar
    fd --type f --exclude '*.tmp' | sort > $NAR_NEW
    popd

    pushd /var/cache/r2
    diff --unified $NAR_OLD $NAR_NEW | tail +4 > nar.diff
    rg '^-'  nar.diff --no-line-number --replace '' > $NAR_DEL
    rg '^\+' nar.diff --no-line-number --replace '' > $NAR_CPY
    rm --force nar.diff
    popd
}

delete_narinfo() {
    pushd /var/cache/guix-moe
    old_IFS=$IFS
    IFS=""
    while read -r file ; do
        rm --force "$file"
    done < $NARINFO_DEL
    IFS=${old_IFS}
    popd
}

copy_narinfo() {
    pushd /var/cache/guix/publish
    old_IFS=$IFS
    IFS=""
    while read -r file ; do
        cp --archive --force "$file" /var/cache/guix-moe
    done < $NARINFO_CPY
    IFS=${old_IFS}
    popd
}

delete_nar() {
    [[ -s $NAR_DEL ]] && rclone delete $RCLONE_ARGS --files-from $NAR_DEL r2:substitutes-apac/nar
}

upload_nar() {
    [[ -s $NAR_CPY ]] && rclone copy   $RCLONE_ARGS --files-from $NAR_CPY /var/cache/guix/publish/nar r2:substitutes-apac/nar
}

main() {
    [[ -e $NARINFO_NEW || -e $NAR_NEW ]] && die "previous syncing process unfinished"

    mkdir --parents /var/cache/r2 /var/cache/guix-moe
    touch $NARINFO_OLD $NAR_OLD

    list_narinfo
    list_nar

    delete_narinfo
    upload_nar
    copy_narinfo
    delete_nar

    rm --force $NARINFO_DEL $NARINFO_CPY $NAR_DEL $NAR_CPY
    mv --force $NARINFO_NEW $NARINFO_OLD
    mv --force $NAR_NEW $NAR_OLD
}

main
