#!/usr/bin/env bash
# Copyright (c) .NET Foundation and contributors. All rights reserved.
# Licensed under the MIT license. See LICENSE file in the project root for full license information.
#

# Stop script on NZEC
set -e
# Stop script if unbound variable found (use ${var:-} if intentional)
set -u
# By default cmd1 | cmd2 returns exit code of cmd2 regardless of cmd1 success
# This is causing it to fail
set -o pipefail

# Use in the the functions: eval $invocation
invocation='say_verbose "Calling: ${yellow:-}${FUNCNAME[0]} ${green:-}$*${normal:-}"'

# standard output may be used as a return value in the functions
# we need a way to write text on the screen in the functions so that
# it won't interfere with the return value.
# Exposing stream 3 as a pipe to standard output of the script itself
exec 3>&1

# Setup some colors to use. These need to work in fairly limited shells, like the Ubuntu Docker container where there are only 8 colors.
# See if stdout is a terminal
if [ -t 1 ]; then
    # see if it supports colors
    ncolors=$(tput colors)
    if [ -n "$ncolors" ] && [ $ncolors -ge 8 ]; then
        bold="$(tput bold       || echo)"
        normal="$(tput sgr0     || echo)"
        black="$(tput setaf 0   || echo)"
        red="$(tput setaf 1     || echo)"
        green="$(tput setaf 2   || echo)"
        yellow="$(tput setaf 3  || echo)"
        blue="$(tput setaf 4    || echo)"
        magenta="$(tput setaf 5 || echo)"
        cyan="$(tput setaf 6    || echo)"
        white="$(tput setaf 7   || echo)"
    fi
fi

say_err() {
    printf "%b\n" "${red:-}fake-install: Error: $1${normal:-}" >&2
}

say() {
    # using stream 3 (defined in the beginning) to not interfere with stdout of functions
    # which may be used as return value
    printf "%b\n" "${cyan:-}fake-install:${normal:-} $1" >&3
}

say_verbose() {
    if [ "$verbose" = true ]; then
        say "$1"
    fi
}

get_os_download_name_from_platform() {
    eval $invocation

    platform="$1"
    case "$platform" in
        "ubuntu.14.04")
            echo "ubuntu.14.04"
            return 0
            ;;
        "ubuntu.16.04")
            echo "ubuntu.16.04"
            return 0
            ;;
    esac
    return 1
}

get_current_os_name() {
    eval $invocation

    local uname=$(uname)
    if [ "$uname" = "Darwin" ]; then
        echo "osx.10.11"
        return 0
    else
        if [ "$uname" = "Linux" ]; then
            echo "linux"
            return 0
        fi
    fi
    
    say_err "OS name could not be detected: $ID.$VERSION_ID"
    return 1
}

get_distro_specific_os_name() {
    eval $invocation

    local uname=$(uname)
    if [ "$uname" = "Darwin" ]; then
        echo "osx.10.11"
        return 0
    elif [ -n "$runtime_id" ]; then
        echo $(get_os_download_name_from_platform "${runtime_id%-*}" || echo "${runtime_id%-*}")
        return 0
    else
        if [ -e /etc/os-release ]; then
            . /etc/os-release
            os=$(get_os_download_name_from_platform "$ID.$VERSION_ID" || echo "")
            if [ -n "$os" ]; then
                echo "$os"
                return 0
            fi
        fi
    fi
    
    say_err "OS name could not be detected: $ID.$VERSION_ID"
    return 1
}

machine_has() {
    eval $invocation

    hash "$1" > /dev/null 2>&1
    return $?
}

check_min_reqs() {
    local hasMinimum=false
    if machine_has "curl"; then
        hasMinimum=true
    elif machine_has "wget"; then
        hasMinimum=true
    fi

    if [ "$hasMinimum" = "false" ]; then
        say_err "curl (recommended) or wget are required to download fake. Install missing prerequisite to proceed."
        return 1
    fi
    return 0
}

check_pre_reqs() {
    eval $invocation
    
    local failing=false;

    if [ "${FAKE_INSTALL_SKIP_PREREQS:-}" = "1" ]; then
        return 0
    fi

    if [ "$(uname)" = "Linux" ]; then
        if ! [ -x "$(command -v ldconfig)" ]; then
            echo "ldconfig is not in PATH, trying /sbin/ldconfig."
            LDCONFIG_COMMAND="/sbin/ldconfig"
        else
            LDCONFIG_COMMAND="ldconfig"
        fi

        [ -z "$($LDCONFIG_COMMAND -p | grep libunwind)" ] && say_err "Unable to locate libunwind. Install libunwind to continue" && failing=true
        [ -z "$($LDCONFIG_COMMAND -p | grep libssl)" ] && say_err "Unable to locate libssl. Install libssl to continue" && failing=true
        [ -z "$($LDCONFIG_COMMAND -p | grep libicu)" ] && say_err "Unable to locate libicu. Install libicu to continue" && failing=true
    fi

    if [ "$failing" = true ]; then
       return 1
    fi
    
    return 0
}

# args:
# input - $1
to_lowercase() {
    #eval $invocation
    
    echo "$1" | tr '[:upper:]' '[:lower:]'
    return 0
}

# args:
# input - $1
remove_trailing_slash() {
    #eval $invocation
    
    local input=${1:-}
    echo "${input%/}"
    return 0
}

# args:
# input - $1
remove_beginning_slash() {
    #eval $invocation
    
    local input=${1:-}
    echo "${input#/}"
    return 0
}

# args:
# root_path - $1
# child_path - $2 - this parameter can be empty
combine_paths() {
    eval $invocation
    
    # TODO: Consider making it work with any number of paths. For now:
    if [ ! -z "${3:-}" ]; then
        say_err "combine_paths: Function takes two parameters."
        return 1
    fi
    
    local root_path=$(remove_trailing_slash $1)
    local child_path=$(remove_beginning_slash ${2:-})
    say_verbose "combine_paths: root_path=$root_path"
    say_verbose "combine_paths: child_path=$child_path"
    echo "$root_path/$child_path"
    return 0
}

get_machine_architecture() {
    eval $invocation
    
    # Currently the only one supported
    echo "x64"
    return 0
}

# args:
# architecture - $1
get_normalized_architecture_from_architecture() {
    eval $invocation
    
    local architecture=$(to_lowercase $1)
    case $architecture in
        \<auto\>)
            echo "$(get_normalized_architecture_from_architecture $(get_machine_architecture))"
            return 0
            ;;
        amd64|x64)
            echo "x64"
            return 0
            ;;
        x86)
            say_err "Architecture \`x86\` currently not supported"
            return 1
            ;;
    esac
   
    say_err "Architecture \`$architecture\` not supported. If you think this is a bug, please report it at https://github.com/dotnet/cli/issues"
    return 1
}

# version_info is a conceptual two line string representing commit hash and 4-part version
# format:
# Line 1: # commit_hash
# Line 2: # 4-part version

# args:
# version_text - stdin
get_version_from_version_info() {
    eval $invocation
    
    cat | tail -n 1
    return 0
}

# args:
# version_text - stdin
get_commit_hash_from_version_info() {
    eval $invocation
    
    cat | head -n 1
    return 0
}

# args:
# install_root - $1
is_fake_package_installed() {
    eval $invocation
    
    local install_root=$1
    
    if [ -d "$install_root" ]; then
        return 0
    else
        return 1
    fi
}

# args:
# repo - $1
# normalized_architecture - $2
get_latest_version_info() {
    eval $invocation
    
    local github_repo=$1
    local normalized_architecture=$2

    local releases_url="https://api.github.com/repos/$github_repo/releases"
    say_verbose "get_latest_version_info: releases url: $releases_url"
    
    download $releases_url | jq -r '.[0].tag_name'
    return $?
}

# args:
# repo - $1
# normalized_architecture - $2
# version - $3
get_specific_version_from_version() {
    eval $invocation
    
    local github_repo=$1
    local normalized_architecture=$2
    local version=$(to_lowercase $3)

    case $version in
        latest)
            local version_info
            version_info="$(get_latest_version_info $github_repo $normalized_architecture)" || return 1
            say_verbose "get_specific_version_from_version: version_info=$version_info"
            echo "$version_info" | get_version_from_version_info
            return 0
            ;;
        *)
            echo $version
            return 0
            ;;
    esac
}

# args:
# repo - $1
# normalized_architecture - $2
# specific_version - $3
construct_download_link() {
    eval $invocation
    
    local github_repo=$1
    local normalized_architecture=$2
    local specific_version=${3//[$'\t\r\n']}
    
    local osname
    osname=$(get_current_os_name) || return 1
    say_verbose "osname=$osname"

    local download_link=$(download "https://api.github.com/repos/$github_repo/releases/tags/$specific_version" | jq -r ".assets | .[] | select(.name | endswith(\"$osname-$normalized_architecture.zip\")) | .browser_download_url")
    
    echo "$download_link"
    return 0
}

# args:
# repo - $1
# normalized_architecture - $2
# specific_version - $3
construct_alt_download_link() {
    eval $invocation
    
    local github_repo=$1
    local normalized_architecture=$2
    local specific_version=${3//[$'\t\r\n']}
    
    local distro_specific_osname
    distro_specific_osname=$(get_distro_specific_os_name) || return 1
    say_verbose "distro_specific_osname=$distro_specific_osname"

    local alt_download_link=$(download "https://api.github.com/repos/$github_repo/releases/tags/$specific_version" | jq -r ".assets | .[] | select(.name | endswith(\"$distro_specific_osname-$normalized_architecture.zip\")) | .browser_download_url")
    
    echo "$alt_download_link"
    return 0
}

# args:
# specific_version - $1
get_user_install_path() {
    eval $invocation
    
    local specific_version=$1
    if [ ! -z "${FAKE_INSTALL_DIR:-}" ]; then
        echo $FAKE_INSTALL_DIR
    else
        echo "$HOME/.fake/sdk/$specific_version"
    fi
    return 0
}

# args:
# install_dir - $1
# specific_version - $2
resolve_installation_path() {
    eval $invocation
    
    local install_dir=$1
    local specific_version=$2
    if [ "$install_dir" = "<auto>" ]; then
        local user_install_path=$(get_user_install_path $specific_version)
        say_verbose "resolve_installation_path: user_install_path=$user_install_path"
        echo "$user_install_path"
        return 0
    fi
    
    echo "$install_dir"
    return 0
}


# args:
# install_root - $1
get_installed_version_info() {
    eval $invocation
    
    local install_root=$1
    local version_file=$(combine_paths "$install_root" "$local_version_file_relative_path")
    say_verbose "Local version file: $version_file"
    if [ ! -z "$version_file" ] | [ -r "$version_file" ]; then
        local version_info="$(cat $version_file)"
        echo "$version_info"
        return 0
    fi
    
    say_verbose "Local version file not found."
    return 0
}

# args:
# relative_or_absolute_path - $1
get_absolute_path() {
    eval $invocation
    
    local relative_or_absolute_path=$1
    echo $(cd $(dirname "$1") && pwd -P)/$(basename "$1")
    return 0
}

# args:
# zip_path - $1
# out_path - $2
extract_fake_package() {
    eval $invocation
    
    local zip_path=$1
    local out_path=$2
    
    local temp_out_path=$(mktemp -d $temporary_file_template)
    
    local failed=false
    say_verbose "unzip -a $zip_path -d $temp_out_path"
    unzip -a "$zip_path" -d "$temp_out_path" > /dev/null || failed=true
    
    say_verbose "cp -r $temp_out_path/ $out_path/"
    cp -r "$temp_out_path/" "$out_path"
    ls -la "$temp_out_path" >&3
    ls -la "$out_path" >&3
    rm -rf $temp_out_path
    
    if [ "$failed" = true ]; then
        say_err "Extraction failed"
        return 1
    fi
}

# args:
# remote_path - $1
# [out_path] - $2 - stdout if not provided
download() {
    eval $invocation

    local remote_path=$1
    local out_path=${2:-}

    local failed=false
    # Restart the request up to N times if it fails
    local retries=3
    # Give up after 120 seconds of retrying
    local retry_max_time=120
    if machine_has "curl"; then
        downloadcurl $remote_path $out_path || failed=true
    elif machine_has "wget"; then
        downloadwget $remote_path $out_path || failed=true
    else
        failed=true
    fi
    if [ "$failed" = true ]; then
        say_verbose "Download failed: $remote_path"
        return 1
    fi
    return 0
}

downloadcurl() {
    eval $invocation
    local remote_path=$1
    local out_path=${2:-}

    local failed=false
    if [ -z "$out_path" ]; then
        curl --fail --retry $retries --retry-max-time $retry_max_time -sSL $remote_path \
             || wget -qO- $remote_path \
             || failed=true
    else
        curl --fail --retry $retries --retry-max-time $retry_max_time -sSL -o $out_path $remote_path \
             || wget -qO $out_path $remote_path \
             || failed=true
    fi
    if [ "$failed" = true ]; then
        say_verbose "Curl download failed"
        return 1
    fi
    return 0
}

downloadwget() {
    eval $invocation
    local remote_path=$1
    local out_path=${2:-}

    local failed=false
    if [ -z "$out_path" ]; then
        wget -q --tries 10 $remote_path || failed=true
    else
        wget -v --tries 10 -O $out_path $remote_path || failed=true
    fi
    if [ "$failed" = true ]; then
        say_verbose "Wget download failed"
        return 1
    fi
    return 0
}

calculate_vars() {
    eval $invocation
    
    normalized_architecture=$(get_normalized_architecture_from_architecture "$architecture")
    say_verbose "normalized_architecture=$normalized_architecture"
    
    specific_version=$(get_specific_version_from_version $github_repo $normalized_architecture $version)
    say_verbose "specific_version=$specific_version"
    if [ -z "$specific_version" ]; then
        say_err "Could not get version information."
        return 1
    fi
    
    download_link=$(construct_download_link $github_repo $normalized_architecture $specific_version)
    say_verbose "download_link=$download_link"

    if [ "$(uname)" = "Linux" ]; then
        alt_download_link=$(construct_alt_download_link $github_repo $normalized_architecture $specific_version)
        say_verbose "alt_download_link=$alt_download_link"
    fi

    install_root=$(resolve_installation_path $install_dir $specific_version)
    say_verbose "install_root=$install_root"
}

install_fake() {
    eval $invocation
    local download_failed=false

    if is_fake_package_installed $install_root; then
        say "FAKE SDK version $specific_version is already installed."
        return 0
    fi
    
    mkdir -p $install_root
    zip_path=$(mktemp $temporary_file_template)
    say_verbose "Zip path: $zip_path"

    say "Downloading link: $download_link"
    download "$download_link" $zip_path || download_failed=true

    #  if the download fails, download the alt_download_link [Linux only]
    if [ "$(uname)" = "Linux" ] && [ "$download_failed" = true ]; then
        say "Cannot download: $download_link"
        zip_path=$(mktemp $temporary_file_template)
        say_verbose "Alternate zip path: $zip_path"
        say "Downloading alternate link: $alt_download_link"
        download "$alt_download_link" $zip_path
    fi
    
    say "Extracting zip"
    extract_fake_package $zip_path $install_root
    chmod +x "$install_root/Fake"
    
    return 0
}

local_version_file_relative_path="/.version"
bin_folder_relative_path=""
temporary_file_template="${TMPDIR:-/tmp}/fake.XXXXXXXXX"

github_repo="fsharp/FAKE"
version="Latest"
install_dir="<auto>"
architecture="<auto>"
dry_run=false
no_path=false
verbose=true
runtime_id=""

while [ $# -ne 0 ]
do
    name=$1
    case $name in
        -r|--repo)
            shift
            github_repo=$1
            ;;
        -v|--version)
            shift
            version="$1"
            ;;
        -i|--install-dir)
            shift
            install_dir="$1"
            ;;
        --arch|--architecture)
            shift
            architecture="$1"
            ;;
        --dry-run)
            dry_run=true
            ;;
        --no-path)
            no_path=true
            ;;
        --verbose)
            verbose=true
            ;;
        -?|--?|-h|--help)
            script_name="$(basename $0)"
            echo "FAKE Installer"
            echo "Usage: $script_name [-r|--repo <GITHUB_REPO>] [-v|--version <VERSION>]"
            echo "       $script_name -h|-?|--help"
            echo ""
            echo "$script_name is a simple command line interface for obtaining fake cli."
            echo ""
            echo "Options:"
            echo "  -r,--repo <CHANNEL>            Download from the CHANNEL specified (default: $github_repo)."
            echo "  -v,--version <VERSION>         Use specific version, or \`latest\`. Defaults to \`latest\`."
            echo "  -i,--install-dir <DIR>         Install under specified location (see Install Location below)"
            echo "  --architecture <ARCHITECTURE>  Architecture of .NET Tools. Currently only x64 is supported."
            echo "  --dry-run                      Do not perform installation. Display download link."
            echo "  --no-path                      Do not set PATH for the current process."
            echo "  --verbose                      Display diagnostics information."
            echo "  -?,--?,-h,--help               Shows this help message"
            echo ""
            echo "Install Location:"
            echo "  Location is chosen in following order:"
            echo "    - --install-dir option"
            echo "    - Environmental variable FAKE_INSTALL_DIR"
            echo "    - $HOME/.fake"
            exit 0
            ;;
        *)
            say_err "Unknown argument \`$name\`"
            exit 1
            ;;
    esac

    shift
done

check_min_reqs
calculate_vars
if [ "$dry_run" = true ]; then
    say "Payload URL: $download_link"
    if [ "$(uname)" = "Linux" ]; then
        say "Alternate payload URL: $alt_download_link"
    fi
    say "Repeatable invocation: ./$(basename $0) --version $specific_version --repo $github_repo --install-dir $install_dir"
    exit 0
fi

check_pre_reqs
install_fake

bin_path=$(get_absolute_path $(combine_paths $install_root $bin_folder_relative_path))
if [ "$no_path" = false ]; then
    say "Adding to current process PATH: \`$bin_path\`. Note: This change will be visible only when sourcing script."
    export PATH=$bin_path:$PATH
else
    say "Binaries of dotnet can be found in $bin_path"
fi

say "Installation finished successfully."