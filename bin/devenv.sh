#!/usr/bin/env sh
set -u

SCRIPT_DIR=$(cd -- "$(dirname "${0}")" && pwd)
REPO_DIR="${SCRIPT_DIR}/.."
LIMA_TEMPLATE_PATH="${REPO_DIR}/lima.yaml"
LIMA_BASE_VM_NAME=devenv
LIMA_DEFAULT_VM_NAME=$(basename "$PWD")

devenv_status() {
    limactl list -l ".name == \"${1}\"" --yq .status
}

devenv_stop() {
    limactl stop "${LIMA_BASE_VM_NAME}-${1}" -y
}

devenv_delete() {
    limactl delete "${LIMA_BASE_VM_NAME}-${1}" -y
}

devenv_clone() {
    _vm_name=${1}
    _base_vm_status=$(devenv_status "${LIMA_BASE_VM_NAME}")

    if [ -z "${_base_vm_status:-}" ]; then
        # init from scratch
        limactl create "--name=${LIMA_BASE_VM_NAME}" "${LIMA_TEMPLATE_PATH}" -y
        limactl start --mount-none "${LIMA_BASE_VM_NAME}" -y
        limactl stop "${LIMA_BASE_VM_NAME}" -y
    fi

    if [ "${_base_vm_status}" = "Running" ]; then
        limactl stop "${LIMA_BASE_VM_NAME}" -y
    fi

    limactl clone "${LIMA_BASE_VM_NAME}" "${_vm_name}" -y --mount-only .:w
}

devenv_start() {
    _name=${1}
    shift

    _vm_name="${LIMA_BASE_VM_NAME}-${_name}"
    _vm_status=$(devenv_status "${_vm_name}")
    case "${_vm_status}" in
        "")
            devenv_clone "${_vm_name}"
            limactl start "${_vm_name}" -y --mount-only .:w "${@}"
            ;;
        Stopped)
            limactl start "${_vm_name}" -y --mount-only .:w "${@}"
            ;;
    esac
}

devenv_run() {
    _name=${1}
    shift
    limactl shell "${LIMA_BASE_VM_NAME}-${_name}" "$@"
}

devenv_list() {
    limactl list -l ".name == \"${LIMA_BASE_VM_NAME}-*\"" "${@}"
}

print_help() {
    cat <<EOF
usage: $0 ACTION [-n | --name NAME] [PARAMS]

Actions:
  list            List all devenv VMs
  delete          Delete a devenv VM
  start           Start a devenv VM
  stop            Stop a devenv VM
  restart         Restart a devenv VM
  run, r          Run a command in VM
  help, h         Display this help

When no parameters are specified for "run" command, the default shell is
executed.

By default, current directory name will be used as the VM name. This can be
overridden using the --name / -n flag.
EOF
}

main() {
    _action=${1:-}
    shift

    _name=$LIMA_DEFAULT_VM_NAME
    case "${1:-}" in
        -n|--name)
            _name="${2}"
            shift
            shift
    esac

    case "${_action}" in
    delete)
        devenv_delete "${_name}"
        ;;
    list)
        devenv_list "${@}"
        ;;
    start)
        devenv_start "${_name}" "${@}"
        ;;
    stop)
        devenv_stop "${_name}"
        ;;
    restart)
        devenv_stop "${_name}"
        devenv_start "${_name}" "${@}"
        ;;
    run|r)
        devenv_start "${_name}"
        devenv_run "${_name}" "${@}"
        ;;
    help|h|-h|--help|"")
        print_help
        ;;
    esac
}

main "${@}"
