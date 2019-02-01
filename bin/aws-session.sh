#!/usr/bin/env bash

set -euo pipefail

AWS_PROFILE=${AWS_PROFILE:-}
AWS_SESSION_DURATION=${AWS_SESSION_DURATION:-3600}
MFA_TOKEN=${MFA_TOKEN:-}
PRINT_HELP=""

parse_args() {
    local key
    while [[ "$#" -gt 0 ]]; do
        key=$1
        case "$key" in
            -p|--profile)
                AWS_PROFILE=$2
                shift
                shift
                ;;
            -d|--duration)
                AWS_SESSION_DURATION=$2
                shift
                shift
                ;;
            -t|--token)
                MFA_TOKEN=$2
                shift
                shift
                ;;
            -h|--help)
                PRINT_HELP=yes
                shift
                ;;
        esac
    done
}

run_aws() {
    if [ -n "${AWS_PROFILE:-}" ]; then
        aws --profile "$AWS_PROFILE" "$@"
    else
        aws "$@"
    fi
}

get_mfa_id() {
    run_aws iam list-mfa-devices \
        --query 'MFADevices[0].SerialNumber' --output text
}

get_session() {
    local mfa_id
    if [ -n "$MFA_TOKEN" ]; then
        mfa_id=$(get_mfa_id)
        run_aws sts get-session-token \
            --duration-seconds "$AWS_SESSION_DURATION" \
            --serial-number "$mfa_id" --token-code "$MFA_TOKEN"
    else
        run_aws sts get-session-token \
            --duration-seconds "$AWS_SESSION_DURATION"
    fi
}

print_help() {
    local cmd=$(basename "$0")
    cat <<EOF
usage: $cmd [options]

options:
    -p, --profile       AWS CLI profile to use for creating a session
    -t, --token         MFA token
    -d, --duration      Session duration in seconds
    -h, --help          Print this help
EOF
}

main() {
    local session_data
    parse_args "$@"

    if [ -n "${PRINT_HELP:-}" ]; then
        print_help
        exit 0
    fi

    session_data=$(get_session)
    echo "export AWS_ACCESS_KEY_ID=$(echo "$session_data" | jq -r .Credentials.AccessKeyId)"
    echo "export AWS_SECRET_ACCESS_KEY=$(echo "$session_data" | jq -r .Credentials.SecretAccessKey)"
    echo "export AWS_SESSION_TOKEN=$(echo "$session_data" | jq -r .Credentials.SessionToken)"
}

main "$@"

