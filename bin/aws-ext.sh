#!/usr/bin/env bash
set -euo pipefail

COMMAND_NAME_IN_HELP=$(basename "$0")

aws_ecr_login() {
    local account ecr_hostname
    local region=${AWS_REGION:-}

    while [[ ${#} -gt 0 ]]; do
        case "${1}" in
            -h|--help)
                print_aws_ecr_login_help
                return 0
                ;;
            -a|--account)
                account=${2}
                shift
                shift
                ;;
            -r|--region)
                region=${2}
                shift
                shift
                ;;
            *)
                echo "unknown argument: ${1}" >&2
                return 1
        esac
    done

    if [ -z "${account:-}" ]; then
        account=$(aws sts get-caller-identity --query Account --output text)
    fi
    if [ -z "${region:-}" ]; then
        region=us-east-1
    fi

    ecr_hostname="${account}.dkr.ecr.${region}.amazonaws.com"
    echo "Docker login to AWS account ${account} region ${region}" >&2
    aws ecr get-login-password --region "${region}" \
        | docker login --username AWS --password-stdin "${ecr_hostname}"
}

aws_ecs_port_forward() {
    local pf_target pf_parameters
    local pf_host=localhost pf_port pf_local_port
    local ecs_cluster=${AWS_ECS_CLUSTER:-}
    local ecs_task_id ecs_container_id
    local region=${AWS_REGION:-}

    while [[ ${#} -gt 0 ]]; do
        case "${1}" in
            "--help"|"-h")
                print_aws_ecs_port_forward_help
                return 0
                ;;
            --cluster)
                ecs_cluster=${2}
                shift
                shift
                ;;
            "--task"|"-t")
                ecs_task_id=${2}
                shift
                shift
                ;;
            "--container"|"-c")
                ecs_container_id=${2}
                shift
                shift
                ;;
            "--host")
                pf_host=${2}
                shift
                shift
                ;;
            "--port"|"-p")
                pf_port=${2}
                shift
                shift
                ;;
            --local-port)
                pf_local_port=${2}
                shift
                shift
                ;;
            "--region"|"-r")
                region=${2}
                shift
                shift
                ;;
            *)
                echo "unknown argument: ${1}" >&2
                return 1
        esac
    done

    if [ -z "${pf_local_port:-}" ]; then
        pf_local_port=${pf_port}
    fi
    pf_target="ecs:${ecs_cluster}_${ecs_task_id}_${ecs_container_id}"
    pf_parameters=$(\
        printf '{"host":["%s"],"portNumber":["%d"],"localPortNumber":["%d"]}' \
        "${pf_host}" "${pf_port}" "${pf_local_port}" \
    )

    aws ssm start-session \
        --target "${pf_target}" \
        --document-name AWS-StartPortForwardingSessionToRemoteHost \
        --parameters "${pf_parameters}" \
        --region "${region}"
}

aws_ecs_execute_command() {
    local ecs_cluster=${AWS_ECS_CLUSTER:-}
    local ecs_container ecs_task
    local ecs_command='/bin/bash'
    local region=${AWS_REGION:-}

    while [[ ${#} -gt 0 ]]; do
        case "${1}" in
            "--help"|"-h")
                print_aws_ecs_execute_command_help
                return 0
                ;;
            --cluster)
                ecs_cluster=${2}
                shift
                shift
                ;;
            "--task"|"-t")
                ecs_task=${2}
                shift
                shift
                ;;
            "--container"|"-c")
                ecs_container=${2}
                shift
                shift
                ;;
            --command)
                ecs_command=${2}
                shift
                shift
                ;;
            "--region"|"-r")
                region=${2}
                shift
                shift
                ;;
            *)
                echo "unknown argument: ${1}" >&2
                return 1
        esac
    done

    aws ecs execute-command \
        --interactive \
        --cluster "${ecs_cluster}" \
        --container "${ecs_container}" \
        --task "${ecs_task}" \
        --command "${ecs_command}" \
        --region "${region}"
}

print_aws_ecr_login_help() {
    cat << EOF
usage: $COMMAND_NAME_IN_HELP ecr-login [OPTIONS]

Log in to ECR with Docker to be able to push/pull Docker images from ECR.

Options:
  --account, -a         AWS account ID for ECR. By default, the current AWS profile
                        account ID is used.
  --region, -r          AWS region for ECR. By default, the profile default region
                        or us-east-1 is used.
  --help, -h            Print this help message

EOF
}

print_aws_ecs_port_forward_help() {
    cat << EOF
usage: $COMMAND_NAME_IN_HELP ecs-port-forward [OPTIONS]

Start a port-forward session on a container on ECS.

Options:
  --cluster             ECR cluster name where the container is located.
                        By default, the env var AWS_ECS_CLUSTER is used.
  --task, -t            ID of the ECS task to target.
  --container, -c       ID of the container to target.
  --host                Host name where to open the port-forward in the
                        container. By default, localhost is used.
  --port, -p            Port where to open the port-forward in the
                        container.
  --local-port          Port on the local machine where to open the
                        port-forward. By default, the same value is used
                        in the port parameter.
  --region, -r          AWS region where the container is located in.
                        By default, the profile default region or
                        us-east-1 is used.
  --help, -h            Print this help message

EOF
}

print_aws_ecs_execute_command_help() {
    cat << EOF
usage: $COMMAND_NAME_IN_HELP ecs-exec [OPTIONS]

Execute a command on a container on ECS.

Options:
  --cluster             ECR cluster name where the container is located.
                        By default, the env var AWS_ECS_CLUSTER is used.
  --task, -t            ID of the ECS task to target.
  --container, -c       Name of the container to target.
  --command             Command to execute on the container. By default,
                        /bin/bash is executed on the container.
  --region, -r          AWS region where the container is located in.
                        By default, the profile default region or
                        us-east-1 is used.
  --help, -h            Print this help message

EOF
}

print_main_help() {
    cat << EOF
usage: $COMMAND_NAME_IN_HELP COMMAND [OPTIONS]

Commands:
  ecr-login                    Login to ECR repo with Docker
  ecs-port-forward, ecs-pf     Start a port-forward session to a
                               container on ECS
  ecs-exec                     Run a command on a container on ECS
  help, h                      Print this help message
 

EOF
}

main() {
    local cmd=${1:-}
    if [ -z "${cmd:-}" ]; then
        echo "No command specified" >&2
        print_main_help >&2
        return 1
    fi

    shift
    case "${cmd:-}" in
    "ecr-login")
        aws_ecr_login "${@}"
        ;;
    "ecs-exec")
        aws_ecs_execute_command "${@}"
        ;;
    "ecs-port-forward"|"ecs-pf")
        aws_ecs_port_forward "${@}"
        ;;
    "help"|"h"|"--help"|"-h")
        print_main_help
        ;;
    *)
        echo "Unknown command: ${cmd:-}" >&2
        print_main_help >&2
        return 1
        ;;
    esac
}

main "${@}"

