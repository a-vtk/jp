# Job Planner

A job processor service for the coding challenge. The service receives sets of linked tasks and orders them.

## Prerequisites

To build application locally using your machine:
* Erlang/OTP 26

To build the application inside a container:
* Docker

## Build

Use the provided rebar3 to compile and run tests.

    ./rebar3 compile
    ./rebar3 ct

## Docker

It's possible to launch the service inside a docker container. To do so follow the instructions below.

    docker build --tag jp .
    docker run --name jp -d -p 8080:8080 jp

## API

The service provides only one method to build an execution plan for the given tasks and build a shell script from the given tasks.

### Build an execution plan (Json)

#### Request

`POST /api/v1/plans`

    curl -i -H'Accept: application/json' -H'Content-Type: application/json' 'http://localhost:8080/api/v1/plans' -d '{
        "tasks": [
            {
                "name": "task-4",
                "command": "rm /tmp/file1",
                "requires": [
                    "task-2",
                    "task-3"
                ]
            },
            {
                "name": "task-1",
                "command": "touch /tmp/file1"
            },
            {
                "name": "task-2",
                "command": "cat /tmp/file1",
                "requires": [
                    "task-3"
                ]
            },
            {
                "name": "task-3",
                "command": "echo '\''Hello World!'\'' > /tmp/file1",
                "requires": [
                    "task-1"
                ]
            }
        ]
    }'

#### Response

    HTTP/1.1 200 OK
    content-length: 232
    content-type: application/json
    date: Sun, 04 Aug 2024 19:41:06 GMT
    server: Cowboy
    vary: accept

    {"status":"ok","tasks":[{"command":"touch \/tmp\/file1","name":"task-1"},{"command":"echo 'Hello World!' > \/tmp\/file1","name":"task-3"},{"command":"cat \/tmp\/file1","name":"task-2"},{"command":"rm \/tmp\/file1","name":"task-4"}]}


### Build a shell script from given tasks

#### Request

`POST /api/v1/plans`

    curl -i -H'Accept: application/x-sh' -H'Content-Type: application/json' 'http://localhost:8080/api/v1/plans' -d '{
        "tasks": [
            {
                "name": "task-4",
                "command": "rm /tmp/file1",
                "requires": [
                    "task-2",
                    "task-3"
                ]
            },
            {
                "name": "task-1",
                "command": "touch /tmp/file1"
            },
            {
                "name": "task-2",
                "command": "cat /tmp/file1",
                "requires": [
                    "task-3"
                ]
            },
            {
                "name": "task-3",
                "command": "echo '\''Hello World!'\'' > /tmp/file1",
                "requires": [
                    "task-1"
                ]
            }
        ]
    }'

#### Response

    HTTP/1.1 200 OK
    content-length: 98
    content-type: application/x-sh
    date: Sun, 04 Aug 2024 19:44:40 GMT
    server: Cowboy
    vary: accept

    #!/usr/bin/env bash
    touch /tmp/file1
    echo 'Hello World!' > /tmp/file1
    cat /tmp/file1
    rm /tmp/file1

## Errors

Format of error messages for `application/json`:

    {"status":"error", "message":"..."}

And for `application/x-sh`:

    "..."

### Error Messages

* `"Body limit exceeded"` -- the body is greater than 8MB
* `"Invalid JSON"` -- there is invalid JSON in the body
* `"Duplicate tasks: <task-name>"` -- there are two or more tasks with the same name 
* `"Cycle detected: <task-name>-><task-name>->..."` -- there are two or more tasks that depend on each other directly or indirectly
* `"Dependency not found: <task-name>"` -- there is no definition for a task with the given name
