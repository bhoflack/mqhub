# mqhub

## Building
To build mqhub run

    make rel

## Test release
Create a test release with

    make devrel

You will have 3 servers for development testing.  Start the three servers with:

    for d in dev/dev*; do $d/bin/mqhub start; done
    for d in dev/dev{2,3}; do $d/bin/mqhub-admin join mqhub1@127.0.0.1; done

Now you have 3 servers working together.  To join the session start:

    dev/dev1/bin/mqhub attach
