# mqhub

## Building
To build mqhub run

    make rel

## Test release
### Erlang API
Create a test release with

    make devrel

You will have 3 servers for development testing.  Start the three servers with:

    for d in dev/dev*; do $d/bin/mqhub start; done
    for d in dev/dev{2,3}; do $d/bin/mqhub-admin join mqhub1@127.0.0.1; done

Now you have 3 servers working together.  To join the session start:

    dev/dev1/bin/mqhub attach

#### Queues
And you can start pushing and pulling messages:

    mqhub:push("brh/queue/a", <<"hello world">>).
    mqhub:pull("brh/queue/a").

#### Topics
Topics allow you to publish a message to multiple queues at the same time.

Creating a topic can be done by:

    mqhub:create_topic("brh/topic/test").

You can add subscribers to a topic by subscribing them:

    mqhub:subscribe("brh/topic/test", "brh/queue/a").
    mqhub:subscribe("brh/topic/test", "brh/queue/b").

Now both queues will get messages sent to *brh/topic/test*.  To publish a message to them:

    mqhub:publish("brh/topic/test", <<"hello">>).
    mqhub:publish("brh/topic/test", <<"world">>).

This will publish two messages to both queues.  You can verify by pulling from both queues:

    mqhub:pull("brh/queue/a").

### Http REST api
The app http contains the REST api.  This is automatically started on all hosts when you start mqhub.
#### Queues
To push a message execute:

    curl -X PUT -H 'Content-Type: application/json' localhost:8000/brh/queue/a -d "hello world"

To pull the messages execute:

    curl localhost:8000/brh/queue/a
#### Topic
To add a listener to a topic execute:

    curl -X PUT -H 'Content-Type: application/json' localhost:8000/brh/topic/a/listener -d "/brh/queue/a"

You can verify the listeners for a topic by executing:

    curl localhost:8000/brh/topic/a/listener

And you can delete a topic by executing:

    curl -X DELETE -H 'Content-Type: application/json' localhost:8000/brh/topic/a/listener/brh/queue/a

Publishing a message to the topic can be done by:

    curl -X PUT -H 'Content-Type: application/json' localhost:8000/brh/topic/a -d 'hello world'

You can get the messages from the queue now:

    curl localhost:8000/brh/queue/a






