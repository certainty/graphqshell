# GraphQL Client TUI (in the works)

[![Build Status](https://travis-ci.com/certainty/graphqshell.svg?branch=main)](https://travis-ci.com/certainty/graphqshell)

## Status
This is in early hacking state, where I sketch out what it could be. 
It's by no means stable and will lack features, crash or have other undesirable effects.

![Screenshot 2021-10-09 at 18 12 15](https://user-images.githubusercontent.com/338957/136666500-8c5e872f-5081-4937-9c79-946000d5e2ef.png)


## Trying it out

The easiest way to play around with the client is to use the dockerized version like so:

``` shellsession
make demo
```

This will spin up the docker containers that are required and put you in a shell 
where you can execute the client. If you don't provide any arguments it will default to connect
to the test server that is provided.
