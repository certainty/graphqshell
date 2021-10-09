# graphqshell

## Usage

You first have to create a configuration file that you can use to connect to a GraphQL API.
Have a look at the [config](config/config.yaml) for an example. 
Once you have the configuration in place you can start the client with

```
stack exec graphqshell -- --config path/to/your/config
```

This will get you into the client which will look something like this:
![Screenshot 2021-10-09 at 18 11 56](https://user-images.githubusercontent.com/338957/136666434-608af1ab-20db-426d-bf24-5d96b8a15f4d.png)

### Exiting the shell

The client aims to have a discoverable navigation, but you first need to know how to enable it.
Just hit the `<space>` key and the menu will pop-up at the bottom of the client. 
Now you can hit `q` to exit the client or `esc` to close the menu bar. 

![Screenshot 2021-10-09 at 18 12 15](https://user-images.githubusercontent.com/338957/136666428-e659772c-4d4c-4e33-a559-3c0a347c5bb1.png)

For vim users, this should be familiar if you have used the which-key extension, which
was the inspiration for the menu system.


## Local development

As this is a GraphQL client you need a GraphQL server to talk to.
There are two builtin ways that you can chose from. Pick whatever suits you best.

## Use the provided demo_server

The demo_server is bundled with this repository and can be build when the appropriate flag
is provided during build. The following will build the demo server and make it available on http://localhost:5050/graphql.

```
make demo_server
```

Then you can connect to the local server using the following:

```
make local_client
```

## Use the public API https://api.react-finland.fi/graphql 

This connects to the open GraphQL-API if the react finland group. 
This is a good place to start and play around with the client.

```
make client
```
