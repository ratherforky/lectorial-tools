# Lectorial Tools Webapp

Deployed at https://lectorial.ihpapp.com/

## Compiling and running the app

1. [Make sure Nix and IHP are installed](https://ihp.digitallyinduced.com/Guide/installation.html)
1. If you've never created an IHP app on your machine before, run `ihp-new foo`
  - This will create a new IHP project, but the only point of this command is to set up cachix (it's not impossible to compile without cachix, but 24 GB of RAM isn't enough to do it, so you really should make sure it's set up)
  - Delete the newly created `foo` directory afterwards
1. In the root directory of the project, run `./start`

## Other useful notes

- Connect to DB: `make psql`
  - `\q` to quit
