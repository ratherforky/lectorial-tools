# Lectorial Tools Web App

Deployed at https://lectorial.uk/

## Dependencies

- `nix`
- `direnv` (with shell hook)
- VSCode extensions:
  - Haskell/HLS
  - direnv

## Compiling and running the app

1. [Make sure Nix and IHP are installed](https://ihp.digitallyinduced.com/Guide/installation.html)
1. If you've never created an IHP app on your machine before, run `ihp-new foo`
  - This will create a new IHP project, but the only point of this command is to set up cachix (it's not impossible to compile without cachix, but 24 GB of RAM isn't enough to do it, so you really should make sure it's set up)
  - Delete the newly created `foo` directory afterwards
1. In the root directory of the project, run `devenv up`

## VSCode setup

- For HLS:
  - Install `Haskell` extension
    - Manage HLS via PATH
  - Install `direnv` extension (this will ensure the HLS bundled with IHP works correctly)

## Other useful notes

- The `Web` directory is where most of the core logic goes
- Uses an MVC pattern
  - Model: Postgres database
  - View: `Web.View`
  - Controller: `Web.Controller`
- Connect to DB: `make psql`
  - `\q` to quit

## Deploying

With SSH config:

```ssh
Host lectorial
    HostName INSERT-URL.compute.amazonaws.com
    User root
        IdentityFile ~/.ssh/SSH-KEY.pem
```

- In project root: `deploy-to-nixos lectorial`

### Restarting AWS instance

- Stopping an instance and restarting it will lead to a new 'Public DNS' ==> In Cloudflare configuration Update DNS->Records->Edit main CNAME 'Content' with new 'Public DNS' from AWS instance details
