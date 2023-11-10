# Shipnix recommended settings
# IMPORTANT: These settings are here for ship-nix to function properly on your server
# Modify with care

{ config, pkgs, modulesPath, lib, ... }:
{
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes ca-derivations
    '';
    settings = {
      trusted-users = [ "root" "ship" "nix-ssh" ];
    };
  };

  programs.git.enable = true;
  programs.git.config = {
    advice.detachedHead = false;
  };

  services.openssh = {
    enable = true;
    # ship-nix uses SSH keys to gain access to the server
    # Manage permitted public keys in the `authorized_keys` file
    settings.PasswordAuthentication = false;
  };


  users.users.ship = {
    isNormalUser = true;
    extraGroups = [ "wheel" "nginx" ];
    # If you don't want public keys to live in the repo, you can remove the line below
    # ~/.ssh will be used instead and will not be checked into version control. 
    # Note that this requires you to manage SSH keys manually via SSH,
    # and your will need to manage authorized keys for root and ship user separately
    openssh.authorizedKeys.keyFiles = [ ./authorized_keys ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC/r10l03MJjMEC8mknnt7uJWqhPwmVvrJyVYT3w42hhV8SUrY9XaSzkcsPdm9JNGb0uoziarX/cWmm+mqJjzm6Hj2gNyTzZsf/TW7y0OpKVPhiyWMKBVkkUMwBinQyPDwaWMuH2o3NrfdUGHDhxgFGx19hllv/yTPefOlBkDaU2K13Cmdmtmxg1OSKKWP+54Hyh++Aba4rIZmn3Ek8kFqSCVn2KRKTHx70MQavx0zXX2mzf8wrcu79n4oJATAMtm4Qj//LeZMKkUbhCJnSeHNIgotYKH6AHb0GVgJa04pqynEfaB/bOw+5BhP8e5sRziVdNu07dU5BrSLAMVlVKRkfhKswxh6I/IhKiNfGIRVQEqoN04zEhCQmaz6I9PUz+S1NCmC3l/Vm4qUJer2UBxHie0lkfeLkcQ3qPN59HdWIsCogLISsGfw7VKTNMbM4HmyeNG+TfpHjh5em/C2fQRRW5d8k+I91xHUcIqKc5EYEFoIfot+Z5wNPucVGTUrC5TM= ship@tite-ship
"
    ];
  };

  # Can be removed if you want authorized keys to only live on server, not in repository
  # Se note above for users.users.ship.openssh.authorizedKeys.keyFiles
  users.users.root.openssh.authorizedKeys.keyFiles = [ ./authorized_keys ];
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC/r10l03MJjMEC8mknnt7uJWqhPwmVvrJyVYT3w42hhV8SUrY9XaSzkcsPdm9JNGb0uoziarX/cWmm+mqJjzm6Hj2gNyTzZsf/TW7y0OpKVPhiyWMKBVkkUMwBinQyPDwaWMuH2o3NrfdUGHDhxgFGx19hllv/yTPefOlBkDaU2K13Cmdmtmxg1OSKKWP+54Hyh++Aba4rIZmn3Ek8kFqSCVn2KRKTHx70MQavx0zXX2mzf8wrcu79n4oJATAMtm4Qj//LeZMKkUbhCJnSeHNIgotYKH6AHb0GVgJa04pqynEfaB/bOw+5BhP8e5sRziVdNu07dU5BrSLAMVlVKRkfhKswxh6I/IhKiNfGIRVQEqoN04zEhCQmaz6I9PUz+S1NCmC3l/Vm4qUJer2UBxHie0lkfeLkcQ3qPN59HdWIsCogLISsGfw7VKTNMbM4HmyeNG+TfpHjh5em/C2fQRRW5d8k+I91xHUcIqKc5EYEFoIfot+Z5wNPucVGTUrC5TM= ship@tite-ship
"
  ];

  security.sudo.extraRules = [
    {
      users = [ "ship" ];
      commands = [
        {
          command = "ALL";
          options = [ "NOPASSWD" "SETENV" ];
        }
      ];
    }
  ];
}
