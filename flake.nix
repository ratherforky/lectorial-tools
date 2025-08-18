{
    inputs = {
        # Here you can adjust the IHP version of your project
        # You can find new releases at https://github.com/digitallyinduced/ihp/releases
        ihp.url = "github:digitallyinduced/ihp/v1.3";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
        agenix.url = "github:ryantm/agenix";
        agenix.inputs.nixpkgs.follows = "ihp/nixpkgs";
    };

    outputs = inputs@{ ihp, flake-parts, systems, nixpkgs, self, agenix, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {

            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, ... }: {
                ihp = {
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [
                        # Native dependencies, e.g. imagemagick
                    ];
                    haskellPackages = p: with p; [
                        # Haskell dependencies go here
                        p.ihp
                        cabal-install
                        base
                        wai
                        text
                        hlint

                        # Extra deps
                        random
                    ];
                };
            };

            # flake.nixosConfigurations."lectorial"  = nixpkgs.lib.nixosSystem {
            #   system = "x86_64-linux";
            #   specialArgs = inputs // {
            #     environment = "production";
            #     ihp-migrate = self.packages.x86_64-linux.migrate;
            #     ihpApp = self.packages.x86_64-linux.default;
            #   };
            #   modules = [
            #     ./nixos/configuration.nix
            #   ];
            # };
            flake.nixosConfigurations."lectorial" = nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              specialArgs = inputs;
              modules = [
                  "${nixpkgs}/nixos/modules/virtualisation/amazon-image.nix"
                  ihp.nixosModules.appWithPostgres
                  ({ ... }: {
                      security.acme.defaults.email = "jess.foster@bristol.ac.uk";

                      networking.firewall = {
                          enable = true;
                          allowedTCPPorts = [ 22 80 443 ];
                      };

                      # Accept the terms of service of the Let's encrypt provider.
                      security.acme.acceptTerms = true;

                      # security.acme.defaults.server = "https://acme-staging-v02.api.letsencrypt.org/directory";
                      # security.acme.preliminarySelfsigned = true;

                      # /var/lib/acme/.challenges must be writable by the ACME user
                      # and readable by the Nginx user. The easiest way to achieve
                      # this is to add the Nginx user to the ACME group.
                      # users.users.nginx.extraGroups = [ "acme" ];

                      services.nginx = {
                        # enable = true;
                        virtualHosts."lectorial.uk" =  {
                          forceSSL = true;
                          enableACME = true;
                          # acmeRoot = null;
                          serverAliases = [ "www.lectorial.uk" ];
                          locations."/" = {
                            root = "/var/www";
                          };
                            # Uncomment to have http auth with username `foo` and password `bar`.
                            # basicAuth = { foo = "bar"; };
                        };
                      };


                      services.ihp = {
                          domain = "lectorial.uk";
                          migrations = ./Application/Migration;
                          schema = ./Application/Schema.sql;
                          fixtures = ./Application/Fixtures.sql;
                          sessionSecret = "xxx";
                      };

                      # troubleshooting failed worker service
                      # from Joachim Breitner post on the IHP slack: https://ihpframework.slack.com/archives/C01DQE0F4F8/p1719935767784549
                      systemd.services.worker.enable = nixpkgs.lib.mkForce false;

                      # Add swap to avoid running out of memory during builds
                      # Useful if your server have less than 4GB memory
                      swapDevices = [ { device = "/swapfile"; size = 8192; } ];

                      # # As we use a pre-built AMI on AWS,
                      # # it is essential to enable automatic updates.
                      # # @see https://nixos.wiki/wiki/Automatic_system_upgrades
                      # system.autoUpgrade.enable = true;

                      # This should reflect the nixos version from the NixOS AMI initally installed
                      # After the initial install, it should not be changed. Otherwise e.g. the postgres
                      # server might need a manual data migration if NixOS changes the default postgres version
                      system.stateVersion = "23.05";
                  })
                  # {
                  #   services.userborn.enable = false;
                  # }
                  agenix.nixosModules.default
                  {
                    age.secrets.admin-password.file = ./secrets/admin-password.age;
                  }
              ];
          };


        };
}
